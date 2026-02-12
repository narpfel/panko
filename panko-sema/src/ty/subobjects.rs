use std::fmt;
use std::num::NonZero;

use panko_parser::nonempty;
use panko_parser::sexpr_builder::AsSExpr;
use panko_parser::sexpr_builder::SExpr;

use crate::ty::ArrayType;
use crate::ty::Complete;
use crate::ty::Struct;
use crate::typecheck::ArrayLength;
use crate::typecheck::Member;
use crate::typecheck::QualifiedType;
use crate::typecheck::Type;
use crate::typecheck::Typeck;

#[derive(Debug, Clone, Copy)]
pub(crate) struct Subobject<'a> {
    pub(crate) ty: QualifiedType<'a>,
    pub(crate) offset: u64,
}

#[derive(Clone)]
pub(crate) enum SubobjectIterator<'a> {
    Scalar {
        ty: Type<'a>,
        is_exhausted: bool,
        offset: u64,
    },
    Array {
        ty: ArrayType<'a, Typeck>,
        index: u64,
        offset: u64,
    },
    Struct {
        ty: Complete<'a, Typeck>,
        index: usize,
        offset: u64,
    },
}

impl AsSExpr for SubobjectIterator<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            Self::Scalar { ty, is_exhausted, offset } => SExpr::new(format!(
                "scalar-subobject @{offset} is_exhausted={is_exhausted}"
            ))
            .inherit(ty),
            Self::Array { ty, index, offset } =>
                SExpr::new(format!("array-subobject @{offset} index={index}")).inherit(ty),
            Self::Struct { ty, index, offset } =>
                SExpr::new(format!("struct-subobject @{offset} index={index}"))
                    .inline_string(Type::Struct(Struct::Complete(*ty)).as_sexpr().to_string()),
        }
    }
}

impl fmt::Debug for SubobjectIterator<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_sexpr())
    }
}

impl<'a> SubobjectIterator<'a> {
    fn new(ty: &Type<'a>, offset: u64) -> Result<Self, ()> {
        match *ty {
            Type::Array(ty) => Ok(Self::Array { ty, index: 0, offset }),
            Type::Struct(Struct::Complete(ty)) => Ok(Self::Struct { ty, index: 0, offset }),
            ty if ty.is_scalar() => Ok(Self::Scalar { ty, is_exhausted: false, offset }),
            _ => Err(()),
        }
    }

    fn next(&mut self) -> Option<Subobject<'a>> {
        let result = self.current();
        match self {
            Self::Scalar { ty: _, is_exhausted, offset: _ } => *is_exhausted = true,
            Self::Array { ty: _, index, offset: _ } => *index = index.strict_add(1),
            Self::Struct { ty: _, index, offset: _ } => *index = index.strict_add(1),
        }
        result
    }

    fn parent(&self) -> Option<Subobject<'a>> {
        match self {
            Self::Scalar { .. } => None,
            Self::Array { ty, index: _, offset } => Some(Subobject {
                ty: Type::Array(*ty).unqualified(),
                offset: *offset,
            }),
            Self::Struct { ty, index: _, offset } => Some(Subobject {
                ty: Type::Struct(Struct::Complete(*ty)).unqualified(),
                offset: *offset,
            }),
        }
    }

    pub(crate) fn current(&self) -> Option<Subobject<'a>> {
        if self.is_empty() {
            None
        }
        else {
            match self {
                // TODO: is it correct to return the unqualified type here or should the qualifiers
                // be retained?
                Self::Scalar { ty, is_exhausted: _, offset } =>
                    Some(Subobject { ty: ty.unqualified(), offset: *offset }),
                Self::Array { ty, index, offset } => Some(Subobject {
                    ty: ty.ty.make_unqualified(),
                    offset: offset.strict_add(index.strict_mul(ty.ty.ty.size())),
                }),
                Self::Struct { ty, index, offset } => {
                    let Member { name: _, ty, offset: member_offset } = ty.members[*index];
                    Some(Subobject {
                        ty,
                        offset: offset.strict_add(member_offset),
                    })
                }
            }
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            Self::Scalar { ty: _, is_exhausted, offset: _ } => *is_exhausted,
            Self::Array { ty, index, offset: _ } => match ty.length {
                ArrayLength::Constant(len) => *index >= len,
                ArrayLength::Variable(_) =>
                    todo!("VLAs cannot be initialised by braced initialisation"),
                ArrayLength::Unknown => false,
            },
            Self::Struct { ty, index, offset: _ } => *index >= ty.members.len(),
        }
    }

    pub(crate) fn kind(&self) -> &'static str {
        match self {
            Self::Scalar { .. } => "scalar",
            Self::Array { .. } => "array",
            Self::Struct { .. } => "struct",
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Explicit {
    No,
    Yes,
    Next,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum AllowExplicit {
    No,
    Yes(NonZero<usize>),
}

#[derive(Debug, Clone)]
pub(crate) struct Subobjects<'a> {
    stack: nonempty::Vec<(SubobjectIterator<'a>, Explicit)>,
}

impl<'a> Subobjects<'a> {
    pub(crate) fn new(ty: QualifiedType<'a>) -> Result<Self, ()> {
        Ok(Self {
            stack: nonempty::Vec::new((SubobjectIterator::new(&ty.ty, 0)?, Explicit::Yes)),
        })
    }

    fn current(&self) -> &SubobjectIterator<'a> {
        &self.stack.last().0
    }

    fn current_mut(&mut self) -> &mut SubobjectIterator<'a> {
        &mut self.stack.last_mut().0
    }

    pub(crate) fn parent(&self) -> Option<Subobject<'a>> {
        self.current().parent()
    }

    pub(crate) fn goto_index(&mut self, target_index: u64) -> Result<(), SubobjectIterator<'a>> {
        match self.current_mut() {
            SubobjectIterator::Scalar { .. } | SubobjectIterator::Struct { .. } =>
                Err(self.current().clone()),
            SubobjectIterator::Array { ty: _, index, offset: _ } => {
                *index = target_index;
                Ok(())
            }
        }
    }

    fn leave_empty_subobjects(&mut self) {
        while self.current().is_empty() {
            let left = self.try_leave_subobject(AllowExplicit::No);
            if !left {
                break;
            }
        }
    }

    fn push(&mut self, iterator: SubobjectIterator<'a>) {
        self.stack.push((iterator, Explicit::No))
    }

    pub(crate) fn next(
        &mut self,
        initialiser_ty: &Type<'a>,
    ) -> Result<Subobject<'a>, SubobjectIterator<'a>> {
        loop {
            self.leave_empty_subobjects();

            let subobject = self
                .current_mut()
                .next()
                .ok_or_else(|| self.current().clone())?;
            match subobject.ty.ty {
                Type::Array(ty) =>
                    self.push(SubobjectIterator::Array { ty, index: 0, offset: subobject.offset }),
                struct_ty @ Type::Struct(Struct::Complete(ty)) if initialiser_ty != &struct_ty =>
                    self.push(SubobjectIterator::Struct { ty, index: 0, offset: subobject.offset }),
                _ => {
                    if let (_, explicit @ Explicit::Next) = self.stack.last_mut() {
                        *explicit = Explicit::No;
                    }
                    return Ok(subobject);
                }
            }
        }
    }

    fn enter_subobject_impl(
        &mut self,
        explicitness: Explicit,
    ) -> (NonZero<usize>, Result<(), SubobjectIterator<'a>>) {
        self.leave_empty_subobjects();
        let depth = self.depth();

        let (iterator, result) = match self.current_mut().next() {
            Some(Subobject { ty, offset }) => (
                SubobjectIterator::new(&ty.ty, offset).expect("todo"),
                Ok(()),
            ),
            // Example for this case:
            //     int x = {1, {}};
            None => (self.current().clone(), Err(self.current().clone())),
        };
        self.stack.push((iterator, explicitness));
        (depth, result)
    }

    pub(crate) fn enter_subobject_implicit(
        &mut self,
    ) -> (NonZero<usize>, Result<(), SubobjectIterator<'a>>) {
        self.enter_subobject_impl(Explicit::Next)
    }

    pub(crate) fn enter_subobject(
        &mut self,
    ) -> (NonZero<usize>, Result<(), SubobjectIterator<'a>>) {
        self.enter_subobject_impl(Explicit::Yes)
    }

    #[must_use]
    pub(crate) fn try_leave_subobject(&mut self, allow_explicit: AllowExplicit) -> bool {
        match allow_explicit {
            AllowExplicit::No => self
                .stack
                .pop_if(|(_, explicit)| matches!(explicit, Explicit::No))
                .is_some(),
            AllowExplicit::Yes(depth) => {
                self.stack.truncate(depth);
                depth <= self.depth()
            }
        }
    }

    pub(crate) fn depth(&self) -> NonZero<usize> {
        NonZero::new(self.stack.len()).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use std::iter::from_fn;

    use itertools::Itertools as _;
    use panko_lex::Loc;

    use super::*;
    use crate::fake_trait_impls::HashEqIgnored;
    use crate::ty::ArrayType;
    use crate::ty::Type;
    use crate::typecheck::ArrayLength;

    #[test]
    fn test_scalar_subobjects() {
        let ty = Type::size_t().unqualified();

        let mut subobjects = Subobjects::new(ty).unwrap();
        let subobject_offsets = from_fn(|| subobjects.next(&ty.ty).ok())
            .map(|subobject| subobject.offset)
            .collect_vec();
        assert_eq!(subobject_offsets, vec![0]);
    }

    #[test]
    fn test_array_subobjects() {
        let size_t = Type::size_t().unqualified();
        let ty = Type::Array(ArrayType {
            ty: &size_t,
            length: ArrayLength::Constant(3),
            loc: HashEqIgnored(Loc::synthesised()),
        })
        .unqualified();

        let mut subobjects = Subobjects::new(ty).unwrap();
        let subobject_offsets = from_fn(|| subobjects.next(&size_t.ty).ok())
            .map(|subobject| subobject.offset)
            .collect_vec();
        assert_eq!(subobject_offsets, vec![0, 8, 16]);
    }

    #[test]
    fn test_2d_array_subobjects() {
        let size_t = Type::size_t().unqualified();
        let array_ty = Type::Array(ArrayType {
            ty: &size_t,
            length: ArrayLength::Constant(4),
            loc: HashEqIgnored(Loc::synthesised()),
        })
        .unqualified();
        let ty = Type::Array(ArrayType {
            ty: &array_ty,
            length: ArrayLength::Constant(3),
            loc: HashEqIgnored(Loc::synthesised()),
        })
        .unqualified();

        let mut subobjects = Subobjects::new(ty).unwrap();
        let subobject_offsets = from_fn(|| subobjects.next(&size_t.ty).ok())
            .map(|subobject| subobject.offset)
            .collect_vec();
        assert_eq!(
            subobject_offsets,
            (0..4 * 3)
                .map(|offset| offset * size_t.ty.size())
                .collect_vec()
        );
    }

    #[test]
    fn test_2d_array_subobjects_leave() {
        let size_t = Type::size_t().unqualified();
        let array_ty = Type::Array(ArrayType {
            ty: &size_t,
            length: ArrayLength::Constant(4),
            loc: HashEqIgnored(Loc::synthesised()),
        })
        .unqualified();
        let ty = Type::Array(ArrayType {
            ty: &array_ty,
            length: ArrayLength::Constant(3),
            loc: HashEqIgnored(Loc::synthesised()),
        })
        .unqualified();

        let mut subobjects = Subobjects::new(ty).unwrap();
        assert_eq!(subobjects.next(&size_t.ty).unwrap().offset, 0);
        assert!(subobjects.try_leave_subobject(AllowExplicit::No));
        let size = size_t.ty.size();
        assert_eq!(subobjects.next(&size_t.ty).unwrap().offset, size * 4);
        assert_eq!(subobjects.next(&size_t.ty).unwrap().offset, size * 4 + size);
        assert!(subobjects.try_leave_subobject(AllowExplicit::No));
        assert_eq!(subobjects.next(&size_t.ty).unwrap().offset, size * 8);
    }
}
