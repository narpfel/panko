use std::mem;

use crate::ty::ArrayType;
use crate::typecheck::ArrayLength;
use crate::typecheck::QualifiedType;
use crate::typecheck::Type;
use crate::typecheck::TypedExpression;

#[derive(Debug, Clone, Copy)]
pub(crate) struct Subobject<'a> {
    pub(crate) ty: QualifiedType<'a>,
    pub(crate) offset: u64,
}

#[derive(Debug, Clone)]
enum SubobjectIterator<'a> {
    Scalar {
        ty: Type<'a>,
        is_exhausted: bool,
        offset: u64,
    },
    Array {
        ty: ArrayType<'a, !, ArrayLength<&'a TypedExpression<'a>>>,
        index: u64,
        offset: u64,
    },
}

impl<'a> SubobjectIterator<'a> {
    fn next(&mut self) -> Option<Subobject<'a>> {
        let result = self.current();
        match self {
            Self::Scalar { ty: _, is_exhausted, offset: _ } => {
                *is_exhausted = true;
            }
            Self::Array { ty: _, index, offset: _ } => {
                *index += 1;
            }
        }
        result
    }

    fn current(&self) -> Option<Subobject<'a>> {
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
                    ty: ty.ty.ty.unqualified(),
                    offset: offset
                        .checked_add(index.checked_mul(ty.ty.ty.size()).unwrap())
                        .unwrap(),
                }),
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
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Explicit {
    No,
    Yes,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum AllowExplicit {
    No,
    Yes,
}

#[derive(Debug, Clone)]
pub(crate) struct Subobjects<'a> {
    stack: Vec<(SubobjectIterator<'a>, Explicit)>,
    current: SubobjectIterator<'a>,
}

impl<'a> Subobjects<'a> {
    pub(crate) fn new(ty: QualifiedType<'a>) -> Self {
        let subobject_iterator = match ty.ty {
            Type::Array(ty) => SubobjectIterator::Array { ty, index: 0, offset: 0 },
            ty if ty.is_scalar() =>
                SubobjectIterator::Scalar { ty, is_exhausted: false, offset: 0 },
            _ => unreachable!(),
        };
        Self {
            stack: vec![],
            current: subobject_iterator,
        }
    }

    pub(crate) fn next_scalar(&mut self) -> Option<Subobject<'a>> {
        loop {
            while self.current.is_empty() && !self.stack.is_empty() {
                let left = self.leave_subobject(AllowExplicit::No);
                if !left {
                    break;
                }
            }

            let subobject = self.current.next()?;
            match subobject.ty.ty {
                Type::Array(ty) => {
                    self.stack.push((
                        mem::replace(&mut self.current, SubobjectIterator::Array {
                            ty,
                            index: 0,
                            offset: subobject.offset,
                        }),
                        Explicit::No,
                    ));
                }
                _ => return Some(subobject),
            }
        }
    }

    pub(crate) fn enter_subobject(&mut self) {
        let iterator = match self.current.next() {
            Some(subobject) => match subobject.ty.ty {
                Type::Array(ty) =>
                    SubobjectIterator::Array { ty, index: 0, offset: subobject.offset },
                ty if ty.is_scalar() => SubobjectIterator::Scalar {
                    ty,
                    is_exhausted: false,
                    offset: subobject.offset,
                },
                _ => unreachable!(),
            },
            // Example for this case:
            //     int x = {1, {}};
            None => todo!("error: excess initialiser"),
        };
        self.stack
            .push((mem::replace(&mut self.current, iterator), Explicit::Yes));
    }

    pub(crate) fn leave_subobject(&mut self, allow_explicit: AllowExplicit) -> bool {
        if let Some((iterator, _)) = self.stack.pop_if(|(_, explicit)| {
            matches!(allow_explicit, AllowExplicit::Yes) || matches!(explicit, Explicit::No)
        }) {
            self.current = iterator;
            true
        }
        else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use std::iter::from_fn;

    use itertools::Itertools as _;

    use super::*;
    use crate::ty::ArrayType;
    use crate::ty::Type;
    use crate::typecheck::ArrayLength;

    #[test]
    fn test_scalar_subobjects() {
        let ty = Type::size_t().unqualified();

        let mut subobjects = Subobjects::new(ty);
        let subobject_offsets = from_fn(|| subobjects.next_scalar())
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
        })
        .unqualified();

        let mut subobjects = Subobjects::new(ty);
        let subobject_offsets = from_fn(|| subobjects.next_scalar())
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
        })
        .unqualified();
        let ty = Type::Array(ArrayType {
            ty: &array_ty,
            length: ArrayLength::Constant(3),
        })
        .unqualified();

        let mut subobjects = Subobjects::new(ty);
        let subobject_offsets = from_fn(|| subobjects.next_scalar())
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
        })
        .unqualified();
        let ty = Type::Array(ArrayType {
            ty: &array_ty,
            length: ArrayLength::Constant(3),
        })
        .unqualified();

        let mut subobjects = Subobjects::new(ty);
        assert_eq!(subobjects.next_scalar().unwrap().offset, 0);
        subobjects.leave_subobject(AllowExplicit::No);
        let size = size_t.ty.size();
        assert_eq!(subobjects.next_scalar().unwrap().offset, size * 4);
        assert_eq!(subobjects.next_scalar().unwrap().offset, size * 4 + size);
        subobjects.leave_subobject(AllowExplicit::No);
        assert_eq!(subobjects.next_scalar().unwrap().offset, size * 8);
    }
}
