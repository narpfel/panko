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
    },
    Array {
        ty: ArrayType<'a, !, ArrayLength<&'a TypedExpression<'a>>>,
        index: u64,
    },
}

impl<'a> SubobjectIterator<'a> {
    fn next(&mut self) -> Option<Subobject<'a>> {
        let result = self.current();
        match self {
            Self::Scalar { ty: _, is_exhausted } => {
                *is_exhausted = true;
            }
            Self::Array { ty: _, index } => {
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
                Self::Scalar { ty, is_exhausted: _ } =>
                    Some(Subobject { ty: ty.unqualified(), offset: 0 }),
                Self::Array { ty, index } => Some(Subobject {
                    ty: ty.ty.ty.unqualified(),
                    offset: index.checked_mul(ty.ty.ty.size()).unwrap(),
                }),
            }
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            Self::Scalar { ty: _, is_exhausted } => *is_exhausted,
            Self::Array { ty, index } => match ty.length {
                ArrayLength::Constant(len) => *index >= len,
                ArrayLength::Variable(_) =>
                    todo!("VLAs cannot be initialised by braced initialisation"),
                ArrayLength::Unknown => false,
            },
        }
    }

    fn size(&self) -> u64 {
        match self {
            Self::Array { ty, index: _ } => Type::Array(*ty).size(),
            Self::Scalar { ty, is_exhausted: _ } => ty.size(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Subobjects<'a> {
    stack: Vec<SubobjectIterator<'a>>,
    current: SubobjectIterator<'a>,
    offset: u64,
}

impl<'a> Subobjects<'a> {
    pub(crate) fn new(ty: QualifiedType<'a>) -> Self {
        let subobject_iterator = match ty.ty {
            Type::Array(ty) => SubobjectIterator::Array { ty, index: 0 },
            ty if ty.is_scalar() => SubobjectIterator::Scalar { ty, is_exhausted: false },
            _ => unreachable!(),
        };
        Self {
            stack: vec![],
            current: subobject_iterator,
            offset: 0,
        }
    }

    pub(crate) fn next_scalar(&mut self) -> Option<Subobject<'a>> {
        loop {
            while self.current.is_empty() && !self.stack.is_empty() {
                self.leave_subobject();
            }

            let subobject = self.current.next()?;
            match subobject.ty.ty {
                Type::Array(ty) => {
                    self.stack
                        .push(mem::replace(&mut self.current, SubobjectIterator::Array {
                            ty,
                            index: 0,
                        }));
                }
                _ =>
                    return Some(Subobject {
                        offset: subobject.offset + self.offset,
                        ..subobject
                    }),
            }
        }
    }

    pub(crate) fn leave_subobject(&mut self) {
        if let Some(iterator) = self.stack.pop() {
            self.offset += self.current.size();
            self.current = iterator;
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
        subobjects.leave_subobject();
        let size = size_t.ty.size();
        assert_eq!(subobjects.next_scalar().unwrap().offset, size * 4);
        assert_eq!(subobjects.next_scalar().unwrap().offset, size * 4 + size);
        subobjects.leave_subobject();
        assert_eq!(subobjects.next_scalar().unwrap().offset, size * 8);
    }
}
