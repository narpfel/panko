use std::collections::VecDeque;
use std::mem;

use crate::ty::Type;
use crate::typecheck::QualifiedType;

#[derive(Debug, Clone, Copy)]
pub struct Subobject<'a> {
    pub(crate) ty: QualifiedType<'a>,
    pub(crate) offset: u64,
}

#[derive(Debug, Clone)]
pub(crate) struct Subobjects<'a> {
    stack: Vec<VecDeque<Subobject<'a>>>,
    queue: VecDeque<Subobject<'a>>,
    current: Option<Subobject<'a>>,
}

impl<'a> Subobjects<'a> {
    pub(crate) fn new(ty: QualifiedType<'a>) -> Self {
        let subobject = Subobject { ty, offset: 0 };
        Self {
            stack: vec![[subobject].into_iter().collect()],
            current: Some(subobject),
            queue: VecDeque::new(),
        }
    }

    pub(crate) fn next_scalar(&mut self) -> Option<Subobject<'a>> {
        self.current = None;
        loop {
            if self.queue.is_empty()
                && let Some(queue) = self.stack.pop()
            {
                self.queue = queue;
            }

            let subobject = self.queue.pop_front()?;
            match subobject.ty.ty {
                ty @ Type::Array(_) => {
                    self.stack.push(mem::take(&mut self.queue));
                    self.queue.extend(ty.unqualified().direct_subobjects().scan(
                        subobject.offset,
                        |offset, &ty| {
                            let subobject = Some(Subobject { ty, offset: *offset });
                            *offset += ty.ty.size();
                            subobject
                        },
                    ));
                }
                ty => {
                    assert!(ty.is_scalar());
                    self.current = Some(subobject);
                    return self.current;
                }
            }
        }
    }

    pub(crate) fn leave_subobject(&mut self) {
        if let Some(queue) = self.stack.pop() {
            self.queue = queue;
        }
    }

    pub(crate) fn current(&self) -> Option<Subobject<'a>> {
        self.current
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
