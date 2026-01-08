use std::ops::Index;
use std::slice::SliceIndex;

#[derive(Clone)]
pub struct Vec<T>(std::vec::Vec<T>);

impl<T> std::fmt::Debug for Vec<T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<T> Vec<T> {
    pub fn new(value: T) -> Self {
        Vec(vec![value])
    }

    #[expect(
        clippy::len_without_is_empty,
        reason = "`is_empty` is not necessary for a `nonempty::Vec`"
    )]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn push(&mut self, value: T) {
        self.0.push(value)
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.0.len() == 1 {
            None
        }
        else {
            self.0.pop()
        }
    }

    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.0.iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, T> {
        self.0.iter_mut()
    }

    pub fn last_mut(&mut self) -> &mut T {
        self.0.last_mut().unwrap()
    }
}

impl<T> Default for Vec<T>
where
    T: Default,
{
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T, Idx> Index<Idx> for Vec<T>
where
    Idx: SliceIndex<[T]>,
{
    type Output = <Idx as SliceIndex<[T]>>::Output;

    fn index(&self, index: Idx) -> &Self::Output {
        self.0.index(index)
    }
}
