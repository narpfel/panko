use std::iter::Peekable;

use itertools::PeekingNext;

pub(super) struct Stacked<T>(Vec<T>);

impl<T> Stacked<T> {
    pub(super) fn new(iter: T) -> Self {
        Self(vec![iter])
    }

    pub(super) fn push(&mut self, iter: T) {
        self.0.push(iter)
    }

    pub(super) fn pop(&mut self) -> Option<T> {
        self.0.pop()
    }

    pub(super) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<T> Stacked<T>
where
    T: Iterator,
{
    pub(super) fn last_by_ref(&mut self) -> &mut T {
        self.0.last_mut().expect("`self.0` is nonempty").by_ref()
    }
}

impl<T> Stacked<Peekable<T>>
where
    T: Iterator,
{
    pub(super) fn peek(&mut self) -> Option<&T::Item> {
        self.last_by_ref().peek()
    }
}

impl<T> PeekingNext for Stacked<T>
where
    T: PeekingNext,
{
    fn peeking_next<F>(&mut self, accept: F) -> Option<Self::Item>
    where
        Self: Sized,
        F: FnOnce(&Self::Item) -> bool,
    {
        self.last_by_ref().peeking_next(accept)
    }
}

impl<T> Iterator for Stacked<T>
where
    T: Iterator,
{
    type Item = T::Item;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.0.last_mut()?.next() {
                Some(value) => return Some(value),
                None if self.0.len() == 1 => return None,
                None => self.0.pop(),
            };
        }
    }
}
