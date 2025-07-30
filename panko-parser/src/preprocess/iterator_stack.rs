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
