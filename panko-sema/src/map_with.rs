pub(crate) trait MapWithExt: Iterator {
    fn map_with<State, F, MappedItem>(self, state: State, f: F) -> MappedWith<Self, State, F>
    where
        Self: Sized,
        F: FnMut(&mut State, Self::Item) -> MappedItem,
    {
        MappedWith { iter: self, state, f }
    }
}

impl<Iter> MapWithExt for Iter where Iter: Iterator {}

pub(crate) struct MappedWith<Iter, State, F> {
    iter: Iter,
    state: State,
    f: F,
}

impl<Iter, State, F, MappedItem> Iterator for MappedWith<Iter, State, F>
where
    Iter: Iterator,
    F: FnMut(&mut State, Iter::Item) -> MappedItem,
{
    type Item = MappedItem;

    fn next(&mut self) -> Option<Self::Item> {
        Some((self.f)(&mut self.state, self.iter.next()?))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<Iter, State, F, MappedItem> ExactSizeIterator for MappedWith<Iter, State, F>
where
    Iter: ExactSizeIterator,
    F: FnMut(&mut State, Iter::Item) -> MappedItem,
{
}
