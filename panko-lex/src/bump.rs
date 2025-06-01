#[derive(Debug, Default)]
pub struct Bump(bumpalo::Bump);

impl Bump {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn alloc<T>(&self, value: T) -> &T {
        const {
            assert!(!std::mem::needs_drop::<T>());
        }
        self.0.alloc(value)
    }

    pub fn alloc_str<'a>(&'a self, src: &str) -> &'a str {
        self.0.alloc_str(src)
    }

    pub fn alloc_slice_copy<'a, T>(&'a self, src: &[T]) -> &'a [T]
    where
        T: Copy,
    {
        self.0.alloc_slice_copy(src)
    }

    pub fn alloc_slice_fill_iter<T, I>(&self, iter: I) -> &[T]
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
    {
        const {
            assert!(!std::mem::needs_drop::<T>());
        }
        self.0.alloc_slice_fill_iter(iter)
    }
}
