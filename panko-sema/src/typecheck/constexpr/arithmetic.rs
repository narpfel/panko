pub(super) trait C where
    Self: Sized,
{
    fn neg(self) -> Option<Self>;
    fn compl(self) -> Option<Self>;
    fn not(self) -> Option<i32>;
}

macro_rules! int_impl {
    (impl C for $t:ty => $prefix:ident) => {
        impl C for $t {
            fn neg(self) -> Option<Self> {
                Option::from(self.${concat($prefix, _neg)}())
            }

            fn compl(self) -> Option<Self> {
                Some(!self)
            }

            fn not(self) -> Option<i32> {
                Some((self == 0).into())
            }
        }
    };
}

int_impl!(impl C for u32 => wrapping);
int_impl!(impl C for u64 => wrapping);
int_impl!(impl C for i32 => checked);
int_impl!(impl C for i64 => checked);
