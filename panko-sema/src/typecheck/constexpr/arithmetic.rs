use crate::typecheck::TypedExpression;
use crate::typecheck::constexpr::Repr;
use crate::typecheck::constexpr::Value;
use crate::typecheck::constexpr::diagnostics::Diagnostic;

pub(super) trait C where
    Self: Sized,
{
    fn into_value<'a>(value: Option<Self>, at: &TypedExpression<'a>) -> Value<'a>;

    fn neg(self) -> Option<Self>;
    fn compl(self) -> Option<Self>;
    fn not(self) -> Option<i32>;
}

macro_rules! int_impl {
    (impl C for $t:ty => $prefix:ident) => {
        impl C for $t {
            fn into_value<'a>(value: Option<Self>, at: &TypedExpression<'a>) -> Value<'a> {
                let ty = at.ty.ty;
                value.map_or_else(
                    || Value::with_error(ty, Diagnostic::SignedOverflow { at: *at }),
                    |value| {
                        assert!(ty.can_represent(value));
                        let repr = Repr::Bytes(Box::new(value.to_le_bytes()));
                        Value { ty, repr }
                    },
                )
            }

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
