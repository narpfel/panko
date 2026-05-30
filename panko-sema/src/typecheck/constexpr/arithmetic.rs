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

    fn mul(self, rhs: Self) -> Option<Self>;
    fn div(self, rhs: Self) -> Option<Self>;
    fn rem(self, rhs: Self) -> Option<Self>;
    fn add(self, rhs: Self) -> Option<Self>;
    fn sub(self, rhs: Self) -> Option<Self>;
    fn shl(self, rhs: u32) -> Option<Self>;
    fn shr(self, rhs: u32) -> Option<Self>;
    fn and(self, rhs: Self) -> Option<Self>;
    fn xor(self, rhs: Self) -> Option<Self>;
    fn or(self, rhs: Self) -> Option<Self>;
}

macro_rules! int_impl {
    (impl C for $t:ident with $prefix:ident { $( $meth:ident ),* } and $shl:ident) => {
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

            $(
                fn $meth(self, rhs: Self) -> Option<Self> {
                    Option::from(self.${concat($prefix, _, $meth)}(rhs))
                }
            )*

            fn shl(self, rhs: u32) -> Option<Self> {
                #[allow(unused_comparisons, reason = "this macro is used for signed and unsigned types")]
                if self < 0 {
                    return None;
                }
                if rhs >= $t::BITS {
                    return None;
                }
                Option::from(self.$shl(rhs))
            }

            fn shr(self, rhs: u32) -> Option<Self> {
                if rhs >= $t::BITS {
                    return None;
                }
                Some(self.wrapping_shr(rhs))
            }

            fn and(self, rhs: Self) -> Option<Self> {
                Some(self & rhs)
            }

            fn xor(self, rhs: Self) -> Option<Self> {
                Some(self ^ rhs)
            }

            fn or(self, rhs: Self) -> Option<Self> {
                Some(self | rhs)
            }
        }
    };
}

int_impl!(impl C for u32 with wrapping { mul, div, rem, add, sub } and wrapping_shl);
int_impl!(impl C for u64 with wrapping { mul, div, rem, add, sub } and wrapping_shl);
int_impl!(impl C for i32 with checked { mul, div, rem, add, sub } and shl_exact);
int_impl!(impl C for i64 with checked { mul, div, rem, add, sub } and shl_exact);
