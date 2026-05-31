use crate::typecheck::TypedExpression;
use crate::typecheck::constexpr::Repr;
use crate::typecheck::constexpr::Value;
use crate::typecheck::constexpr::diagnostics::Diagnostic;
use crate::typecheck::constexpr::diagnostics::Kind;
use crate::typecheck::constexpr::diagnostics::Kind::*;

type Result<T, E = Kind> = std::result::Result<T, E>;

pub(super) trait C where
    Self: Sized,
{
    fn into_value<'a>(value: Result<Self>, at: &TypedExpression<'a>) -> Value<'a>;

    fn neg(self) -> Result<Self>;
    fn compl(self) -> Result<Self>;
    fn not(self) -> Result<i32>;

    fn mul(self, rhs: Self) -> Result<Self>;
    fn div(self, rhs: Self) -> Result<Self>;
    fn rem(self, rhs: Self) -> Result<Self>;
    fn add(self, rhs: Self) -> Result<Self>;
    fn sub(self, rhs: Self) -> Result<Self>;
    fn shl(self, rhs: u32) -> Result<Self>;
    fn shr(self, rhs: u32) -> Result<Self>;
    fn and(self, rhs: Self) -> Result<Self>;
    fn xor(self, rhs: Self) -> Result<Self>;
    fn or(self, rhs: Self) -> Result<Self>;
}

macro_rules! int_impl {
    (impl C for $t:ident with $prefix:ident { $( $meth:ident ),* } and $shl:ident) => {
        impl C for $t {
            fn into_value<'a>(value: Result<Self>, at: &TypedExpression<'a>) -> Value<'a> {
                let ty = at.ty.ty;
                value.map_or_else(
                    |kind| Value::with_error(ty, Diagnostic::ArithmeticError { at: *at, kind }),
                    |value| {
                        assert!(ty.can_represent(value));
                        let repr = Repr::Bytes(Box::new(value.to_le_bytes()));
                        Value { ty, repr }
                    },
                )
            }

            fn neg(self) -> Result<Self> {
                Option::from(self.${concat($prefix, _neg)}()).ok_or(SignedOverflow)
            }

            fn compl(self) -> Result<Self> {
                Ok(!self)
            }

            fn not(self) -> Result<i32> {
                Ok((self == 0).into())
            }

            $(
                fn $meth(self, rhs: Self) -> Result<Self> {
                    Option::from(self.${concat($prefix, _, $meth)}(rhs)).ok_or(SignedOverflow)
                }
            )*

            fn shl(self, rhs: u32) -> Result<Self> {
                #[allow(unused_comparisons, reason = "this macro is used for signed and unsigned types")]
                if self < 0 {
                    return Err(NegativeShiftLhs);
                }
                if rhs >= $t::BITS {
                    return Err(ShiftRhsOutOfRange);
                }
                Option::from(self.$shl(rhs)).ok_or(SignedOverflow)
            }

            fn shr(self, rhs: u32) -> Result<Self> {
                if rhs >= $t::BITS {
                    return Err(ShiftRhsOutOfRange);
                }
                Ok(self.wrapping_shr(rhs))
            }

            fn and(self, rhs: Self) -> Result<Self> {
                Ok(self & rhs)
            }

            fn xor(self, rhs: Self) -> Result<Self> {
                Ok(self ^ rhs)
            }

            fn or(self, rhs: Self) -> Result<Self> {
                Ok(self | rhs)
            }
        }
    };
}

int_impl!(impl C for u32 with wrapping { mul, div, rem, add, sub } and wrapping_shl);
int_impl!(impl C for u64 with wrapping { mul, div, rem, add, sub } and wrapping_shl);
int_impl!(impl C for i32 with checked { mul, div, rem, add, sub } and shl_exact);
int_impl!(impl C for i64 with checked { mul, div, rem, add, sub } and shl_exact);
