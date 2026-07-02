use std::ops::BitAnd as _;
use std::ops::BitOr as _;
use std::ops::BitXor as _;

use panko_parser::Comparison;

use crate::typecheck::TypedExpression;
use crate::typecheck::constexpr::Byte;
use crate::typecheck::constexpr::Errors;
use crate::typecheck::constexpr::Repr;
use crate::typecheck::constexpr::Value;
use crate::typecheck::constexpr::diagnostics::Kind::*;

type Result<'a, T, E = Errors<'a>> = std::result::Result<T, E>;

pub(super) trait C<'a, 'b>
where
    Self: Sized,
{
    fn into_value(self, expr: &'b TypedExpression<'a>) -> Value<'a, 'b>;

    fn neg(self, expr: &TypedExpression<'a>) -> Self;
    fn compl(self, expr: &TypedExpression<'a>) -> Self;
    fn not(self, expr: &TypedExpression<'a>) -> Result<'a, i32>;

    fn mul(self, rhs: Self, expr: &TypedExpression<'a>) -> Self;
    fn div(self, rhs: Self, expr: &TypedExpression<'a>) -> Self;
    fn rem(self, rhs: Self, expr: &TypedExpression<'a>) -> Self;
    fn add(self, rhs: Self, expr: &TypedExpression<'a>) -> Self;
    fn sub(self, rhs: Self, expr: &TypedExpression<'a>) -> Self;

    fn shl(self, rhs: Result<'a, u32>, expr: &TypedExpression<'a>) -> Self;
    fn shr(self, rhs: Result<'a, u32>) -> Self;

    fn bitand(self, rhs: Self) -> Self;
    fn bitxor(self, rhs: Self) -> Self;
    fn bitor(self, rhs: Self) -> Self;

    fn eq(self, rhs: Self) -> Result<'a, bool>;
    fn ne(self, rhs: Self) -> Result<'a, bool>;
    fn lt(self, rhs: Self) -> Result<'a, bool>;
    fn le(self, rhs: Self) -> Result<'a, bool>;
    fn gt(self, rhs: Self) -> Result<'a, bool>;
    fn ge(self, rhs: Self) -> Result<'a, bool>;

    fn nonzero(self, expr: &TypedExpression<'a>) -> Self;
    fn nonnegative(self, on_negative: impl FnOnce() -> Errors<'a>) -> Self;

    fn compare(self, kind: Comparison, rhs: Self) -> Result<'a, i32> {
        let result = match kind {
            Comparison::Equal => self.eq(rhs),
            Comparison::NotEqual => self.ne(rhs),
            Comparison::Less => self.lt(rhs),
            Comparison::LessEqual => self.le(rhs),
            Comparison::Greater => self.gt(rhs),
            Comparison::GreaterEqual => self.ge(rhs),
        };
        result.map(i32::from)
    }
}

pub(super) fn binop<'a, T, U, F, R, Err, Res>(
    lhs: Result<'a, T>,
    rhs: Result<'a, U>,
    f: F,
    error: Err,
) -> Result<'a, Res>
where
    F: FnOnce(T, U) -> R,
    Option<Res>: From<R>,
    Err: FnOnce() -> Errors<'a>,
{
    match (lhs, rhs) {
        (Ok(lhs), Ok(rhs)) => Option::from(f(lhs, rhs)).ok_or_else(error),
        (Err(errors), Ok(_)) | (Ok(_), Err(errors)) => Err(errors),
        (Err(lhs), Err(rhs)) => Err(lhs.chain(rhs)),
    }
}

macro_rules! infallible {
    ($meth:ident -> $t:ty) => {
        fn $meth(self, rhs: Self) -> Result<'a, $t> {
            binop(self, rhs, |lhs, rhs| lhs.$meth(&rhs), || unreachable!())
        }
    };
}

macro_rules! int_impl {
    (impl C for $t:ident with $prefix:ident { $( $meth:ident ),* } and $shl:ident) => {
        impl<'a, 'b> C<'a, 'b> for Result<'a, $t> {
            fn into_value(self, expr: &'b TypedExpression<'a>) -> Value<'a, 'b> {
                self.map_or_else(
                    |errors| Value::with_errors(expr, errors),
                    |value| {
                        let ty = expr.ty.ty;
                        assert!(ty.can_represent(value), "`{ty}`.can_represent({value})");
                        let repr = Repr::Bytes(Box::new(value.to_le_bytes().map(Byte::Literal)));
                        Value { expr, repr }
                    },
                )
            }

            fn neg(self, expr: &TypedExpression<'a>) -> Self {
                Option::from(self?.${concat($prefix, _neg)}()).ok_or_else(|| SignedOverflow.at(expr))
            }

            fn compl(self, _expr: &TypedExpression<'a>) -> Self {
                Ok(!self?)
            }

            fn not(self, _expr: &TypedExpression<'a>) -> Result<'a, i32> {
                Ok((self? == 0).into())
            }

            $(
                fn $meth(self, rhs: Self, expr: &TypedExpression<'a>) -> Self {
                    binop(self, rhs, $t::${concat($prefix, _, $meth)}, || SignedOverflow.at(expr))
                }
            )*

            fn shl(self, rhs: Result<'a, u32>, expr: &TypedExpression<'a>) -> Self {
                let lhs = self.nonnegative(|| NegativeShiftLhs.at(expr));
                binop(lhs, rhs, $t::$shl, || SignedOverflow.at(expr))
            }

            fn shr(self, rhs: Result<'a, u32>) -> Self {
                binop(self, rhs, |lhs, rhs| lhs.wrapping_shr(rhs), || unreachable!())
            }

            infallible!(bitand -> $t);
            infallible!(bitor -> $t);
            infallible!(bitxor -> $t);
            infallible!(eq -> bool);
            infallible!(ne -> bool);
            infallible!(lt -> bool);
            infallible!(le -> bool);
            infallible!(gt -> bool);
            infallible!(ge -> bool);

            fn nonzero(self, expr: &TypedExpression<'a>) -> Self {
                match self? {
                    0 => Err(IsZero.at(expr)),
                    value => Ok(value),
                }
            }

            fn nonnegative(self, on_negative: impl FnOnce() -> Errors<'a>) -> Self {
                match self? {
                    #[allow(unused_comparisons, reason = "this macro is used for signed and unsigned types")]
                    value if value < 0 => Err(on_negative()),
                    value => Ok(value),
                }
            }
        }
    };
}

int_impl!(impl C for u32 with wrapping { mul, div, rem, add, sub } and wrapping_shl);
int_impl!(impl C for u64 with wrapping { mul, div, rem, add, sub } and wrapping_shl);
int_impl!(impl C for i32 with checked { mul, div, rem, add, sub } and shl_exact);
int_impl!(impl C for i64 with checked { mul, div, rem, add, sub } and shl_exact);
