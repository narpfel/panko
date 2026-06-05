use std::fmt;

use ariadne::Color::Red;
use panko_report::Report;
use panko_report::Sliced as _;

use crate::typecheck::TypedExpression;
use crate::typecheck::constexpr::Errors;

#[derive(Debug, Clone, Report)]
#[exit_code(1)]
pub(crate) enum Diagnostic<'a> {
    #[error("{kind} in constant expression")]
    #[diagnostics(at(label = "in this expression of type `{ty}`", colour = Red))]
    #[with(ty = at.ty.ty)]
    ArithmeticError { at: TypedExpression<'a>, kind: Kind },

    #[error("constexpr evaluation not implemented yet")]
    #[diagnostics(at(colour = Red))]
    NotImplementedYet { at: TypedExpression<'a> },
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Kind {
    SignedOverflow,
    NegativeShiftLhs,
    NegativeShiftRhs,
    ShiftRhsOutOfRange,
    IsZero,
}

impl Kind {
    pub(super) fn at<'a>(self, expr: &TypedExpression<'a>) -> Errors<'a> {
        Errors::new(Diagnostic::ArithmeticError { at: *expr, kind: self })
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::SignedOverflow => "signed overflow",
            Self::NegativeShiftLhs => "left shift of negative value",
            Self::NegativeShiftRhs => "shift count negative",
            Self::ShiftRhsOutOfRange => "shift count exceeds size of shifted type",
            Self::IsZero => "division by zero",
        };
        write!(f, "{s}")
    }
}
