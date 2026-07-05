use std::fmt;

use ariadne::Color::Blue;
use ariadne::Color::Red;
use ariadne::Fmt as _;
use panko_report::Report;
use panko_report::Sliced as _;

use crate::typecheck::Type;
use crate::typecheck::TypedExpression;
use crate::typecheck::constexpr::ConversionKind;
use crate::typecheck::constexpr::Errors;

#[derive(Debug, Clone, Report)]
#[exit_code(1)]
pub(crate) enum Diagnostic<'a> {
    #[error("{kind} in constant expression")]
    #[diagnostics(at(label = "in this expression of type `{ty}`", colour = Red))]
    #[with(ty = at.ty.ty)]
    ArithmeticError { at: TypedExpression<'a>, kind: Kind },

    #[error("not a constant expression")]
    #[diagnostics(at(colour = Red))]
    NotConstexpr { at: TypedExpression<'a> },

    #[error("integral value expected, but got value of type `{ty}`")]
    #[diagnostics(at(colour = Red))]
    #[with(ty = at.ty.ty)]
    IntegralExpected { at: TypedExpression<'a> },

    #[error("cannot expose numerical value of address bytes to constexpr context")]
    #[diagnostics(at(colour = Red))]
    AddressBytesExposed { at: TypedExpression<'a> },

    #[error("cannot perform {kind} cast from `{ty}` to `{to_ty}` in constant expression")]
    #[diagnostics(at(colour = Red))]
    #[with(
        to_ty = at.ty.ty,
        ty = ty.fg(Blue),
        kind = match kind {
            ConversionKind::Noop | ConversionKind::Bool => unreachable!(),
            ConversionKind::Truncate => "truncating",
            ConversionKind::SignExtend => "sign-extending",
            ConversionKind::ZeroExtend => "zero-extending",
        }.fg(Red),
    )]
    InvalidCast {
        at: TypedExpression<'a>,
        kind: ConversionKind,
        ty: Type<'a>,
    },
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Kind {
    SignedOverflow,
    NegativeShiftLhs,
    NegativeShiftRhs,
    ShiftRhsOutOfRange,
    IsZero,
    PointerDiffOverflow,
    UnalignedPointer,
    PointerAdditionOverflow,
    SizeOverflow,
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
            Self::PointerDiffOverflow => "overflow in pointer difference",
            Self::UnalignedPointer => "arithmetic on unaligned pointer",
            Self::PointerAdditionOverflow => "overflow in pointer addition",
            Self::SizeOverflow => "overflow in object size calculation",
        };
        write!(f, "{s}")
    }
}
