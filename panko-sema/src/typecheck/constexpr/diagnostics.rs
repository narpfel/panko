use ariadne::Color::Red;
use panko_report::Report;

use crate::typecheck::TypedExpression;

#[derive(Debug, Report)]
#[exit_code(1)]
pub(crate) enum Diagnostic<'a> {
    #[error("signed overflow in constant expression")]
    #[diagnostics(at(label = "this expression overflows its type `{ty}`", colour = Red))]
    #[with(ty = at.ty.ty)]
    SignedOverflow { at: TypedExpression<'a> },
}
