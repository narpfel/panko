use ariadne::Color::Blue;
use ariadne::Color::Magenta;
use ariadne::Color::Red;
use ariadne::Fmt as _;
use panko_lex::Loc;
use panko_lex::Token;
use panko_report::Report;
use panko_report::Sliced as _;

#[derive(Debug, Report)]
#[exit_code(1)]
pub(super) enum Diagnostic<'a> {
    #[error("`{at}` outside of preprocessing directive")]
    #[diagnostics(at(colour = Red))]
    HashOutsideDirective { at: Token<'a> },

    #[error("preprocessor directive does not start at beginning of line")]
    #[diagnostics(at(colour = Red), line(colour = Blue))]
    DirectiveDoesNotStartAtBeginningOfLine { at: Token<'a>, line: Loc<'a> },

    #[error("preprocessor directive `{at}{directive}` does not start at beginning of line")]
    #[diagnostics(at(colour = Red), directive(colour = Magenta), line(colour = Blue))]
    NamedDirectiveDoesNotStartAtBeginningOfLine {
        at: Token<'a>,
        directive: Token<'a>,
        line: Loc<'a>,
    },

    #[error(
        "argument count mismatch: too {determiner} arguments to function-like macro invocation (expected {at_least}{expected} but got {actual})"
    )]
    #[diagnostics(
        at(colour = Red, label = "this macro expects {at_least}{expected} argument{maybe_plural_s}"),
    )]
    #[with(
        at_least = if *is_varargs { "at least " } else { "" },
        determiner = if expected > actual { "few" } else { "many" },
        maybe_plural_s = if *expected != 1 { "s" } else { "" },
        expected = expected.fg(Blue),
        actual = actual.fg(Red),
    )]
    ArityMismatch {
        at: Token<'a>,
        expected: usize,
        actual: usize,
        is_varargs: bool,
    },

    #[error("`{at}` outside of variadic macro")]
    #[diagnostics(at(colour = Red, label = "`{at}` is forbidden outside of variadic macros"))]
    VaArgsOrVaOptOutsideOfVariadicMacro { at: Token<'a> },

    #[error("`{at}` nested in another `{va_opt}`")]
    #[diagnostics(
        va_opt(colour = Blue, label = "... in this `{va_opt}`"),
        at(colour = Red, label = "this `{at}` is nested ..."),
    )]
    NestedVaOpt { at: Token<'a>, va_opt: Token<'a> },

    #[error("`{at}` does not define anything")]
    #[diagnostics(at(colour = Red, label = "help: add a macro name and an optional expansion"))]
    EmptyDefine { at: Loc<'a> },

    #[error("`{define}` of non-identifier `{at}`")]
    #[diagnostics(
        define(colour = Blue),
        at(colour = Red, label = "only identifiers can be `{define}`d"),
    )]
    DefineOfNonIdentifier { at: Token<'a>, define: Loc<'a> },
}
