use ariadne::Color::Blue;
use ariadne::Color::Green;
use ariadne::Color::Magenta;
use ariadne::Color::Red;
use ariadne::Fmt as _;
use panko_lex::Loc;
use panko_lex::Token;
use panko_report::Report;
use panko_report::Sliced as _;

use crate::ast::FromError;

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

    #[error("`{at}` does not undefine anything")]
    #[diagnostics(at(colour = Red, label = "help: add a macro name"))]
    EmptyUndef { at: Loc<'a> },

    #[error("extraneous tokens in `{at}` directive")]
    #[diagnostics(
        at(colour = Blue),
        name(colour = Green),
        tokens(colour = Red, label = "help: remove this"),
    )]
    ExtraneousTokensInUndef {
        at: Loc<'a>,
        name: Token<'a>,
        tokens: Loc<'a>,
    },

    #[error("`{at}` at {kind} of macro")]
    #[diagnostics(at(colour = Red, label = "`{at}` operator must not be at {kind} of macro"))]
    PasteAtBeginningOfMacro { at: Token<'a>, kind: &'a str },

    #[error("duplicate parameter name in function-like macro: `{at}`")]
    #[diagnostics(at(colour = Red, label = "this parameter name is already in use"))]
    DuplicateMacroParamName { at: Token<'a> },

    #[error("unexpected token `{slice}` of kind `{kind}` in `{defined}` macro expression")]
    #[diagnostics(
        defined(colour = Blue, label = "in this `{defined}` expression"),
        at(colour = Red, label = "expected {expectation} here"),
    )]
    #[with(
        slice = at.slice().escape_debug(),
        kind = format!("{:?}", at.kind).fg(Red),
    )]
    UnexpectedTokenInDefinedExpression {
        at: Token<'a>,
        defined: Token<'a>,
        expectation: &'a str,
    },

    #[error("`{at}` not preceded by `#if`")]
    #[diagnostics(at(colour = Red, label = "this `{at}` does not have a matching `#if`"))]
    UnmatchedElif { at: Loc<'a> },
}

pub(super) struct MaybeError<T>(Option<T>);

impl<T> MaybeError<T> {
    pub(super) fn new(value: T) -> Self {
        Self(Some(value))
    }

    pub(super) fn value(self) -> Option<T> {
        let Self(value) = self;
        value
    }
}

impl<'a, T> FromError<'a> for MaybeError<T>
where
    T: 'a,
{
    fn from_error(_error: &'a dyn Report) -> Self {
        Self(None)
    }
}
