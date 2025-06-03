use ariadne::Color::Blue;
use ariadne::Color::Magenta;
use ariadne::Color::Red;
use panko_lex::Loc;
use panko_lex::Token;
use panko_report::Report;

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
}
