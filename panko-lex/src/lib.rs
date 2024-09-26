#![feature(type_alias_impl_trait)]
#![feature(closure_lifetime_binder)]

use std::ops::Range;
use std::path::Path;

use bumpalo::Bump;
use logos::Logos;

#[derive(Clone, Copy)]
pub struct Token<'a> {
    pub kind: TokenKind,
    loc: Loc<'a>,
}

impl<'a> Token<'a> {
    pub fn loc(&self) -> Loc<'a> {
        self.loc
    }

    pub fn slice(&self) -> &'a str {
        self.loc.slice()
    }
}

impl std::fmt::Debug for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}({:?})@{:?}", self.kind, self.slice(), self.loc())
    }
}

#[derive(Clone, Copy)]
struct SourceFile<'a> {
    file: &'a Path,
    src: &'a str,
}

#[derive(Clone, Copy)]
pub struct Loc<'a> {
    span: Span,
    source_file: &'a SourceFile<'a>,
}

impl std::fmt::Debug for Loc<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{:?}", self.file().display(), self.span())
    }
}

impl<'a> Loc<'a> {
    fn file(&self) -> &'a Path {
        self.source_file.file
    }

    fn src(&self) -> &'a str {
        self.source_file.src
    }

    pub fn start(&self) -> usize {
        self.span.start
    }

    fn span(&self) -> Range<usize> {
        self.span.start..self.span.end
    }

    pub fn slice(&self) -> &'a str {
        &self.src()[self.span()]
    }

    pub fn until(self, other: Self) -> Self {
        assert_eq!(self.file(), other.file());
        assert_eq!(self.src(), other.src());
        assert!(self.span.end <= other.span.start);
        Self {
            span: Span {
                start: self.span.start,
                end: other.span.end,
            },
            ..self
        }
    }

    pub fn report(&self, kind: ariadne::ReportKind<'a>) -> ariadne::ReportBuilder<'a, Self> {
        ariadne::Report::build(kind, self.file(), self.start())
    }

    pub fn cache(&self) -> impl ariadne::Cache<Path> + 'a {
        struct Cache<'b>(&'b Path, ariadne::Source<&'b str>);

        impl<'b> ariadne::Cache<Path> for Cache<'b> {
            type Storage = &'b str;

            fn fetch(
                &mut self,
                id: &Path,
            ) -> Result<&ariadne::Source<&'b str>, Box<dyn std::fmt::Debug + '_>> {
                if self.0 == id {
                    Ok(&self.1)
                }
                else {
                    Err(Box::new(format!(
                        "failed to fetch source `{}`",
                        id.display(),
                    )))
                }
            }

            fn display<'a>(&self, id: &'a Path) -> Option<Box<dyn std::fmt::Display + 'a>> {
                Some(Box::new(id.display()))
            }
        }

        Cache(self.file(), ariadne::Source::from(self.src()))
    }
}

impl ariadne::Span for Loc<'_> {
    type SourceId = Path;

    fn source(&self) -> &Self::SourceId {
        self.file()
    }

    fn start(&self) -> usize {
        self.span.start
    }

    fn end(&self) -> usize {
        self.span.end
    }
}

#[derive(Debug, Clone, Copy)]
struct Span {
    start: usize,
    end: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Logos)]
#[logos(skip r"[ \n\r\t\f]+")]
#[logos(skip r"//[^\n]*\n?")]
pub enum TokenKind {
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    #[token("<%")]
    LBrace,
    #[token("}")]
    #[token("%>")]
    RBrace,
    #[token("[")]
    #[token("<:")]
    LBracket,
    #[token("]")]
    #[token(":>")]
    RBracket,
    #[token(".")]
    Dot,
    #[token("->")]
    Arrow,

    #[token("++")]
    PlusPlus,
    #[token("--")]
    MinusMinus,
    #[token("&")]
    And,
    #[token("*")]
    Star,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("~")]
    Tilde,
    #[token("!")]
    Bang,

    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("<<")]
    LessLess,
    #[token(">>")]
    GreaterGreater,
    #[token("<")]
    Less,
    #[token(">")]
    Greater,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("==")]
    EqualEqual,
    #[token("!=")]
    BangEqual,
    #[token("^")]
    Hat,
    #[token("|")]
    Pipe,
    #[token("&&")]
    AndAnd,
    #[token("||")]
    PipePipe,

    #[token("?")]
    QuestionMark,
    #[token(":")]
    Colon,
    #[token("::")]
    ColonColon,
    #[token(";")]
    Semicolon,
    #[token("...")]
    Ellipsis,

    #[token("=")]
    Equal,
    #[token("*=")]
    StarEqual,
    #[token("/=")]
    SlashEqual,
    #[token("%=")]
    PercentEqual,
    #[token("+=")]
    PlusEqual,
    #[token("-=")]
    MinusEqual,
    #[token("<<=")]
    LessLessEqual,
    #[token(">>=")]
    GreaterGreaterEqual,
    #[token("&=")]
    AndEqual,
    #[token("^=")]
    HatEqual,
    #[token("|=")]
    PipeEqual,

    #[token(",")]
    Comma,

    #[regex(r"[\p{XID_start}_]\p{XID_continue}*")]
    Identifier,
    #[regex(r#""[^"]*""#)]
    String,
    #[regex(r"[0-9]+")]
    Integer,

    #[token("alignas")]
    #[token("_Alignas")]
    Alignas,

    #[token("alignof")]
    #[token("_Alignof")]
    Alignof,

    #[token("auto")]
    Auto,

    #[token("bool")]
    #[token("_Bool")]
    Bool,

    #[token("break")]
    Break,

    #[token("case")]
    Case,

    #[token("char")]
    Char,

    #[token("const")]
    Const,

    #[token("constexpr")]
    Constexpr,

    #[token("continue")]
    Continue,

    #[token("default")]
    Default,

    #[token("do")]
    Do,

    #[token("double")]
    Double,

    #[token("else")]
    Else,

    #[token("enum")]
    Enum,

    #[token("extern")]
    Extern,

    #[token("false")]
    False,

    #[token("float")]
    Float,

    #[token("for")]
    For,

    #[token("goto")]
    Goto,

    #[token("if")]
    If,

    #[token("inline")]
    Inline,

    #[token("int")]
    Int,

    #[token("long")]
    Long,

    #[token("nullptr")]
    Nullptr,

    #[token("register")]
    Register,

    #[token("restrict")]
    Restrict,

    #[token("return")]
    Return,

    #[token("short")]
    Short,

    #[token("signed")]
    Signed,

    #[token("sizeof")]
    Sizeof,

    #[token("static")]
    Static,

    #[token("static_assert")]
    #[token("_Static_assert")]
    StaticAssert,

    #[token("struct")]
    Struct,

    #[token("switch")]
    Switch,

    #[token("thread_local")]
    ThreadLocal,

    #[token("true")]
    True,

    #[token("typedef")]
    Typedef,

    #[token("typeof")]
    Typeof,

    #[token("typeof_unqual")]
    TypeofUnqual,

    #[token("union")]
    Union,

    #[token("unsigned")]
    Unsigned,

    #[token("void")]
    Void,

    #[token("volatile")]
    Volatile,

    #[token("while")]
    While,

    #[token("_Atomic")]
    Atomic,

    #[token("_BitInt")]
    BitInt,

    #[token("_Complex")]
    Complex,

    #[token("_Decimal128")]
    Decimal128,
    #[token("_Decimal32")]
    Decimal32,
    #[token("_Decimal64")]
    Decimal64,

    #[token("_Generic")]
    Generic,

    #[token("_Imaginary")]
    Imaginary,

    #[token("_Noreturn")]
    Noreturn,
}

#[derive(Debug, Clone, Copy)]
pub struct Error<'a> {
    pub at: Loc<'a>,
}

impl<'a> Error<'a> {
    pub fn loc(&self) -> Loc<'a> {
        self.at
    }
}

mod wrap_defining_use_for_token_iter {
    use super::*;

    pub type TokenIter<'a> = impl Iterator<Item = Result<Token<'a>, Error<'a>>>;

    pub fn lex<'a>(bump: &'a Bump, filename: &'a Path, src: &str) -> TokenIter<'a> {
        let src = bump.alloc_str(src);
        let source_file = &*bump.alloc(SourceFile { file: filename, src });
        TokenKind::lexer(src).spanned().map(|(kind, span)| {
            let loc = Loc {
                span: Span { start: span.start, end: span.end },
                source_file,
            };
            Ok(Token {
                kind: kind.map_err(|()| Error { at: loc })?,
                loc,
            })
        })
    }
}

pub use wrap_defining_use_for_token_iter::lex;
pub use wrap_defining_use_for_token_iter::TokenIter;

#[cfg(test)]
mod test {
    use rstest::fixture;
    use rstest::rstest;

    use super::*;

    #[fixture]
    fn bump() -> Bump {
        Bump::new()
    }

    macro_rules! check {
        ($body:expr) => {
            for<'a> |result: TokenIter<'a>| -> () {
                #[allow(clippy::redundant_closure_call)]
                let () = $body(result.collect::<Result<Vec<_>, _>>());
            }
        };
    }

    macro_rules! check_err {
        ($pattern:pat) => {
            check!(|result| pretty_assertions::assert_matches!(result, Err($pattern)))
        };
    }

    const FULLWIDTH_NUMBER_4_LEN: usize = '４'.len_utf8();

    #[rstest]
    #[case::unicode_number(
        "４２",
        check_err!(Error { at: Loc { span: Span { start: 0, end: FULLWIDTH_NUMBER_4_LEN }, .. } }),
    )]
    fn test_lex_error(
        bump: Bump,
        #[case] src: &str,
        #[case] expected: impl for<'a> FnOnce(TokenIter<'a>),
    ) {
        expected(lex(&bump, Path::new("<src>"), src))
    }
}
