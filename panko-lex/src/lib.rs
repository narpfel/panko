#![feature(closure_lifetime_binder)]
#![feature(type_alias_impl_trait)]
#![feature(unqualified_local_imports)]

use std::cell::RefCell;
use std::ops::Range;
use std::path::Path;
use std::path::PathBuf;
use std::sync::LazyLock;

use bumpalo::Bump;
use logos::Lexer;
use logos::Logos;

pub use crate::typedef_names::TypedefNames;

pub mod typedef_names;

#[derive(Clone, Copy)]
pub struct Token<'a> {
    pub kind: TokenKind,
    loc: Loc<'a>,
}

impl<'a> Token<'a> {
    pub fn synthesised(kind: TokenKind, loc: Loc<'a>) -> Self {
        Self { kind, loc }
    }

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
        let prefix = &self.source_file.src[..self.span().start];
        let line = prefix.bytes().filter(|c| *c == b'\n').count() + 1;
        let column = prefix
            .rfind('\n')
            .map(|i| prefix.len() - i)
            .unwrap_or(prefix.len() + 1);
        write!(f, "{}:{line}:{column}", self.file().display())
    }
}

impl<'a> Loc<'a> {
    pub fn synthesised() -> Self {
        static PATH: LazyLock<PathBuf> = LazyLock::new(|| PathBuf::from("<synthesised>"));
        static SOURCE_FILE: LazyLock<SourceFile> =
            LazyLock::new(|| SourceFile { file: &PATH, src: "<synthesised>" });

        Self {
            span: Span { start: 0, end: 0 },
            source_file: &SOURCE_FILE,
        }
    }

    pub fn file(&self) -> &'a Path {
        self.source_file.file
    }

    pub fn src(&self) -> &'a str {
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
        let start = self.span.start.min(other.span.start);
        let end = self.span.end.max(other.span.end);
        assert!(start <= end);
        Self { span: Span { start, end }, ..self }
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

    pub fn loc(&self) -> Self {
        *self
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
pub enum IntegerSuffix {
    None,
    #[regex("[uU]")]
    Unsigned,
    #[regex("[uU][lL]|[lL][uU]")]
    UnsignedLong,
    #[regex("[uU](ll|LL)|(ll|LL)[uU]")]
    UnsignedLongLong,
    #[regex("[lL]")]
    Long,
    #[regex("ll|LL")]
    LongLong,
    #[regex("wb|WB")]
    BitInt,
    #[regex("[uU](wb|WB)|(wb|WB)[uU]")]
    UnsignedBitInt,
    #[regex(r"[\p{XID_start}_]\p{XID_continue}*", priority = 0)]
    Invalid,
}

fn lex_integer_suffix(src: &str) -> (usize, IntegerSuffix) {
    let mut lexer = IntegerSuffix::lexer(src).spanned();
    match lexer.next() {
        None | Some((Err(()), _)) => (0, IntegerSuffix::None),
        Some((Ok(suffix), span)) => (span.end, suffix),
    }
}

fn lex_integer(lexer: &mut Lexer<TokenKind>, base: u32) -> Integer {
    let (suffix_len, suffix) = lex_integer_suffix(lexer.remainder());
    lexer.bump(suffix_len);
    let prefix_len = match base {
        10 => 0,
        8 => 0,
        16 => 2,
        2 => 2,
        _ => unreachable!(),
    };
    Integer { suffix, suffix_len, base, prefix_len }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Integer {
    pub suffix: IntegerSuffix,
    pub suffix_len: usize,
    pub base: u32,
    pub prefix_len: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum EncodingPrefix {
    None,
    Utf8,
    Utf16,
    Utf32,
    Wchar,
}

impl EncodingPrefix {
    #[expect(clippy::len_without_is_empty, reason = "`Self` is not a container`")]
    pub fn len(self) -> usize {
        match self {
            Self::None => 0,
            Self::Utf8 => 2,
            Self::Utf16 => 1,
            Self::Utf32 => 1,
            Self::Wchar => 1,
        }
    }

    pub fn encoding(self) -> &'static str {
        match self {
            EncodingPrefix::Utf8 => "UTF-8",
            EncodingPrefix::Utf16 => "UTF-16",
            EncodingPrefix::Utf32 => "UTF-32",
            EncodingPrefix::None | EncodingPrefix::Wchar => unreachable!(),
        }
    }
}

fn lex_encoding_prefix(lexer: &mut Lexer<TokenKind>) -> EncodingPrefix {
    // the slice it at least two bytes long due to opening and closing quotes
    let bytes = lexer.slice().as_bytes();
    let maybe_prefix = (bytes[0], bytes[1]);
    match maybe_prefix {
        (b'u', b'8') => EncodingPrefix::Utf8,
        (b'u', _) => EncodingPrefix::Utf16,
        (b'U', _) => EncodingPrefix::Utf32,
        (b'L', _) => EncodingPrefix::Wchar,
        _ => EncodingPrefix::None,
    }
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

    // only generated via the lexer hack
    TypeIdentifier,

    #[regex(r#""([^"\\\n]|\\(['"?\\abfnrtv\n]|[0-7]{1,3}|x[0-9a-fA-F]+))*""#)]
    String,

    #[regex("[1-9]('?[0-9])*", |lexer| lex_integer(lexer, 10))]
    #[regex("0[bB]0('?[01])*", |lexer| lex_integer(lexer, 2))]
    #[regex("0[bB]1('?[01])*", |lexer| lex_integer(lexer, 2))]
    #[regex("0[xX]0('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]1('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]2('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]3('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]4('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]5('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]6('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]7('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]8('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]9('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]a('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]b('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]c('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]d('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]e('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]f('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]A('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]B('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]C('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]D('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]E('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0[xX]F('?[0-9a-fA-F])*", |lexer| lex_integer(lexer, 16))]
    #[regex("0('?[0-7])*", |lexer| lex_integer(lexer, 8))]
    Integer(Integer),

    #[regex(
        r#"(u8|u|U|L)?'([^'\\\n]|\\(['"?\\abfnrtv\n]|[0-7]{1,3}|x[0-9a-fA-F]+))*'"#,
        lex_encoding_prefix
    )]
    CharConstant(EncodingPrefix),

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

    #[token("_Lengthof")]
    Lengthof,
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

pub type TokenIter<'a> = impl Iterator<Item = Result<Token<'a>, Error<'a>>>;

#[define_opaque(TokenIter)]
pub fn lex<'a>(
    bump: &'a Bump,
    filename: &'a Path,
    src: &str,
    typedef_names: &'a RefCell<TypedefNames<'a>>,
) -> TokenIter<'a> {
    let src = bump.alloc_str(src);
    let source_file = &*bump.alloc(SourceFile { file: filename, src });
    TokenKind::lexer(src).spanned().map(|(kind, span)| {
        let loc = Loc {
            span: Span { start: span.start, end: span.end },
            source_file,
        };

        let kind = kind.map_err(|()| Error { at: loc })?;
        let kind = match kind {
            TokenKind::Identifier if typedef_names.borrow().is_type_identifier(loc.slice()) =>
                TokenKind::TypeIdentifier,
            kind => kind,
        };
        Ok(Token { kind, loc })
    })
}

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
    #[case::zero_b_literal("0b", &[
        TokenKind::Integer(Integer {
            suffix: IntegerSuffix::Invalid,
            suffix_len: 1,
            base: 8,
            prefix_len: 0,
        })
    ])]
    #[case::zero_x_literal("0x", &[
        TokenKind::Integer(Integer {
            suffix: IntegerSuffix::Invalid,
            suffix_len: 1,
            base: 8,
            prefix_len: 0,
        })
    ])]
    #[case::invalid_suffix("123abc", &[
        TokenKind::Integer(Integer {
            suffix: IntegerSuffix::Invalid,
            suffix_len: 3,
            base: 10,
            prefix_len: 0,
        })
    ])]
    #[case::invalid_suffix("0bcd123abc", &[
        TokenKind::Integer(Integer {
            suffix: IntegerSuffix::Invalid,
            suffix_len: 9,
            base: 8,
            prefix_len: 0,
        })
    ])]
    #[case::invalid_suffix("0xcd123xyz", &[
        TokenKind::Integer(Integer {
            suffix: IntegerSuffix::Invalid,
            suffix_len: 3,
            base: 16,
            prefix_len: 2,
        })
    ])]
    #[case::invalid_suffix("0xqd123abc", &[
        TokenKind::Integer(Integer {
            suffix: IntegerSuffix::Invalid,
            suffix_len: 9,
            base: 8,
            prefix_len: 0,
        })
    ])]
    fn test_lexer(bump: Bump, #[case] src: &str, #[case] expected: &[TokenKind]) {
        let tokens = lex(&bump, Path::new("<src>"), src, &RefCell::default())
            .map(|token| Ok(token?.kind))
            .collect::<Result<Vec<_>, Error>>()
            .unwrap();
        pretty_assertions::assert_eq!(tokens, expected);
    }

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
        expected(lex(&bump, Path::new("<src>"), src, &RefCell::default()))
    }

    #[rstest]
    #[case::no_suffix_at_end_of_input("", 0, IntegerSuffix::None)]
    #[case::no_suffix("+ 42", 0, IntegerSuffix::None)]
    #[case::no_suffix_then_unsigned(" u", 0, IntegerSuffix::None)]
    #[case::unsigned("u)", 1, IntegerSuffix::Unsigned)]
    #[case::ullong("uLL ", 3, IntegerSuffix::UnsignedLongLong)]
    fn test_integer_suffix_lexer(
        #[case] src: &str,
        #[case] suffix_len: usize,
        #[case] expected: IntegerSuffix,
    ) {
        pretty_assertions::assert_eq!(lex_integer_suffix(src), (suffix_len, expected));
    }
}
