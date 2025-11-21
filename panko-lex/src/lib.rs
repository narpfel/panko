#![feature(closure_lifetime_binder)]
#![feature(iter_map_windows)]
#![feature(type_alias_impl_trait)]
#![feature(unqualified_local_imports)]

use std::cell::RefCell;
use std::iter::once;
use std::ops::Range;
use std::path::Path;
use std::path::PathBuf;
use std::sync::LazyLock;

use logos::Lexer;
use logos::Logos;
use logos::Skip;

pub use crate::bump::Bump;
pub use crate::typedef_names::TypedefNames;

mod bump;
pub mod typedef_names;

#[derive(Clone, Copy)]
pub struct Token<'a> {
    pub kind: TokenKind,
    loc: Loc<'a>,
}

impl<'a> Token<'a> {
    pub fn from_str(bump: &'a Bump, kind: TokenKind, s: &'a str) -> Self {
        Self {
            kind,
            loc: Loc {
                span: Span { start: 0, end: s.len() },
                source_file: bump.alloc(SourceFile {
                    file: Path::new("<scratch area>"),
                    physical_src: s,
                    logical_src: s,
                    chunk_starts: &[0],
                }),
            },
        }
    }

    pub fn synthesised(kind: TokenKind, loc: Loc<'a>) -> Self {
        Self { kind, loc }
    }

    pub fn loc(&self) -> Loc<'a> {
        self.loc
    }

    pub fn slice(&self) -> &'a str {
        self.loc.slice()
    }

    pub fn is_identifier(&self) -> bool {
        matches!(self.kind, TokenKind::Identifier | TokenKind::TypeIdentifier)
    }

    pub fn is_keyword(&self) -> bool {
        match self.kind {
            TokenKind::Error(_)
            | TokenKind::BlockComment
            | TokenKind::Newline
            | TokenKind::Hash
            | TokenKind::HashHash
            | TokenKind::LParen
            | TokenKind::RParen
            | TokenKind::LBrace
            | TokenKind::RBrace
            | TokenKind::LBracket
            | TokenKind::RBracket
            | TokenKind::Dot
            | TokenKind::Arrow
            | TokenKind::PlusPlus
            | TokenKind::MinusMinus
            | TokenKind::And
            | TokenKind::Star
            | TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Tilde
            | TokenKind::Bang
            | TokenKind::Slash
            | TokenKind::Percent
            | TokenKind::LessLess
            | TokenKind::GreaterGreater
            | TokenKind::Less
            | TokenKind::Greater
            | TokenKind::LessEqual
            | TokenKind::GreaterEqual
            | TokenKind::EqualEqual
            | TokenKind::BangEqual
            | TokenKind::Hat
            | TokenKind::Pipe
            | TokenKind::AndAnd
            | TokenKind::PipePipe
            | TokenKind::QuestionMark
            | TokenKind::Colon
            | TokenKind::ColonColon
            | TokenKind::Semicolon
            | TokenKind::Ellipsis
            | TokenKind::Equal
            | TokenKind::StarEqual
            | TokenKind::SlashEqual
            | TokenKind::PercentEqual
            | TokenKind::PlusEqual
            | TokenKind::MinusEqual
            | TokenKind::LessLessEqual
            | TokenKind::GreaterGreaterEqual
            | TokenKind::AndEqual
            | TokenKind::HatEqual
            | TokenKind::PipeEqual
            | TokenKind::Comma
            | TokenKind::Identifier
            | TokenKind::TypeIdentifier
            | TokenKind::String
            | TokenKind::Integer(_)
            | TokenKind::CharConstant(_) => false,

            TokenKind::Alignas
            | TokenKind::Alignof
            | TokenKind::Auto
            | TokenKind::Bool
            | TokenKind::Break
            | TokenKind::Case
            | TokenKind::Char
            | TokenKind::Const
            | TokenKind::Constexpr
            | TokenKind::Continue
            | TokenKind::Default
            | TokenKind::Do
            | TokenKind::Double
            | TokenKind::Else
            | TokenKind::Enum
            | TokenKind::Extern
            | TokenKind::False
            | TokenKind::Float
            | TokenKind::For
            | TokenKind::Goto
            | TokenKind::If
            | TokenKind::Inline
            | TokenKind::Int
            | TokenKind::Long
            | TokenKind::Nullptr
            | TokenKind::Register
            | TokenKind::Restrict
            | TokenKind::Return
            | TokenKind::Short
            | TokenKind::Signed
            | TokenKind::Sizeof
            | TokenKind::Static
            | TokenKind::StaticAssert
            | TokenKind::Struct
            | TokenKind::Switch
            | TokenKind::ThreadLocal
            | TokenKind::True
            | TokenKind::Typedef
            | TokenKind::Typeof
            | TokenKind::TypeofUnqual
            | TokenKind::Union
            | TokenKind::Unsigned
            | TokenKind::Void
            | TokenKind::Volatile
            | TokenKind::While
            | TokenKind::Atomic
            | TokenKind::BitInt
            | TokenKind::Complex
            | TokenKind::Decimal128
            | TokenKind::Decimal32
            | TokenKind::Decimal64
            | TokenKind::Generic
            | TokenKind::Imaginary
            | TokenKind::Noreturn
            | TokenKind::Lengthof => true,
        }
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
    physical_src: &'a str,
    logical_src: &'a str,
    chunk_starts: &'a [usize],
}

#[derive(Clone, Copy)]
pub struct Loc<'a> {
    span: Span,
    source_file: &'a SourceFile<'a>,
}

impl std::fmt::Debug for Loc<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = &self.source_file.physical_src[..ariadne::Span::start(self)];
        let line = self.line();
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
        static SOURCE_FILE: LazyLock<SourceFile> = LazyLock::new(|| SourceFile {
            file: &PATH,
            physical_src: "<synthesised>",
            logical_src: "<synthesised>",
            chunk_starts: &[0],
        });

        Self {
            span: Span { start: 0, end: 0 },
            source_file: &SOURCE_FILE,
        }
    }

    pub fn file(&self) -> &'a Path {
        self.source_file.file
    }

    pub fn src(&self) -> &'a str {
        self.source_file.logical_src
    }

    pub fn start(&self) -> usize {
        self.span.start
    }

    pub fn end(&self) -> usize {
        self.span.end
    }

    fn span(&self) -> Range<usize> {
        self.span.start..self.span.end
    }

    pub fn slice(&self) -> &'a str {
        &self.src()[self.span()]
    }

    pub fn line(&self) -> usize {
        let prefix = &self.source_file.physical_src[..ariadne::Span::start(self)];
        prefix.bytes().filter(|c| *c == b'\n').count() + 1
    }

    pub fn until(self, other: Self) -> Self {
        // TODO: This check is incorrect for two tokens that are in different files with the same
        // name (`<scratch area>` and `<synthesised>` files are special-cased). Would be fixed by a
        // string/file/token interner.
        if self.file() == other.file()
            && !["<synthesised>".as_ref(), "<scratch area>".as_ref()].contains(&self.file())
        {
            // TODO: this assumes that `Loc`s are contiguous
            debug_assert_eq!(self.src(), other.src());
            let start = self.span.start.min(other.span.start);
            let end = self.span.end.max(other.span.end);
            assert!(start <= end);
            Self { span: Span { start, end }, ..self }
        }
        else {
            // TODO: how to represent locations that span multiple files?
            self
        }
    }

    pub fn report(&self, kind: ariadne::ReportKind<'a>) -> ariadne::ReportBuilder<'a, Self> {
        ariadne::Report::build(kind, *self)
    }

    pub fn cache(&self) -> impl ariadne::Cache<Path> + 'a {
        struct Cache<'b>(&'b Path, ariadne::Source<&'b str>);

        impl<'b> ariadne::Cache<Path> for Cache<'b> {
            type Storage = &'b str;

            fn fetch(
                &mut self,
                id: &Path,
            ) -> Result<&ariadne::Source<&'b str>, impl std::fmt::Debug> {
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

            fn display<'a>(&self, id: &'a Path) -> Option<impl std::fmt::Display + 'a> {
                Some(Box::new(id.display()))
            }
        }

        Cache(
            self.file(),
            ariadne::Source::from(self.source_file.physical_src),
        )
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
        let start = self.span.start;
        let index = self
            .source_file
            .chunk_starts
            .binary_search(&start)
            .unwrap_or_else(|i| i - 1);
        start + 2 * index
    }

    fn end(&self) -> usize {
        let end = self.span.end;
        let index = self
            .source_file
            .chunk_starts
            .binary_search(&end)
            .unwrap_or_else(|i| i - 1);
        end + 2 * index
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

fn skip_block_comment(lexer: &mut Lexer<TokenKind>) -> Result<Skip, ErrorKind> {
    const END_COMMENT_MARKER: &[u8] = b"*/";
    static FINDER: LazyLock<memchr::memmem::Finder> =
        LazyLock::new(|| memchr::memmem::Finder::new(END_COMMENT_MARKER));

    let bytes = lexer.remainder().as_bytes();
    let end_comment_start = FINDER
        .find(bytes)
        .ok_or(ErrorKind::UnterminatedBlockComment)?;
    lexer.bump(end_comment_start + END_COMMENT_MARKER.len());
    Ok(Skip)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Logos)]
#[logos(error = ErrorKind)]
#[logos(skip r"[ \r\t\f]+")]
#[logos(skip r"//[^\n]*")]
pub enum TokenKind {
    Error(ErrorKind),

    #[token("/*", skip_block_comment)]
    BlockComment,

    #[token("\n")]
    Newline,

    #[token("#")]
    #[token("%:")]
    Hash,
    #[token("##")]
    #[token("%:%:")]
    HashHash,

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

    #[regex(r#""([^"\\\n]|\\(['"?\\abfnrtv]|[0-7]{1,3}|x[0-9a-fA-F]+))*["\n]"#)]
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
        r#"(u8|u|U|L)?'([^'\\\n]|\\(['"?\\abfnrtv]|[0-7]{1,3}|x[0-9a-fA-F]+))*['\n]"#,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum ErrorKind {
    #[default]
    Other,
    UnterminatedBlockComment,
}

pub type TokenIter<'a> = impl Iterator<Item = Token<'a>>;

#[define_opaque(TokenIter)]
pub fn lex<'a>(bump: &'a Bump, filename: &'a Path, physical_src: &str) -> TokenIter<'a> {
    let (logical_src, chunk_starts): (String, Vec<_>) = once(0)
        .chain(memchr::memmem::find_iter(physical_src.as_bytes(), b"\\\n"))
        .chain(once(physical_src.len()))
        .map_windows(|&[start, end]| {
            let chunk = &physical_src[start..end];
            (chunk.strip_prefix("\\\n").unwrap_or(chunk), start)
        })
        .collect();
    let logical_src = bump.alloc_str(&logical_src);
    let chunk_starts = bump.alloc_slice_copy(&chunk_starts);
    let source_file = bump.alloc(SourceFile {
        file: filename,
        physical_src: bump.alloc_str(physical_src),
        logical_src,
        chunk_starts,
    });
    TokenKind::lexer(logical_src).spanned().map(|(kind, span)| {
        let loc = Loc {
            span: Span { start: span.start, end: span.end },
            source_file,
        };

        let kind = kind.unwrap_or_else(TokenKind::Error);
        Token { kind, loc }
    })
}

pub type LexerHacked<'a, Tokens>
where
    Tokens: Iterator<Item = Token<'a>>,
= impl Iterator<Item = Token<'a>>;

#[define_opaque(LexerHacked)]
pub fn apply_lexer_hack<'a, Tokens>(
    tokens: Tokens,
    typedef_names: &'a RefCell<TypedefNames<'a>>,
) -> LexerHacked<'a, Tokens>
where
    Tokens: Iterator<Item = Token<'a>>,
{
    tokens.map(|Token { kind, loc }| {
        let kind = match kind {
            TokenKind::Identifier if typedef_names.borrow().is_type_identifier(loc.slice()) =>
                TokenKind::TypeIdentifier,
            kind => kind,
        };
        Token { kind, loc }
    })
}

#[cfg(test)]
mod test {
    use super::*;

    const FULLWIDTH_NUMBER_4_LEN: usize = '４'.len_utf8();
    const FULLWIDTH_NUMBER_2_END: usize = '４'.len_utf8() + '２'.len_utf8();

    mod test_lexer {
        use super::*;

        macro_rules! test_lexer {
            ($name:ident, $src:expr, $expected:expr $(,)?) => {
                #[test]
                fn $name() {
                    let bump = Bump::new();
                    let tokens = lex(&bump, Path::new("<src>"), $src)
                        .map(|token| token.kind)
                        .collect::<Vec<_>>();
                    pretty_assertions::assert_eq!(tokens, $expected);
                }
            };
        }

        test_lexer!(
            zero_b_literal,
            "0b",
            &[TokenKind::Integer(Integer {
                suffix: IntegerSuffix::Invalid,
                suffix_len: 1,
                base: 8,
                prefix_len: 0,
            })],
        );
        test_lexer!(
            zero_x_literal,
            "0x",
            &[TokenKind::Integer(Integer {
                suffix: IntegerSuffix::Invalid,
                suffix_len: 1,
                base: 8,
                prefix_len: 0,
            })],
        );
        test_lexer!(
            invalid_suffix,
            "123abc",
            &[TokenKind::Integer(Integer {
                suffix: IntegerSuffix::Invalid,
                suffix_len: 3,
                base: 10,
                prefix_len: 0,
            })],
        );
        test_lexer!(
            invalid_suffix_bin,
            "0bcd123abc",
            &[TokenKind::Integer(Integer {
                suffix: IntegerSuffix::Invalid,
                suffix_len: 9,
                base: 8,
                prefix_len: 0,
            })],
        );
        test_lexer!(
            invalid_suffix_hex,
            "0xcd123xyz",
            &[TokenKind::Integer(Integer {
                suffix: IntegerSuffix::Invalid,
                suffix_len: 3,
                base: 16,
                prefix_len: 2,
            })],
        );
        test_lexer!(
            invalid_suffix_hex_2,
            "0xqd123abc",
            &[TokenKind::Integer(Integer {
                suffix: IntegerSuffix::Invalid,
                suffix_len: 9,
                base: 8,
                prefix_len: 0,
            })],
        );
    }

    #[test]
    fn test_lex_error_unicode_error() {
        let bump = Bump::new();
        let src = "４２";
        pretty_assertions::assert_matches!(
            lex(&bump, Path::new("<src>"), src).collect::<Vec<_>>()[..],
            [
                Token {
                    loc: Loc {
                        span: Span { start: 0, end: FULLWIDTH_NUMBER_4_LEN },
                        source_file: _,
                    },
                    kind: TokenKind::Error(ErrorKind::Other),
                },
                Token {
                    loc: Loc {
                        span: Span {
                            start: FULLWIDTH_NUMBER_4_LEN,
                            end: FULLWIDTH_NUMBER_2_END
                        },
                        source_file: _,
                    },
                    kind: TokenKind::Error(ErrorKind::Other),
                },
            ],
        )
    }

    mod test_integer_suffix_lexer {
        use super::*;

        macro_rules! test_integer_suffix_lexer {
            ($name:ident, $src:expr, $suffix_len:expr, $expected:expr) => {
                #[test]
                fn $name() {
                    pretty_assertions::assert_eq!(
                        lex_integer_suffix($src),
                        ($suffix_len, $expected),
                    )
                }
            };
        }

        test_integer_suffix_lexer!(no_suffix_at_end_of_input, "", 0, IntegerSuffix::None);
        test_integer_suffix_lexer!(no_suffix, "+ 42", 0, IntegerSuffix::None);
        test_integer_suffix_lexer!(no_suffix_then_unsigned, " u", 0, IntegerSuffix::None);
        test_integer_suffix_lexer!(unsigned, "u)", 1, IntegerSuffix::Unsigned);
        test_integer_suffix_lexer!(ullong, "uLL ", 3, IntegerSuffix::UnsignedLongLong);
    }
}
