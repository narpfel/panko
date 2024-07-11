// FIXME: everything is incomplete, so there are a lot of unused things
#![expect(unused)]

use bumpalo::Bump;
use itertools::Itertools as _;
use lalrpop_util::lalrpop_mod;
use panko_lex::Token;
use panko_lex::TokenIter;
use panko_lex::TokenKind;

use crate::indent_lines::IndentLines as _;

mod indent_lines;

lalrpop_mod!(grammar);

pub const SEXPR_INDENT: usize = 3;
const NO_VALUE: &str = "∅";

#[derive(Debug)]
pub enum Error<'a> {
    UnterminatedStringLiteral { at: panko_lex::Error<'a> },
}

impl<'a> From<panko_lex::Error<'a>> for Error<'a> {
    fn from(value: panko_lex::Error<'a>) -> Self {
        Self::UnterminatedStringLiteral { at: value }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TranslationUnit<'a> {
    decls: &'a [ExternalDeclaration<'a>],
}

impl TranslationUnit<'_> {
    pub fn as_sexpr(&self) -> String {
        format!(
            "(translation-unit\n{})",
            self.decls
                .iter()
                .map(|decl| decl.as_sexpr())
                .collect::<String>()
                .indent_lines()
                .trim_end(),
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ExternalDeclaration<'a> {
    Declaration(Declaration<'a>),
}

impl ExternalDeclaration<'_> {
    fn as_sexpr(&self) -> String {
        match self {
            ExternalDeclaration::Declaration(decl) => decl.as_sexpr(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Declaration<'a> {
    specifiers: &'a [DeclarationSpecifier<'a>],
    init_declarator_list: Option<()>,
}

impl Declaration<'_> {
    fn as_sexpr(&self) -> String {
        format!(
            "(declaration\n{}\n{})",
            self.specifiers
                .iter()
                .map(|s| format!("{s:?}"))
                .join("\n")
                .indent_lines()
                .trim_end(),
            self.init_declarator_list
                .map(|()| "()")
                .unwrap_or(NO_VALUE)
                .indent_lines()
                .trim_end(),
        )
    }
}

#[derive(Debug, Clone, Copy)]
enum DeclarationSpecifier<'a> {
    StorageClass(StorageClassSpecifier<'a>),
    TypeSpecifierQualifier(TypeSpecifierQualifier<'a>),
    FunctionSpecifier(FunctionSpecifier<'a>),
}

#[derive(Debug, Clone, Copy)]
struct StorageClassSpecifier<'a> {
    token: Token<'a>,
    kind: StorageClassSpecifierKind,
}

impl<'a> StorageClassSpecifier<'a> {
    fn new(token: Token<'a>) -> Self {
        Self {
            token,
            kind: storage_class_specifier_kind(token.kind),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum StorageClassSpecifierKind {
    Auto,
    Constexpr,
    Extern,
    Register,
    Static,
    ThreadLocal,
    Typedef,
}

fn storage_class_specifier_kind(token_kind: TokenKind) -> StorageClassSpecifierKind {
    match token_kind {
        TokenKind::Auto => StorageClassSpecifierKind::Auto,
        TokenKind::Constexpr => StorageClassSpecifierKind::Constexpr,
        TokenKind::Extern => StorageClassSpecifierKind::Extern,
        TokenKind::Register => StorageClassSpecifierKind::Register,
        TokenKind::Static => StorageClassSpecifierKind::Static,
        TokenKind::ThreadLocal => StorageClassSpecifierKind::ThreadLocal,
        TokenKind::Typedef => StorageClassSpecifierKind::Typedef,
        _ => unreachable!(),
    }
}

#[derive(Debug, Clone, Copy)]
enum TypeSpecifierQualifier<'a> {
    Qualifier(TypeQualifier<'a>),
    Specifier(TypeSpecifier<'a>),
}

#[derive(Debug, Clone, Copy)]
struct TypeQualifier<'a> {
    token: Token<'a>,
    kind: TypeQualifierKind,
}

impl<'a> TypeQualifier<'a> {
    fn new(token: Token<'a>) -> Self {
        Self {
            token,
            kind: type_qualifier_kind(token.kind),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum TypeQualifierKind {
    Const,
    Restrict,
    Volatile,
    Atomic,
}

fn type_qualifier_kind(token_kind: TokenKind) -> TypeQualifierKind {
    match token_kind {
        TokenKind::Const => TypeQualifierKind::Const,
        TokenKind::Restrict => TypeQualifierKind::Restrict,
        TokenKind::Volatile => TypeQualifierKind::Volatile,
        TokenKind::Atomic => TypeQualifierKind::Atomic,
        _ => unreachable!(),
    }
}

#[derive(Debug, Clone, Copy)]
struct TypeSpecifier<'a> {
    token: Token<'a>,
    kind: TypeSpecifierKind<'a>,
}

impl<'a> TypeSpecifier<'a> {
    fn new(token: Token<'a>) -> Self {
        Self {
            token,
            kind: type_specifier_kind(token.kind),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum TypeSpecifierKind<'a> {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    BitInt {
        open_paren: Token<'a>,
        size: u64,
        close_paren: Token<'a>,
    },
    Bool,
    Complex,
    Decimal32,
    Decimal64,
    Decimal128,
    // atomic-type-specifier
    // struct-or-union-specifier
    // enum-specifier
    // typedef-name
    // typeof-specifier
}

fn type_specifier_kind(token_kind: TokenKind) -> TypeSpecifierKind<'static> {
    match token_kind {
        TokenKind::Void => TypeSpecifierKind::Void,
        TokenKind::Char => TypeSpecifierKind::Char,
        TokenKind::Short => TypeSpecifierKind::Short,
        TokenKind::Int => TypeSpecifierKind::Int,
        TokenKind::Long => TypeSpecifierKind::Long,
        TokenKind::Float => TypeSpecifierKind::Float,
        TokenKind::Double => TypeSpecifierKind::Double,
        TokenKind::Signed => TypeSpecifierKind::Signed,
        TokenKind::Unsigned => TypeSpecifierKind::Unsigned,
        TokenKind::Bool => TypeSpecifierKind::Bool,
        TokenKind::Complex => TypeSpecifierKind::Complex,
        TokenKind::Decimal32 => TypeSpecifierKind::Decimal32,
        TokenKind::Decimal64 => TypeSpecifierKind::Decimal64,
        TokenKind::Decimal128 => TypeSpecifierKind::Decimal128,
        _ => unreachable!(),
    }
}

#[derive(Debug, Clone, Copy)]
struct FunctionSpecifier<'a> {
    token: Token<'a>,
    kind: FunctionSpecifierKind,
}

impl<'a> FunctionSpecifier<'a> {
    fn new(token: Token<'a>) -> Self {
        Self {
            token,
            kind: function_specifier_kind(token.kind),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum FunctionSpecifierKind {
    Inline,
    Noreturn,
}

fn function_specifier_kind(token_kind: TokenKind) -> FunctionSpecifierKind {
    match token_kind {
        TokenKind::Inline => FunctionSpecifierKind::Inline,
        TokenKind::Noreturn => FunctionSpecifierKind::Noreturn,
        _ => unreachable!(),
    }
}

pub fn parse<'a>(bump: &'a Bump, tokens: TokenIter<'a>) -> Result<TranslationUnit<'a>, Error<'a>> {
    let parser = grammar::TranslationUnitParser::new();
    Ok(parser
        .parse(bump, tokens.map(|token| Ok(token?)))
        .unwrap_or_else(|err| todo!("handle parse error: {err:?}")))
}
