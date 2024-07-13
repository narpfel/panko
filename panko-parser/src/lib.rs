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
                .join("\n")
                .indent_lines()
                .trim_end(),
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ExternalDeclaration<'a> {
    FunctionDefinition(FunctionDefinition<'a>),
    Declaration(Declaration<'a>),
}

impl ExternalDeclaration<'_> {
    fn as_sexpr(&self) -> String {
        match self {
            ExternalDeclaration::FunctionDefinition(def) => def.as_sexpr(),
            ExternalDeclaration::Declaration(decl) => decl.as_sexpr(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Declaration<'a> {
    specifiers: &'a [DeclarationSpecifier<'a>],
    init_declarator_list: &'a [InitDeclarator<'a>],
}

impl Declaration<'_> {
    fn as_sexpr(&self) -> String {
        format!(
            "(declaration\n{}\n{})",
            if self.specifiers.is_empty() {
                NO_VALUE.to_owned()
            }
            else {
                self.specifiers.iter().map(|s| format!("{s:?}")).join("\n")
            }
            .indent_lines()
            .trim_end(),
            if self.init_declarator_list.is_empty() {
                NO_VALUE.to_owned()
            }
            else {
                self.init_declarator_list
                    .iter()
                    .map(|init_declarator| init_declarator.as_sexpr())
                    .join("\n")
            }
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

#[derive(Debug, Clone, Copy)]
struct InitDeclarator<'a> {
    declarator: Declarator<'a>,
    initialiser: Option<Initialiser<'a>>,
}

impl InitDeclarator<'_> {
    fn as_sexpr(&self) -> String {
        format!(
            "(init-declarator {} {})",
            self.declarator.as_sexpr(),
            self.initialiser
                .map(|initialiser| initialiser.as_sexpr())
                .unwrap_or_else(|| NO_VALUE.to_owned())
        )
    }
}

#[derive(Debug, Clone, Copy)]
struct Declarator<'a> {
    pointers: Option<&'a [Pointer<'a>]>,
    direct_declarator: DirectDeclarator<'a>,
}

impl Declarator<'_> {
    fn as_sexpr(&self) -> String {
        match self.pointers {
            None => self.direct_declarator.as_sexpr(),
            Some(pointers) => format!(
                "(pointer level={} {})",
                pointers.len(),
                self.direct_declarator.as_sexpr()
            ),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Pointer<'a> {
    qualifiers: &'a [TypeQualifier<'a>],
}

#[derive(Debug, Clone, Copy)]
enum DirectDeclarator<'a> {
    Identifier(Token<'a>),
    Parenthesised(&'a Declarator<'a>),
    // ArrayDeclarator(ArrayDeclarator<'a>),
    FunctionDeclarator(FunctionDeclarator<'a>),
}

impl DirectDeclarator<'_> {
    fn as_sexpr(&self) -> String {
        match self {
            DirectDeclarator::Identifier(ident) => ident.slice().to_owned(),
            DirectDeclarator::Parenthesised(declarator) => declarator.as_sexpr(),
            DirectDeclarator::FunctionDeclarator(function_declarator) =>
                function_declarator.as_sexpr(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct FunctionDeclarator<'a> {
    direct_declarator: &'a DirectDeclarator<'a>,
    parameter_type_list: &'a [ParameterDeclaration<'a>],
}

impl FunctionDeclarator<'_> {
    fn as_sexpr(&self) -> String {
        format!(
            "(function-declarator {}\n{})",
            self.direct_declarator.as_sexpr(),
            self.parameter_type_list
                .iter()
                .map(|param| param.as_sexpr())
                .join("\n")
                .indent_lines()
                .trim_end(),
        )
    }
}

#[derive(Debug, Clone, Copy)]
struct ParameterDeclaration<'a> {
    declaration_specifiers: &'a [DeclarationSpecifier<'a>],
    declarator: Declarator<'a>,
}

impl ParameterDeclaration<'_> {
    fn as_sexpr(&self) -> String {
        format!("(param {})", self.declarator.as_sexpr())
    }
}

#[derive(Debug, Clone, Copy)]
struct Initialiser<'a>(Token<'a>);

impl Initialiser<'_> {
    fn as_sexpr(&self) -> String {
        self.0.slice().to_owned()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionDefinition<'a> {
    declaration_specifiers: &'a [DeclarationSpecifier<'a>],
    declarator: Declarator<'a>,
    body: CompoundStatement<'a>,
}

impl FunctionDefinition<'_> {
    fn as_sexpr(&self) -> String {
        format!(
            "(function-definition\n{}\n{}\n{})",
            self.declarator.as_sexpr().indent_lines().trim_end(),
            self.declaration_specifiers
                .iter()
                .map(|declaration_specifier| format!("{declaration_specifier:?}"))
                .join("\n")
                .indent_lines()
                .trim_end(),
            self.body.as_sexpr().indent_lines().trim_end(),
        )
    }
}

#[derive(Debug, Clone, Copy)]
struct CompoundStatement<'a>(&'a [BlockItem<'a>]);

impl CompoundStatement<'_> {
    fn as_sexpr(&self) -> String {
        if self.0.is_empty() {
            "(compound-statement)".to_owned()
        }
        else {
            let items = self.0.iter().map(|item| item.as_sexpr()).join("\n");
            format!("(compound-statement\n{})", items.indent_lines().trim_end())
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum BlockItem<'a> {
    Declaration(Declaration<'a>),
}

impl BlockItem<'_> {
    fn as_sexpr(&self) -> String {
        match self {
            BlockItem::Declaration(decl) => decl.as_sexpr(),
        }
    }
}

pub fn parse<'a>(bump: &'a Bump, tokens: TokenIter<'a>) -> Result<TranslationUnit<'a>, Error<'a>> {
    let parser = grammar::TranslationUnitParser::new();
    Ok(parser
        .parse(bump, tokens.map(|token| Ok(token?)))
        .unwrap_or_else(|err| todo!("handle parse error: {err:?}")))
}
