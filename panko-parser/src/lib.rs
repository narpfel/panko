// FIXME: everything is incomplete, so there are a lot of unused things
#![expect(unused)]

use bumpalo::Bump;
use itertools::Itertools as _;
use lalrpop_util::lalrpop_mod;
use panko_lex::Token;
use panko_lex::TokenIter;
use panko_lex::TokenKind;

use crate::sexpr_builder::AsSExpr;
use crate::sexpr_builder::Param;
use crate::sexpr_builder::SExpr;

pub mod sexpr_builder;

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

impl AsSExpr for TranslationUnit<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr {
            name: "translation-unit".to_owned(),
            params: self
                .decls
                .iter()
                .map(|decl| Param::Line(Box::new(*decl)))
                .collect(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ExternalDeclaration<'a> {
    FunctionDefinition(FunctionDefinition<'a>),
    Declaration(Declaration<'a>),
}

impl AsSExpr for ExternalDeclaration<'_> {
    fn as_sexpr(&self) -> SExpr {
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

impl AsSExpr for Declaration<'_> {
    fn as_sexpr(&self) -> SExpr {
        let mut params = vec![];
        if self.specifiers.is_empty() {
            params.push(Param::Inherit(Box::new(None::<Self>)));
        }
        else {
            params.extend(
                self.specifiers
                    .iter()
                    .map(|specifier| Param::String(format!("{specifier:?}"))),
            );
        }

        let param = if self.init_declarator_list.len() > 1 {
            Param::Line
        }
        else {
            Param::Inherit
        };
        if self.init_declarator_list.is_empty() {
            params.push(Param::Inherit(Box::new(None::<Self>)));
        }
        else {
            params.extend(
                self.init_declarator_list
                    .iter()
                    .map(|decl| param(Box::new(*decl))),
            );
        }

        SExpr { name: "declaration".to_owned(), params }
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

impl AsSExpr for InitDeclarator<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr {
            name: "init-declarator".to_owned(),
            params: vec![
                Param::Inherit(Box::new(self.declarator)),
                Param::Inherit(Box::new(self.initialiser)),
            ],
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Declarator<'a> {
    pointers: Option<&'a [Pointer<'a>]>,
    direct_declarator: DirectDeclarator<'a>,
}

impl AsSExpr for Declarator<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self.pointers {
            None => self.direct_declarator.as_sexpr(),
            Some(pointers) => SExpr {
                name: "pointers".to_owned(),
                params: vec![
                    Param::InlineString(format!("level={}", pointers.len())),
                    Param::Inherit(Box::new(self.direct_declarator)),
                ],
            },
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

impl AsSExpr for DirectDeclarator<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            DirectDeclarator::Identifier(ident) => SExpr::string(ident.slice()),
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

impl AsSExpr for FunctionDeclarator<'_> {
    fn as_sexpr(&self) -> SExpr {
        let mut params = vec![Param::Inherit(Box::new(*self.direct_declarator))];
        params.extend(
            self.parameter_type_list
                .iter()
                .map(|param| Param::Line(Box::new(*param))),
        );

        SExpr {
            name: "function-declarator".to_owned(),
            params,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct ParameterDeclaration<'a> {
    declaration_specifiers: &'a [DeclarationSpecifier<'a>],
    declarator: Declarator<'a>,
}

impl AsSExpr for ParameterDeclaration<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr {
            name: "param".to_owned(),
            params: vec![Param::Inherit(Box::new(self.declarator))],
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Initialiser<'a>(Token<'a>);

impl AsSExpr for Initialiser<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::string(self.0.slice())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionDefinition<'a> {
    declaration_specifiers: &'a [DeclarationSpecifier<'a>],
    declarator: Declarator<'a>,
    body: CompoundStatement<'a>,
}

impl AsSExpr for FunctionDefinition<'_> {
    fn as_sexpr(&self) -> SExpr {
        let mut params = self
            .declaration_specifiers
            .iter()
            .map(|declaration_specifier| Param::String(format!("{declaration_specifier:?}")))
            .collect_vec();
        params.push(Param::Inherit(Box::new(self.declarator)));
        params.push(Param::Line(Box::new(self.body)));
        SExpr {
            name: "function-definition".to_owned(),
            params,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct CompoundStatement<'a>(&'a [BlockItem<'a>]);

impl AsSExpr for CompoundStatement<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr {
            name: "compound-statement".to_owned(),
            params: self
                .0
                .iter()
                .map(|item| Param::Line(Box::new(*item)))
                .collect(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum BlockItem<'a> {
    Declaration(Declaration<'a>),
}

impl AsSExpr for BlockItem<'_> {
    fn as_sexpr(&self) -> SExpr {
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
