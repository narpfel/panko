use lalrpop_util::lalrpop_mod;
use panko_lex::Loc;
use panko_lex::Token;
use panko_lex::TokenIter;
use panko_lex::TokenKind;

use crate::sexpr_builder::AsSExpr;
use crate::sexpr_builder::SExpr;

pub mod ast;
pub mod sexpr_builder;

lalrpop_mod!(grammar);

const SEXPR_INDENT: usize = 3;
const NO_VALUE: &str = "∅";

pub trait Report {
    fn print(&self);
    fn exit_code(&self) -> i32;
}

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
        SExpr::new("translation-unit").lines(self.decls)
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
    specifiers: DeclarationSpecifiers<'a>,
    init_declarator_list: &'a [InitDeclarator<'a>],
}

impl AsSExpr for Declaration<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("declaration")
            .inherit(&self.specifiers)
            .short_inline_explicit_empty(self.init_declarator_list)
    }
}

#[derive(Debug, Clone, Copy)]
struct DeclarationSpecifiers<'a>(&'a [DeclarationSpecifier<'a>]);

impl AsSExpr for DeclarationSpecifiers<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("declaration-specifiers").inherit_many_explicit_empty(self.0)
    }
}

impl<'a> DeclarationSpecifiers<'a> {
    fn loc(&self) -> Loc<'a> {
        self.0
            .first()
            .unwrap()
            .loc()
            .until(self.0.last().unwrap().loc())
    }
}

#[derive(Debug, Clone, Copy)]
enum DeclarationSpecifier<'a> {
    StorageClass(StorageClassSpecifier<'a>),
    TypeSpecifierQualifier(TypeSpecifierQualifier<'a>),
    FunctionSpecifier(FunctionSpecifier<'a>),
}

impl AsSExpr for DeclarationSpecifier<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            DeclarationSpecifier::StorageClass(storage_class) => storage_class.as_sexpr(),
            DeclarationSpecifier::TypeSpecifierQualifier(type_specifier_qualifier) =>
                type_specifier_qualifier.as_sexpr(),
            DeclarationSpecifier::FunctionSpecifier(function_specifier) =>
                function_specifier.as_sexpr(),
        }
    }
}

impl<'a> DeclarationSpecifier<'a> {
    fn loc(&self) -> Loc<'a> {
        match self {
            DeclarationSpecifier::StorageClass(storage_class) => storage_class.loc(),
            DeclarationSpecifier::TypeSpecifierQualifier(type_specifier_qualifier) =>
                type_specifier_qualifier.loc(),
            DeclarationSpecifier::FunctionSpecifier(function_specifier) => function_specifier.loc(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StorageClassSpecifier<'a> {
    token: Token<'a>,
    #[expect(unused)]
    kind: StorageClassSpecifierKind,
}

impl AsSExpr for StorageClassSpecifier<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::string(self.token.slice())
    }
}

impl<'a> StorageClassSpecifier<'a> {
    fn new(token: Token<'a>) -> Self {
        Self {
            token,
            kind: storage_class_specifier_kind(token.kind),
        }
    }

    fn loc(&self) -> Loc<'a> {
        self.token.loc()
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

impl AsSExpr for TypeSpecifierQualifier<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            TypeSpecifierQualifier::Qualifier(qualifier) => qualifier.as_sexpr(),
            TypeSpecifierQualifier::Specifier(specifier) => specifier.as_sexpr(),
        }
    }
}

impl<'a> TypeSpecifierQualifier<'a> {
    fn loc(&self) -> Loc<'a> {
        match self {
            TypeSpecifierQualifier::Qualifier(qualifier) => qualifier.loc(),
            TypeSpecifierQualifier::Specifier(specifier) => specifier.loc(),
        }
    }
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

    fn loc(&self) -> Loc<'a> {
        self.token.loc()
    }

    fn slice(&self) -> &'a str {
        self.token.slice()
    }
}

impl AsSExpr for TypeQualifier<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::string(self.token.slice())
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

    fn loc(&self) -> Loc<'a> {
        self.token.loc()
    }
}

impl AsSExpr for TypeSpecifier<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::string(self.token.slice())
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
    #[expect(unused)]
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
pub struct FunctionSpecifier<'a> {
    token: Token<'a>,
    #[expect(unused)]
    kind: FunctionSpecifierKind,
}

impl<'a> FunctionSpecifier<'a> {
    fn new(token: Token<'a>) -> Self {
        Self {
            token,
            kind: function_specifier_kind(token.kind),
        }
    }

    fn loc(&self) -> Loc<'a> {
        self.token.loc()
    }
}

impl AsSExpr for FunctionSpecifier<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::string(self.token.slice())
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
    initialiser: Option<Expression<'a>>,
}

impl AsSExpr for InitDeclarator<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("init-declarator")
            .inherit(&self.declarator)
            .inherit(&self.initialiser)
    }
}

#[derive(Debug, Clone, Copy)]
struct Declarator<'a> {
    pointers: Option<&'a [Pointer<'a>]>,
    direct_declarator: DirectDeclarator<'a>,
}

impl AsSExpr for Declarator<'_> {
    fn as_sexpr(&self) -> SExpr {
        match &self.pointers {
            None => self.direct_declarator.as_sexpr(),
            Some(pointers) => pointers.as_sexpr().inherit(&self.direct_declarator),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Pointer<'a> {
    qualifiers: &'a [TypeQualifier<'a>],
}

impl AsSExpr for Pointer<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("pointer").inherit_many(self.qualifiers)
    }
}

#[derive(Debug, Clone, Copy)]
enum DirectDeclarator<'a> {
    Abstract,
    Identifier(Token<'a>),
    Parenthesised(&'a Declarator<'a>),
    // ArrayDeclarator(ArrayDeclarator<'a>),
    FunctionDeclarator(FunctionDeclarator<'a>),
}

impl AsSExpr for DirectDeclarator<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            DirectDeclarator::Abstract => ().as_sexpr(),
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
        SExpr::new("function-declarator")
            .inherit(self.direct_declarator)
            .lines(self.parameter_type_list)
    }
}

#[derive(Debug, Clone, Copy)]
struct ParameterDeclaration<'a> {
    declaration_specifiers: DeclarationSpecifiers<'a>,
    declarator: Option<Declarator<'a>>,
}

impl AsSExpr for ParameterDeclaration<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("param")
            .inherit(&self.declaration_specifiers)
            .inherit(&self.declarator)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionDefinition<'a> {
    declaration_specifiers: DeclarationSpecifiers<'a>,
    declarator: Declarator<'a>,
    body: CompoundStatement<'a>,
}

impl AsSExpr for FunctionDefinition<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("function-definition")
            .inherit(&self.declaration_specifiers)
            .inherit(&self.declarator)
            .inherit(&self.body)
    }
}

#[derive(Debug, Clone, Copy)]
struct CompoundStatement<'a>(&'a [BlockItem<'a>]);

impl AsSExpr for CompoundStatement<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("compound-statement").lines(self.0)
    }
}

#[derive(Debug, Clone, Copy)]
enum BlockItem<'a> {
    Declaration(Declaration<'a>),
    UnlabeledStatement(UnlabeledStatement<'a>),
}

impl AsSExpr for BlockItem<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            BlockItem::Declaration(decl) => decl.as_sexpr(),
            BlockItem::UnlabeledStatement(unlabeled_statement) => unlabeled_statement.as_sexpr(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum UnlabeledStatement<'a> {
    ExpressionStatement(ExpressionStatement<'a>),
    PrimaryBlock(PrimaryBlock<'a>),
    JumpStatement(JumpStatement<'a>),
}

impl AsSExpr for UnlabeledStatement<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            UnlabeledStatement::ExpressionStatement(stmt) => stmt.as_sexpr(),
            UnlabeledStatement::PrimaryBlock(block) => block.as_sexpr(),
            UnlabeledStatement::JumpStatement(jump) => jump.as_sexpr(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct ExpressionStatement<'a>(Option<Expression<'a>>);

impl AsSExpr for ExpressionStatement<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("expression").inherit(&self.0)
    }
}

#[derive(Debug, Clone, Copy)]
enum PrimaryBlock<'a> {
    CompoundStatement(CompoundStatement<'a>),
}

impl AsSExpr for PrimaryBlock<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            PrimaryBlock::CompoundStatement(stmt) => SExpr::new("primary-block").inherit(stmt),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum JumpStatement<'a> {
    Return(Option<Expression<'a>>),
}

impl AsSExpr for JumpStatement<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            JumpStatement::Return(expr) => SExpr::new("return").inherit(expr),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Expression<'a> {
    Name(Token<'a>),
    Integer(Token<'a>),
}

impl AsSExpr for Expression<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            Expression::Name(name) => SExpr::new("name").inline_string(name.slice().to_owned()),
            Expression::Integer(int) => SExpr::string(int.slice()),
        }
    }
}

pub fn parse<'a>(
    sess: &'a ast::Session<'a>,
    tokens: TokenIter<'a>,
) -> Result<ast::TranslationUnit<'a>, Error<'a>> {
    let parser = grammar::TranslationUnitParser::new();
    let parse_tree = parser
        .parse(sess.bump, tokens.map(|token| Ok(token?)))
        .unwrap_or_else(|err| todo!("handle parse error: {err:?}"));
    Ok(ast::from_parse_tree(sess, parse_tree))
}
