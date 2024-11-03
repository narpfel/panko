#![feature(coverage_attribute)]

use ariadne::Color::Red;
use ast::Arithmetic;
use ast::Integral;
use ast::IntegralKind;
use ast::Signedness;
use ast::Type;
use itertools::Itertools as _;
use lalrpop_util::lalrpop_mod;
use lalrpop_util::ParseError;
use panko_lex::Loc;
use panko_lex::Token;
use panko_lex::TokenIter;
use panko_lex::TokenKind;
use panko_report::Report;

use crate::ast::QualifiedType;

mod as_sexpr;
pub mod ast;
pub mod sexpr_builder;

lalrpop_mod!(grammar);

const SEXPR_INDENT: usize = 3;
pub const NO_VALUE: &str = "∅";

#[derive(Debug, Report)]
#[exit_code(1)]
enum Error<'a> {
    #[error("unterminated string literal")]
    UnterminatedStringLiteral { at: panko_lex::Error<'a> },
    #[error("unrecognised token")]
    #[diagnostics(at(colour = Red, label = "expected one of the following token kinds: {expected}"))]
    UnrecognisedToken { at: Token<'a>, expected: Strings },
}

impl<'a> From<panko_lex::Error<'a>> for Error<'a> {
    fn from(value: panko_lex::Error<'a>) -> Self {
        Self::UnterminatedStringLiteral { at: value }
    }
}

#[derive(Debug)]
struct Strings(Vec<String>);

impl Strings {
    fn slice(&self) -> String {
        format!("[{}]", self.0.iter().join(", "))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TranslationUnit<'a> {
    decls: &'a [ExternalDeclaration<'a>],
}

#[derive(Debug, Clone, Copy)]
pub enum ExternalDeclaration<'a> {
    FunctionDefinition(FunctionDefinition<'a>),
    Declaration(Declaration<'a>),
}

#[derive(Debug, Clone, Copy)]
pub struct Declaration<'a> {
    specifiers: DeclarationSpecifiers<'a>,
    init_declarator_list: &'a [InitDeclarator<'a>],
}

#[derive(Debug, Clone, Copy)]
struct DeclarationSpecifiers<'a>(&'a [DeclarationSpecifier<'a>]);

impl<'a> DeclarationSpecifiers<'a> {
    fn loc(&self) -> Loc<'a> {
        match self.0 {
            [] => unreachable!(),
            [specifier] => specifier.loc(),
            [first, .., last] => first.loc().until(last.loc()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum DeclarationSpecifier<'a> {
    StorageClass(StorageClassSpecifier<'a>),
    TypeSpecifierQualifier(TypeSpecifierQualifier<'a>),
    FunctionSpecifier(FunctionSpecifier<'a>),
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

    fn parse(&self, _sess: &ast::Session<'a>, ty: &mut Option<Type<'_>>) {
        match self.kind {
            TypeSpecifierKind::Short => match ty {
                None =>
                    *ty = Some(Type::Arithmetic(Arithmetic::Integral(Integral {
                        signedness: Signedness::Signed,
                        kind: IntegralKind::Short,
                    }))),
                Some(Type::Arithmetic(Arithmetic::Integral(Integral {
                    signedness,
                    kind: IntegralKind::Int,
                }))) =>
                    *ty = Some(Type::Arithmetic(Arithmetic::Integral(Integral {
                        signedness: *signedness,
                        kind: IntegralKind::Short,
                    }))),
                _ => todo!(),
            },
            // TODO: only allow at most one `int` per type
            TypeSpecifierKind::Int => match ty {
                Some(Type::Arithmetic(Arithmetic::Integral(Integral {
                    signedness: _,
                    kind:
                        IntegralKind::Short
                        | IntegralKind::Int
                        | IntegralKind::Long
                        | IntegralKind::LongLong,
                }))) => (),
                Some(_) => todo!(),
                None =>
                    *ty = Some(Type::Arithmetic(Arithmetic::Integral(Integral {
                        signedness: Signedness::Signed,
                        kind: IntegralKind::Int,
                    }))),
            },
            TypeSpecifierKind::Long => match ty {
                None =>
                    *ty = Some(Type::Arithmetic(Arithmetic::Integral(Integral {
                        signedness: Signedness::Signed,
                        kind: IntegralKind::Long,
                    }))),
                Some(Type::Arithmetic(Arithmetic::Integral(Integral {
                    signedness,
                    kind: IntegralKind::Int,
                }))) =>
                    *ty = Some(Type::Arithmetic(Arithmetic::Integral(Integral {
                        signedness: *signedness,
                        kind: IntegralKind::Long,
                    }))),
                Some(Type::Arithmetic(Arithmetic::Integral(Integral {
                    signedness,
                    kind: IntegralKind::Long,
                }))) =>
                    *ty = Some(Type::Arithmetic(Arithmetic::Integral(Integral {
                        signedness: *signedness,
                        kind: IntegralKind::LongLong,
                    }))),
                _ => todo!(),
            },
            TypeSpecifierKind::Char => match ty {
                Some(_) => todo!(),
                None =>
                    *ty = Some(Type::Arithmetic(Arithmetic::Integral(Integral {
                        signedness: Signedness::Signed,
                        kind: IntegralKind::PlainChar,
                    }))),
            },
            TypeSpecifierKind::Void => match ty {
                Some(_) => todo!(),
                None => *ty = Some(Type::Void),
            },
            TypeSpecifierKind::Signed => match ty {
                Some(Type::Arithmetic(Arithmetic::Integral(Integral {
                    signedness: _,
                    kind: IntegralKind::PlainChar,
                }))) =>
                    *ty = Some(Type::Arithmetic(Arithmetic::Integral(Integral {
                        signedness: Signedness::Signed,
                        kind: IntegralKind::Char,
                    }))),
                Some(Type::Arithmetic(Arithmetic::Integral(
                    integral @ Integral { signedness: Signedness::Signed, kind: _ },
                ))) => integral.signedness = Signedness::Signed,
                None =>
                    *ty = Some(Type::Arithmetic(Arithmetic::Integral(Integral {
                        signedness: Signedness::Signed,
                        kind: IntegralKind::Int,
                    }))),
                _ => todo!(),
            },
            TypeSpecifierKind::Unsigned => match ty {
                Some(Type::Arithmetic(Arithmetic::Integral(Integral {
                    signedness: _,
                    kind: IntegralKind::PlainChar,
                }))) =>
                    *ty = Some(Type::Arithmetic(Arithmetic::Integral(Integral {
                        signedness: Signedness::Unsigned,
                        kind: IntegralKind::Char,
                    }))),
                Some(Type::Arithmetic(Arithmetic::Integral(
                    integral @ Integral { signedness: Signedness::Signed, kind: _ },
                ))) => integral.signedness = Signedness::Unsigned,
                None =>
                    *ty = Some(Type::Arithmetic(Arithmetic::Integral(Integral {
                        signedness: Signedness::Unsigned,
                        kind: IntegralKind::Int,
                    }))),
                _ => todo!(),
            },
            TypeSpecifierKind::Bool => todo!(),
            _ => todo!("{self:#?}"),
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

#[derive(Debug, Clone, Copy)]
struct Declarator<'a> {
    pointers: Option<&'a [Pointer<'a>]>,
    direct_declarator: DirectDeclarator<'a>,
}

#[derive(Debug, Clone, Copy)]
struct Pointer<'a> {
    star: Token<'a>,
    qualifiers: &'a [TypeQualifier<'a>],
}

#[derive(Debug, Clone, Copy)]
enum DirectDeclarator<'a> {
    Abstract,
    Identifier(Token<'a>),
    Parenthesised(&'a Declarator<'a>),
    // ArrayDeclarator(ArrayDeclarator<'a>),
    FunctionDeclarator(FunctionDeclarator<'a>),
}

#[derive(Debug, Clone, Copy)]
struct FunctionDeclarator<'a> {
    direct_declarator: &'a DirectDeclarator<'a>,
    parameter_type_list: ParameterTypeList<'a>,
    close_paren: Token<'a>,
}

#[derive(Debug, Clone, Copy)]
struct ParameterTypeList<'a> {
    parameter_list: &'a [ParameterDeclaration<'a>],
    is_varargs: bool,
}

#[derive(Debug, Clone, Copy)]
struct ParameterDeclaration<'a> {
    declaration_specifiers: DeclarationSpecifiers<'a>,
    declarator: Option<Declarator<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionDefinition<'a> {
    declaration_specifiers: DeclarationSpecifiers<'a>,
    declarator: Declarator<'a>,
    body: CompoundStatement<'a>,
}

#[derive(Debug, Clone, Copy)]
struct CompoundStatement<'a>(&'a [BlockItem<'a>]);

#[derive(Debug, Clone, Copy)]
enum BlockItem<'a> {
    Declaration(Declaration<'a>),
    UnlabeledStatement(UnlabeledStatement<'a>),
}

#[derive(Debug, Clone, Copy)]
enum UnlabeledStatement<'a> {
    ExpressionStatement(ExpressionStatement<'a>),
    PrimaryBlock(PrimaryBlock<'a>),
    JumpStatement(JumpStatement<'a>),
}

#[derive(Debug, Clone, Copy)]
struct ExpressionStatement<'a>(Option<Expression<'a>>);

#[derive(Debug, Clone, Copy)]
enum PrimaryBlock<'a> {
    CompoundStatement(CompoundStatement<'a>),
}

#[derive(Debug, Clone, Copy)]
enum JumpStatement<'a> {
    Return {
        return_: Token<'a>,
        expr: Option<Expression<'a>>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum Expression<'a> {
    Name(Token<'a>),
    Integer(Token<'a>),
    Parenthesised {
        open_paren: Token<'a>,
        expr: &'a Expression<'a>,
        close_paren: Token<'a>,
    },
    Assign {
        target: &'a Expression<'a>,
        value: &'a Expression<'a>,
    },
    CompoundAssign {
        target: &'a Expression<'a>,
        op: BinOp<'a>,
        value: &'a Expression<'a>,
    },
    BinOp {
        lhs: &'a Expression<'a>,
        op: BinOp<'a>,
        rhs: &'a Expression<'a>,
    },
    UnaryOp {
        operator: UnaryOp<'a>,
        operand: &'a Expression<'a>,
    },
    Call {
        callee: &'a Expression<'a>,
        args: &'a [Expression<'a>],
        close_paren: Token<'a>,
    },
    Sizeof {
        sizeof: Token<'a>,
        ty: QualifiedType<'a>,
        close_paren: Token<'a>,
    },
    Alignof {
        alignof: Token<'a>,
        ty: QualifiedType<'a>,
        close_paren: Token<'a>,
    },
    Cast {
        open_paren: Token<'a>,
        ty: QualifiedType<'a>,
        expr: &'a Expression<'a>,
    },
    Subscript {
        lhs: &'a Expression<'a>,
        rhs: &'a Expression<'a>,
        close_bracket: Token<'a>,
    },
    Generic {
        generic: Token<'a>,
        selector: &'a Expression<'a>,
        assocs: GenericAssocList<'a>,
        close_paren: Token<'a>,
    },
    Logical {
        lhs: &'a Expression<'a>,
        op: LogicalOp<'a>,
        rhs: &'a Expression<'a>,
    },
    Conditional {
        condition: &'a Expression<'a>,
        then: &'a Expression<'a>,
        or_else: &'a Expression<'a>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum BinOpKind {
    Multiply,
    Divide,
    Modulo,
    Add,
    Subtract,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    LeftShift,
    RightShift,
    BitAnd,
    BitXor,
    BitOr,
}

impl BinOpKind {
    fn str(self) -> &'static str {
        match self {
            BinOpKind::Multiply => "multiply",
            BinOpKind::Divide => "divide",
            BinOpKind::Modulo => "modulo",
            BinOpKind::Add => "add",
            BinOpKind::Subtract => "subtract",
            BinOpKind::Equal => "equal",
            BinOpKind::NotEqual => "not-equal",
            BinOpKind::Less => "less",
            BinOpKind::LessEqual => "less-equal",
            BinOpKind::Greater => "greater",
            BinOpKind::GreaterEqual => "greater-equal",
            BinOpKind::LeftShift => "left-shift",
            BinOpKind::RightShift => "right-shift",
            BinOpKind::BitAnd => "bit-and",
            BinOpKind::BitXor => "bit-xor",
            BinOpKind::BitOr => "bit-or",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BinOp<'a> {
    pub kind: BinOpKind,
    pub token: Token<'a>,
}

impl BinOp<'_> {
    pub fn str(&self) -> &'static str {
        self.kind.str()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct UnaryOp<'a> {
    pub kind: UnaryOpKind,
    pub token: Token<'a>,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOpKind {
    Addressof,
    Deref,
    Plus,
    Negate,
    Compl,
    Not,
    Sizeof,
}

impl<'a> UnaryOp<'a> {
    pub fn str(&self) -> &'static str {
        match self.kind {
            UnaryOpKind::Addressof => "addressof",
            UnaryOpKind::Deref => "deref",
            UnaryOpKind::Plus => "plus",
            UnaryOpKind::Negate => "negate",
            UnaryOpKind::Compl => "compl",
            UnaryOpKind::Not => "not",
            UnaryOpKind::Sizeof => "sizeof",
        }
    }

    pub fn loc(&self) -> Loc<'a> {
        self.token.loc()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct GenericAssocList<'a>(pub &'a [GenericAssociation<'a>]);

#[derive(Debug, Clone, Copy)]
pub enum GenericAssociation<'a> {
    Ty {
        ty: QualifiedType<'a>,
        expr: Expression<'a>,
    },
    Default {
        default: Token<'a>,
        expr: Expression<'a>,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct LogicalOp<'a> {
    kind: LogicalOpKind,
    token: Token<'a>,
}

#[derive(Debug, Clone, Copy)]
pub enum LogicalOpKind {
    And,
    Or,
}

impl<'a> LogicalOp<'a> {
    pub fn kind(&self) -> LogicalOpKind {
        self.kind
    }

    pub fn str(&self) -> &'static str {
        match self.kind {
            LogicalOpKind::And => "and",
            LogicalOpKind::Or => "or",
        }
    }

    pub fn loc(&self) -> Loc<'a> {
        self.token.loc()
    }
}

pub fn parse<'a>(
    sess: &'a ast::Session<'a>,
    tokens: TokenIter<'a>,
) -> Result<ast::TranslationUnit<'a>, Box<dyn Report + 'a>> {
    let parser = grammar::TranslationUnitParser::new();
    let parse_tree =
        parser
            .parse(sess, tokens.map(|token| Ok(token?)))
            .map_err(|err| match err {
                ParseError::UnrecognizedToken { token, expected } =>
                    Error::UnrecognisedToken { at: token.1, expected: Strings(expected) },
                err => todo!("{err:#?}"),
            })?;
    Ok(ast::from_parse_tree(sess, parse_tree))
}
