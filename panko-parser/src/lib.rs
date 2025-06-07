#![feature(assert_matches)]
#![feature(coverage_attribute)]
#![feature(gen_blocks)]
#![feature(if_let_guard)]
#![feature(never_type)]
#![feature(try_blocks)]
#![feature(type_alias_impl_trait)]
#![feature(unqualified_local_imports)]

use std::cell::Cell;
use std::cell::RefCell;
use std::iter::empty;

use ariadne::Color::Blue;
use ariadne::Color::Red;
use ariadne::Fmt as _;
use itertools::Either;
use itertools::Itertools as _;
use lalrpop_util::ParseError;
use lalrpop_util::lalrpop_mod;
use panko_lex::LexerHacked;
use panko_lex::Loc;
use panko_lex::Token;
use panko_lex::TokenKind;
use panko_lex::TypedefNames;
use panko_report::Report;
use panko_report::Sliced as _;

use crate::ast::FromError;
use crate::ast::IntegralKind;
use crate::ast::ParsedSpecifiers;
use crate::ast::QualifiedType;
use crate::ast::Signedness;
pub use crate::preprocess::preprocess;

mod as_sexpr;
pub mod ast;
pub mod preprocess;
pub mod sexpr_builder;

lalrpop_mod!(
    #[allow(clippy::as_conversions, reason = "lalrpop generates `as` conversions")]
    #[allow(unused_qualifications)]
    grammar
);

const SEXPR_INDENT: usize = 3;
pub const NO_VALUE: &str = "∅";

#[derive(Debug, Clone, Copy, Report)]
#[exit_code(1)]
enum Error<'a> {
    #[error("lexer error")]
    #[diagnostics(at(colour = Red, label = "lexer failed here"))]
    LexerFailed { at: Loc<'a> },

    #[error("unterminated block comment")]
    #[diagnostics(at(colour = Red, label = "comment opened here"))]
    UnterminatedBlockComment { at: Loc<'a> },

    #[error("unterminated string literal")]
    #[diagnostics(at(colour = Red))]
    UnterminatedStringLiteral { at: Loc<'a> },

    #[error("unterminated character constant")]
    #[diagnostics(at(colour = Red))]
    UnterminatedCharConstant { at: Loc<'a> },

    #[error("unexpected token `{slice}` of kind `{kind}`")]
    #[diagnostics(at(colour = Red, label = "expected one of the following token kinds: {expected}"))]
    #[with(
        slice = at.slice().escape_debug(),
        kind = format!("{:?}", at.kind).fg(Red),
    )]
    UnexpectedToken {
        at: Token<'a>,
        expected: Strings<'a>,
    },
}

impl<'a> Error<'a> {
    fn from_lexer_error(kind: panko_lex::ErrorKind, at: Loc<'a>) -> Self {
        match kind {
            panko_lex::ErrorKind::UnterminatedBlockComment => Self::UnterminatedBlockComment { at },
            panko_lex::ErrorKind::Other => Self::LexerFailed { at },
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Strings<'a>(&'a [&'a str]);

impl Strings<'_> {
    fn slice(&self) -> String {
        format!("[{}]", self.0.iter().join(", "))
    }
}

#[derive(Debug, Report)]
#[exit_code(1)]
enum Diagnostic<'a> {
    #[error("{message}")]
    #[diagnostics(
        specifiers(colour = Blue, label = "previous type"),
        at(colour = Red, label = "cannot combine `{token}` with previous type `{ty}`"),
    )]
    #[with(
        token = at.token,
        ty = ty.into_type(|| unreachable!()).fg(Blue),
    )]
    InvalidTypeSpecifierCombination {
        message: &'a str,
        at: TypeSpecifier<'a>,
        specifiers: DeclarationSpecifiers<'a>,
        ty: ParsedSpecifiers<'a>,
    },
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
    semi: Token<'a>,
}

impl<'a> Declaration<'a> {
    fn loc(&self) -> Loc<'a> {
        let Self {
            specifiers,
            init_declarator_list: _,
            semi,
        } = self;
        specifiers.loc().until(semi.loc())
    }
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

    fn is_typedef(&self) -> bool {
        self.0.iter().any(|specifier| {
            matches!(
                specifier,
                DeclarationSpecifier::StorageClass(StorageClassSpecifier {
                    token: _,
                    kind: StorageClassSpecifierKind::Typedef
                })
            )
        })
    }

    fn until(self, end: usize) -> Self {
        let Self(specifiers) = self;
        Self(&specifiers[..end])
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
    pub token: Token<'a>,
    pub kind: StorageClassSpecifierKind,
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
pub enum StorageClassSpecifierKind {
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

    fn parse(
        &self,
        sess: &ast::Session<'a>,
        specifiers: DeclarationSpecifiers<'a>,
        position: usize,
        ty: ParsedSpecifiers<'a>,
    ) -> ParsedSpecifiers<'a> {
        type Kind<'a> = TypeSpecifierKind<'a>;
        type Parsed<'a> = ParsedSpecifiers<'a>;

        let error_full = |message: &'a str| {
            let () = sess.emit(Diagnostic::InvalidTypeSpecifierCombination {
                message,
                at: *self,
                specifiers: specifiers.until(position),
                ty,
            });
            ty
        };

        let error = || error_full("invalid combination of type specifiers");

        match self.kind {
            Kind::Void => match ty {
                Parsed::None => Parsed::Void,
                _ => error(),
            },
            Kind::Char => match ty {
                Parsed::None => Parsed::Char(None),
                Parsed::Int { kind: None, signedness } => Parsed::Char(signedness),
                _ => error(),
            },
            Kind::Short => match ty {
                Parsed::None => Parsed::Int {
                    kind: Some(IntegralKind::Short),
                    signedness: None,
                },
                Parsed::Int {
                    kind: None | Some(IntegralKind::Int),
                    signedness,
                } => Parsed::Int {
                    kind: Some(IntegralKind::Short),
                    signedness,
                },
                _ => error(),
            },
            Kind::Int => match ty {
                Parsed::None => Parsed::Int {
                    kind: Some(IntegralKind::Int),
                    signedness: None,
                },
                Parsed::Int { kind: None, signedness } => Parsed::Int {
                    kind: Some(IntegralKind::Int),
                    signedness,
                },
                ty @ Parsed::Int {
                    kind: Some(IntegralKind::Short | IntegralKind::Long | IntegralKind::LongLong),
                    signedness: _,
                } => ty,
                _ => error(),
            },
            Kind::Long => match ty {
                Parsed::None => Parsed::Int {
                    kind: Some(IntegralKind::Long),
                    signedness: None,
                },
                Parsed::Int {
                    kind: None | Some(IntegralKind::Int),
                    signedness,
                } => Parsed::Int {
                    kind: Some(IntegralKind::Long),
                    signedness,
                },
                Parsed::Int {
                    kind: Some(IntegralKind::Long),
                    signedness,
                } => Parsed::Int {
                    kind: Some(IntegralKind::LongLong),
                    signedness,
                },
                Parsed::Int {
                    kind: Some(IntegralKind::LongLong),
                    signedness: _,
                } => error_full(sess.alloc_str(&format!(
                    "`{longlonglong}` is too long for {panko}",
                    longlonglong = "long long long".fg(Red),
                    panko = yansi::Paint::bold("panko").fg(ariadne::Color::Rgb(0xaa, 0x22, 0xff)),
                ))),
                _ => error(),
            },
            Kind::Signed => match ty {
                Parsed::None => Parsed::Int {
                    kind: None,
                    signedness: Some(Signedness::Signed),
                },
                Parsed::Char(None) => Parsed::Char(Some(Signedness::Signed)),
                Parsed::Int { kind, signedness: None } => Parsed::Int {
                    kind,
                    signedness: Some(Signedness::Signed),
                },
                _ => error(),
            },
            Kind::Unsigned => match ty {
                Parsed::None => Parsed::Int {
                    kind: None,
                    signedness: Some(Signedness::Unsigned),
                },
                Parsed::Char(None) => Parsed::Char(Some(Signedness::Unsigned)),
                Parsed::Int { kind, signedness: None } => Parsed::Int {
                    kind,
                    signedness: Some(Signedness::Unsigned),
                },
                _ => error(),
            },
            Kind::TypedefName => match ty {
                Parsed::None => Parsed::Typedef(self.token),
                _ => error(),
            },
            _ => todo!("unimplemented type specifier: {self:#?}"),
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
    TypedefName,
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
        TokenKind::TypeIdentifier => TypeSpecifierKind::TypedefName,
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
    initialiser: Option<Initialiser<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub enum Initialiser<'a> {
    Braced {
        open_brace: Token<'a>,
        initialiser_list: &'a [DesignatedInitialiser<'a>],
        close_brace: Token<'a>,
    },
    Expression(Expression<'a>),
}

#[derive(Debug, Clone, Copy)]
pub struct DesignatedInitialiser<'a> {
    pub designation: Option<Designation<'a>>,
    pub initialiser: &'a Initialiser<'a>,
}

#[derive(Debug, Clone, Copy)]
pub struct Designation<'a>(pub &'a [Designator<'a>]);

#[derive(Debug, Clone, Copy)]
pub enum Designator<'a> {
    Bracketed {
        open_bracket: Token<'a>,
        index: Expression<'a>,
        close_bracket: Token<'a>,
    },
    Identifier {
        dot: Token<'a>,
        ident: Token<'a>,
    },
}

#[derive(Debug, Clone, Copy)]
struct Declarator<'a> {
    pointers: Option<&'a [Pointer<'a>]>,
    direct_declarator: DirectDeclarator<'a>,
}

impl<'a> Declarator<'a> {
    fn reinterpret_as_concrete(&self, sess: &'a ast::Session<'a>) -> Option<(Token<'a>, Self)> {
        let Self { pointers, direct_declarator } = *self;
        let (name, direct_declarator) = direct_declarator.reinterpret_as_concrete(sess)?;
        Some((name, Self { pointers, direct_declarator }))
    }
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
    ArrayDeclarator(ArrayDeclarator<'a>),
    FunctionDeclarator(FunctionDeclarator<'a>),
}

impl<'a> DirectDeclarator<'a> {
    fn with_name(&self, sess: &'a ast::Session<'a>, name: Token<'a>) -> Option<Self> {
        match self {
            DirectDeclarator::Abstract => Some(Self::Identifier(name)),
            DirectDeclarator::Identifier(_) => None,
            DirectDeclarator::Parenthesised(_) => None,
            DirectDeclarator::ArrayDeclarator(array_declarator) =>
                Some(Self::ArrayDeclarator(ArrayDeclarator {
                    direct_declarator: sess
                        .alloc(array_declarator.direct_declarator.with_name(sess, name)?),
                    ..*array_declarator
                })),
            DirectDeclarator::FunctionDeclarator(function_declarator) =>
                Some(Self::FunctionDeclarator(FunctionDeclarator {
                    direct_declarator: sess.alloc(
                        function_declarator
                            .direct_declarator
                            .with_name(sess, name)?,
                    ),
                    ..*function_declarator
                })),
        }
    }

    fn name(&self) -> Option<&'a str> {
        match self {
            DirectDeclarator::Abstract => None,
            DirectDeclarator::Identifier(token) => Some(token.slice()),
            DirectDeclarator::Parenthesised(declarator) => declarator.direct_declarator.name(),
            DirectDeclarator::ArrayDeclarator(array_declarator) =>
                array_declarator.direct_declarator.name(),
            DirectDeclarator::FunctionDeclarator(function_declarator) =>
                function_declarator.direct_declarator.name(),
        }
    }

    fn parameter_names(&self) -> impl Iterator<Item = (&'a str, TokenKind)> {
        match self {
            DirectDeclarator::Abstract => Either::Left(empty()),
            DirectDeclarator::Identifier(_token) => Either::Left(empty()),
            DirectDeclarator::Parenthesised(declarator) =>
                declarator.direct_declarator.parameter_names(),
            DirectDeclarator::ArrayDeclarator(_array_declarator) => Either::Left(empty()),
            DirectDeclarator::FunctionDeclarator(function_declarator) => Either::Right(
                function_declarator
                    .parameter_type_list
                    .parameter_list
                    .iter()
                    .filter_map(|param| {
                        let kind = if param.declaration_specifiers.is_typedef() {
                            TokenKind::TypeIdentifier
                        }
                        else {
                            TokenKind::Identifier
                        };
                        Some((param.declarator?.direct_declarator.name()?, kind))
                    }),
            ),
        }
    }

    fn reinterpret_as_concrete(&self, sess: &'a ast::Session<'a>) -> Option<(Token<'a>, Self)> {
        match *self {
            Self::Abstract => None,
            Self::Identifier(_) => None,
            Self::Parenthesised(declarator) => {
                let (name, declarator) = declarator.reinterpret_as_concrete(sess)?;
                Some((name, Self::Parenthesised(sess.alloc(declarator))))
            }
            Self::ArrayDeclarator(ArrayDeclarator {
                direct_declarator,
                type_qualifiers,
                length,
                close_bracket,
            }) => {
                let (name, direct_declarator) = direct_declarator.reinterpret_as_concrete(sess)?;
                let direct_declarator = Self::ArrayDeclarator(ArrayDeclarator {
                    direct_declarator: sess.alloc(direct_declarator),
                    type_qualifiers,
                    length,
                    close_bracket,
                });
                Some((name, direct_declarator))
            }
            Self::FunctionDeclarator(FunctionDeclarator {
                direct_declarator: Self::Abstract,
                parameter_type_list,
                close_paren: _,
            }) if let ParameterTypeList { parameter_list, is_varargs: false } =
                parameter_type_list
                && let [param] = parameter_list
                && let ParameterDeclaration { declaration_specifiers, declarator } = param
                && let DeclarationSpecifiers([specifier]) = declaration_specifiers
                && let DeclarationSpecifier::TypeSpecifierQualifier(
                    TypeSpecifierQualifier::Specifier(specifier),
                ) = specifier
                && let name = specifier.token
                && matches!(name.kind, TokenKind::TypeIdentifier) =>
                match *declarator {
                    Some(Declarator { pointers: Some(_), direct_declarator: _ }) => None,
                    Some(Declarator { pointers: None, direct_declarator }) =>
                        Some((name, direct_declarator.with_name(sess, name)?)),
                    None => Some((name, Self::Identifier(name))),
                },
            Self::FunctionDeclarator(FunctionDeclarator {
                direct_declarator,
                parameter_type_list,
                close_paren,
            }) => {
                let (name, direct_declarator) = direct_declarator.reinterpret_as_concrete(sess)?;
                let direct_declarator = Self::FunctionDeclarator(FunctionDeclarator {
                    direct_declarator: sess.alloc(direct_declarator),
                    parameter_type_list,
                    close_paren,
                });
                Some((name, direct_declarator))
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct ArrayDeclarator<'a> {
    direct_declarator: &'a DirectDeclarator<'a>,
    type_qualifiers: &'a [TypeQualifier<'a>],
    length: Option<Expression<'a>>,
    close_bracket: Token<'a>,
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
    Error(&'a dyn Report),
    Name(Token<'a>),
    Integer(Token<'a>),
    CharConstant(Token<'a>),
    String(&'a [Token<'a>]),
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
    Lengthof {
        lengthof: Token<'a>,
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
        question_mark: Token<'a>,
        then: &'a Expression<'a>,
        or_else: &'a Expression<'a>,
    },
    Comma {
        lhs: &'a Expression<'a>,
        rhs: &'a Expression<'a>,
    },
    Increment {
        operator: IncrementOp<'a>,
        operand: &'a Expression<'a>,
        fixity: IncrementFixity,
    },
}

impl<'a> FromError<'a> for Expression<'a> {
    fn from_error(error: &'a dyn Report) -> Self {
        Self::Error(error)
    }
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

impl<'a> BinOp<'a> {
    pub fn str(&self) -> &'static str {
        self.kind.str()
    }

    pub fn loc(&self) -> Loc<'a> {
        self.token.loc()
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
    Lengthof,
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
            UnaryOpKind::Lengthof => "lengthof",
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

#[derive(Debug, Clone, Copy)]
pub struct IncrementOp<'a> {
    kind: IncrementOpKind,
    token: Token<'a>,
}

#[derive(Debug, Clone, Copy)]
pub enum IncrementOpKind {
    Increment,
    Decrement,
}

impl<'a> IncrementOp<'a> {
    pub fn str(&self) -> &'static str {
        match self.kind {
            IncrementOpKind::Increment => "increment",
            IncrementOpKind::Decrement => "decrement",
        }
    }

    pub fn loc(&self) -> Loc<'a> {
        self.token.loc()
    }

    pub fn kind(&self) -> IncrementOpKind {
        self.kind
    }

    pub fn token(&self) -> Token<'a> {
        self.token
    }
}

#[derive(Debug, Clone, Copy)]
pub enum IncrementFixity {
    Prefix,
    Postfix,
}

impl IncrementFixity {
    pub fn str(&self) -> &'static str {
        match self {
            IncrementFixity::Prefix => "pre",
            IncrementFixity::Postfix => "post",
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum AbstractDeclaratorAllowed {
    Yes,
    No,
}

pub fn parse<'a>(
    sess: &'a ast::Session<'a>,
    typedef_names: &'a RefCell<TypedefNames<'a>>,
    is_in_typedef: &'a Cell<bool>,
    tokens: LexerHacked<'a, impl Iterator<Item = Token<'a>>>,
) -> Result<ast::TranslationUnit<'a>, Box<dyn Report + 'a>> {
    let parser = grammar::TranslationUnitParser::new();
    let parse_tree = parser
        .parse(sess, typedef_names, is_in_typedef, tokens)
        .map_err(|err| match err {
            ParseError::UnrecognizedToken {
                token: ((), token @ Token { kind: TokenKind::Error(error), .. }, ()),
                expected: _,
            } => Error::from_lexer_error(error, token.loc()),
            ParseError::UnrecognizedToken { token, expected } => Error::UnexpectedToken {
                at: token.1,
                expected: Strings(
                    sess.alloc_slice_fill_iter(expected.iter().map(|s| sess.alloc_str(s))),
                ),
            },
            ParseError::User { error } => *error,
            err => todo!("{err:#?}"),
        })?;
    Ok(ast::from_parse_tree(sess, parse_tree))
}
