#![feature(assert_matches)]
#![feature(coverage_attribute)]
#![feature(if_let_guard)]
#![feature(never_type)]
#![feature(try_blocks)]
#![feature(unqualified_local_imports)]

use std::cell::Cell;
use std::cell::RefCell;
use std::iter::empty;

use ariadne::Color::Red;
use itertools::Either;
use itertools::Itertools as _;
use lalrpop_util::ParseError;
use lalrpop_util::lalrpop_mod;
use panko_lex::Loc;
use panko_lex::Token;
use panko_lex::TokenIter;
use panko_lex::TokenKind;
use panko_lex::TypedefNames;
use panko_report::Report;

use crate::ast::Arithmetic;
use crate::ast::ErrorExpr;
use crate::ast::Integral;
use crate::ast::IntegralKind;
use crate::ast::QualifiedType;
use crate::ast::Signedness;
use crate::ast::Type;

mod as_sexpr;
pub mod ast;
pub mod sexpr_builder;

lalrpop_mod!(
    #[allow(clippy::as_conversions, reason = "lalrpop generates `as` conversions")]
    #[allow(clippy::uninlined_format_args)]
    #[allow(unused_qualifications)]
    grammar
);

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

    fn parse(&self, _sess: &ast::Session<'a>, ty: &mut Option<Type<'a>>) {
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
            TypeSpecifierKind::TypedefName => match ty {
                Some(ty) => todo!("{ty}"),
                None => *ty = Some(Type::Typedef(self.token)),
            },
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
    fn reinterpret_as_concrete_declarator(
        &self,
        sess: &'a ast::Session<'a>,
    ) -> Option<(Token<'a>, Self)> {
        let Self { pointers, direct_declarator } = *self;
        try {
            let (name, declarator) = direct_declarator.reinterpret_as_concrete_declarator(sess)?;
            let direct_declarator = match declarator.pointers {
                Some(_) => DirectDeclarator::Parenthesised(sess.alloc(declarator)),
                None => declarator.direct_declarator,
            };
            (name, Self { pointers, direct_declarator })
        }
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
            DirectDeclarator::Parenthesised(declarator) =>
                Some(Self::Parenthesised(sess.alloc(Declarator {
                    direct_declarator: declarator.direct_declarator.with_name(sess, name)?,
                    ..**declarator
                }))),
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

    fn reinterpret_as_concrete_declarator(
        &self,
        sess: &'a ast::Session<'a>,
    ) -> Option<(Token<'a>, Declarator<'a>)> {
        match *self {
            DirectDeclarator::Abstract => None,
            DirectDeclarator::Identifier(_) => None,
            DirectDeclarator::Parenthesised(declarator) =>
                declarator.reinterpret_as_concrete_declarator(sess),
            DirectDeclarator::ArrayDeclarator(ArrayDeclarator {
                direct_declarator,
                type_qualifiers,
                length,
                close_bracket,
            }) => {
                let (name, declarator) =
                    direct_declarator.reinterpret_as_concrete_declarator(sess)?;
                let direct_declarator = match declarator.pointers {
                    Some(_) => DirectDeclarator::Parenthesised(sess.alloc(declarator)),
                    None => declarator.direct_declarator,
                };
                let direct_declarator = DirectDeclarator::ArrayDeclarator(ArrayDeclarator {
                    direct_declarator: sess.alloc(direct_declarator),
                    type_qualifiers,
                    length,
                    close_bracket,
                });
                Some((name, Declarator { pointers: None, direct_declarator }))
            }
            DirectDeclarator::FunctionDeclarator(FunctionDeclarator {
                direct_declarator,
                parameter_type_list,
                close_paren: _,
            }) if let DirectDeclarator::Abstract = direct_declarator
                && let ParameterTypeList { parameter_list, is_varargs: false } =
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
                    Some(Declarator { pointers: None, direct_declarator }) => {
                        let direct_declarator = direct_declarator.with_name(sess, name)?;
                        Some((name, Declarator { pointers: None, direct_declarator }))
                    }
                    None => Some((
                        name,
                        Declarator {
                            pointers: None,
                            direct_declarator: DirectDeclarator::Identifier(name),
                        },
                    )),
                },
            DirectDeclarator::FunctionDeclarator(FunctionDeclarator {
                direct_declarator,
                parameter_type_list,
                close_paren,
            }) => {
                let (name, declarator) =
                    direct_declarator.reinterpret_as_concrete_declarator(sess)?;
                let direct_declarator = match declarator.pointers {
                    Some(_) => DirectDeclarator::Parenthesised(sess.alloc(declarator)),
                    None => declarator.direct_declarator,
                };
                let direct_declarator = DirectDeclarator::FunctionDeclarator(FunctionDeclarator {
                    direct_declarator: sess.alloc(direct_declarator),
                    parameter_type_list,
                    close_paren,
                });
                Some((name, Declarator { pointers: None, direct_declarator }))
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

impl<'a> ErrorExpr<'a> for Expression<'a> {
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
    tokens: TokenIter<'a>,
) -> Result<ast::TranslationUnit<'a>, Box<dyn Report + 'a>> {
    let parser = grammar::TranslationUnitParser::new();
    let parse_tree = parser
        .parse(
            sess,
            typedef_names,
            is_in_typedef,
            tokens.map(|token| Ok(token?)),
        )
        .map_err(|err| match err {
            ParseError::UnrecognizedToken { token, expected } =>
                Error::UnrecognisedToken { at: token.1, expected: Strings(expected) },
            err => todo!("{err:#?}"),
        })?;
    Ok(ast::from_parse_tree(sess, parse_tree))
}
