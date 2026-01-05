#![feature(assert_matches)]
#![feature(closure_track_caller)]
#![feature(coverage_attribute)]
#![feature(gen_blocks)]
#![feature(if_let_guard)]
#![feature(never_type)]
#![feature(stmt_expr_attributes)]
#![feature(try_blocks)]
#![feature(type_alias_impl_trait)]
#![feature(unqualified_local_imports)]

use std::cell::Cell;
use std::cell::RefCell;
use std::iter::empty;
use std::path::Path;

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
pub mod nonempty;
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

    #[error("default parameters are not allowed")]
    #[diagnostics(
        param_loc(colour = Blue, label = "in the declaration of this parameter"),
        at(colour = Red, label = "help: remove this initialiser"),
    )]
    DefaultParameter {
        at: Initialiser<'a>,
        param_loc: Loc<'a>,
    },
}

#[derive(Debug, Clone, Copy, Report)]
#[exit_code(1)]
pub enum IntegerLiteralDiagnostic<'a> {
    #[error("integer literal too large")]
    #[diagnostics(at(colour = Red, label = "this literal does not fit any integer type"))]
    TooLarge { at: Token<'a> },

    #[error("invalid integer suffix `{suffix}`")]
    #[diagnostics(at(colour = Red, label = "invalid integer suffix"))]
    #[with(
        suffix_len = match at.kind {
            TokenKind::Integer(panko_lex::Integer { suffix_len, .. }) => suffix_len,
            _ => unreachable!(),
        },
        suffix = at.slice()[at.slice().len() - suffix_len..].fg(Red),
    )]
    InvalidSuffix { at: Token<'a> },
}

#[derive(Debug, Report)]
#[exit_code(2)]
pub enum TodoError<'a> {
    #[error("TODO: {msg}")]
    #[diagnostics(at(colour = Red))]
    #[with(msg = yansi::Paint::bold(&msg.fg(Red)).to_string())]
    Error { at: Loc<'a>, msg: String },
}

#[macro_export]
macro_rules! error_todo {
    ($at:expr $(,)?) => {{
        let msg = String::from("unimplemented error");
        $crate::TodoError::Error { at: $at.loc(), msg: msg.clone() }.print();
        todo!("{msg}")
    }};
    ($at:expr, $msg:literal $(, $param:expr)* $(,)?) => {{
        let at = $at.loc();
        let msg = format!(concat!("unimplemented error: ", $msg), $($param,)*);
        $crate::TodoError::Error { at, msg: msg.clone() }.print();
        todo!("{msg}")
    }};
}

#[derive(Debug, Clone, Copy)]
pub struct TranslationUnit<'a> {
    filename: &'a Path,
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

    fn slice(&self) -> &'a str {
        self.token.slice()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

    fn incomplete_struct(r#struct: Token<'a>, name: Token<'a>) -> Self {
        Self {
            token: r#struct,
            kind: TypeSpecifierKind::Struct(Struct::Incomplete { name }),
        }
    }

    fn r#struct(
        r#struct: Token<'a>,
        name: Option<Token<'a>>,
        members: &'a [Declaration<'a>],
    ) -> Self {
        Self {
            token: r#struct,
            kind: TypeSpecifierKind::Struct(Struct::Complete { name, members }),
        }
    }

    fn loc(&self) -> Loc<'a> {
        match self.kind {
            TypeSpecifierKind::Struct(
                Struct::Incomplete { name } | Struct::Complete { name: Some(name), members: _ },
            ) => self.token.loc().until(name.loc()),
            _ => self.token.loc(),
        }
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

        let error_full = #[track_caller]
        |message: &'a str| {
            let () = sess.emit(Diagnostic::InvalidTypeSpecifierCombination {
                message,
                at: *self,
                specifiers: specifiers.until(position),
                ty,
            });
            ty
        };

        let error = #[track_caller]
        || error_full("invalid combination of type specifiers");

        let exclusive = #[track_caller]
        |value| match ty {
            Parsed::None => value,
            _ => error(),
        };

        match self.kind {
            Kind::Void => exclusive(Parsed::Void),
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
            Kind::TypedefName => exclusive(Parsed::Typedef(self.token)),
            Kind::Bool => exclusive(Parsed::Int {
                signedness: Some(Signedness::Unsigned),
                kind: Some(IntegralKind::Bool),
            }),
            Kind::Typeof { unqual, expr } => exclusive(Parsed::Typeof { unqual, expr }),
            Kind::TypeofTy { unqual, ty } => exclusive(Parsed::TypeofTy { unqual, ty }),
            Kind::Struct(Struct::Incomplete { name }) =>
                exclusive(Parsed::IncompleteStruct { name }),
            Kind::Struct(Struct::Complete { name, members }) =>
                exclusive(Parsed::CompleteStruct { name, members }),
            _ => todo!("unimplemented type specifier: {self:#?}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Struct<'a> {
    Incomplete {
        name: Token<'a>,
    },
    Complete {
        name: Option<Token<'a>>,
        members: &'a [Declaration<'a>],
    },
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
    Struct(Struct<'a>),
    // enum-specifier
    TypedefName,
    Typeof {
        unqual: bool,
        expr: &'a Expression<'a>,
    },
    TypeofTy {
        unqual: bool,
        ty: &'a QualifiedType<'a>,
    },
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
    kind: FunctionSpecifierKind,
}

impl<'a> FunctionSpecifier<'a> {
    fn new(token: Token<'a>) -> Self {
        Self {
            token,
            kind: function_specifier_kind(token.kind),
        }
    }

    pub fn slice(&self) -> &'a str {
        self.token.slice()
    }

    pub fn loc(&self) -> Loc<'a> {
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

impl<'a> Initialiser<'a> {
    fn loc(&self) -> Loc<'a> {
        match self {
            Self::Braced {
                open_brace,
                initialiser_list: _,
                close_brace,
            } => open_brace.loc().until(close_brace.loc()),
            Self::Expression(expr) => expr.loc(),
        }
    }
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
    fn loc_with(&self, declaration_specifiers: DeclarationSpecifiers<'a>) -> Loc<'a> {
        let Self { pointers, direct_declarator } = self;
        let loc = declaration_specifiers.loc();
        match direct_declarator.maybe_end_loc() {
            Some(end) => loc.until(end),
            None => match pointers {
                Some(pointers) => loc.until(pointers_loc(pointers)),
                None => loc,
            },
        }
    }

    fn reinterpret_as_concrete(&self, sess: &'a ast::Session<'a>) -> Option<(Token<'a>, Self)> {
        let Self { pointers, direct_declarator } = *self;
        let (name, direct_declarator) = direct_declarator.reinterpret_as_concrete(sess)?;
        Some((name, Self { pointers, direct_declarator }))
    }
}

fn pointers_loc<'a>(pointers: &[Pointer<'a>]) -> Loc<'a> {
    match pointers {
        [] => unreachable!(),
        [pointer] => pointer.loc(),
        [first, .., last] => first.loc().until(last.loc()),
    }
}

#[derive(Debug, Clone, Copy)]
struct Pointer<'a> {
    star: Token<'a>,
    qualifiers: &'a [TypeQualifier<'a>],
}

impl<'a> Pointer<'a> {
    fn loc(&self) -> Loc<'a> {
        let Self { star, qualifiers } = self;
        let loc = star.loc();
        match qualifiers.last() {
            Some(qualifier) => loc.until(qualifier.loc()),
            None => loc,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum DirectDeclarator<'a> {
    Abstract,
    Identifier(Token<'a>),
    Parenthesised {
        declarator: &'a Declarator<'a>,
        close_paren: Token<'a>,
    },
    ArrayDeclarator(ArrayDeclarator<'a>),
    FunctionDeclarator(FunctionDeclarator<'a>),
}

impl<'a> DirectDeclarator<'a> {
    fn with_name(&self, sess: &'a ast::Session<'a>, name: Token<'a>) -> Option<Self> {
        match self {
            DirectDeclarator::Abstract => Some(Self::Identifier(name)),
            DirectDeclarator::Identifier(_) => None,
            DirectDeclarator::Parenthesised { declarator: _, close_paren: _ } => None,
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
            DirectDeclarator::Parenthesised { declarator, close_paren: _ } =>
                declarator.direct_declarator.name(),
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
            DirectDeclarator::Parenthesised { declarator, close_paren: _ } =>
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
            Self::Parenthesised { declarator, close_paren } => {
                let (name, declarator) = declarator.reinterpret_as_concrete(sess)?;
                Some((
                    name,
                    Self::Parenthesised {
                        declarator: sess.alloc(declarator),
                        close_paren,
                    },
                ))
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

    fn maybe_end_loc(&self) -> Option<Loc<'a>> {
        Some(match self {
            Self::Abstract => return None,
            Self::Identifier(token) => token.loc(),
            Self::Parenthesised { declarator: _, close_paren } => close_paren.loc(),
            Self::ArrayDeclarator(array_declarator) => array_declarator.close_bracket.loc(),
            Self::FunctionDeclarator(function_declarator) => function_declarator.close_paren.loc(),
        })
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
    Nullptr(Token<'a>),
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

impl<'a> Expression<'a> {
    fn loc(&self) -> Loc<'a> {
        match self {
            Self::Error(report) => report.location(),
            Self::Name(token) => token.loc(),
            Self::Integer(token) => token.loc(),
            Self::CharConstant(token) => token.loc(),
            Self::String(tokens) => preprocess::tokens_loc(tokens),
            Self::Nullptr(token) => token.loc(),
            Self::Parenthesised { open_paren, expr: _, close_paren } =>
                open_paren.loc().until(close_paren.loc()),
            Self::Assign { target, value } => target.loc().until(value.loc()),
            Self::CompoundAssign { target, op: _, value } => target.loc().until(value.loc()),
            Self::BinOp { lhs, op: _, rhs } => lhs.loc().until(rhs.loc()),
            Self::UnaryOp { operator, operand } => operator.loc().until(operand.loc()),
            Self::Call { callee, args: _, close_paren } => callee.loc().until(close_paren.loc()),
            Self::Sizeof { sizeof, ty: _, close_paren } => sizeof.loc().until(close_paren.loc()),
            Self::Lengthof { lengthof, ty: _, close_paren } =>
                lengthof.loc().until(close_paren.loc()),
            Self::Alignof { alignof, ty: _, close_paren } => alignof.loc().until(close_paren.loc()),
            Self::Cast { open_paren, ty: _, expr } => open_paren.loc().until(expr.loc()),
            Self::Subscript { lhs, rhs: _, close_bracket } => lhs.loc().until(close_bracket.loc()),
            Self::Generic {
                generic,
                selector: _,
                assocs: _,
                close_paren,
            } => generic.loc().until(close_paren.loc()),
            Self::Logical { lhs, op: _, rhs } => lhs.loc().until(rhs.loc()),
            Self::Conditional {
                condition,
                question_mark: _,
                then: _,
                or_else,
            } => condition.loc().until(or_else.loc()),
            Self::Comma { lhs, rhs } => lhs.loc().until(rhs.loc()),
            Self::Increment { operator, operand, fixity: _ } => operator.loc().until(operand.loc()),
        }
    }
}

impl<'a> FromError<'a> for Expression<'a> {
    fn from_error(error: &'a dyn Report) -> Self {
        Self::Error(error)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Comparison {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

impl Comparison {
    pub fn str(self) -> &'static str {
        match self {
            Self::Equal => "equal",
            Self::NotEqual => "not-equal",
            Self::Less => "less",
            Self::LessEqual => "less-equal",
            Self::Greater => "greater",
            Self::GreaterEqual => "greater-equal",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOpKind {
    Multiply,
    Divide,
    Modulo,
    Add,
    Subtract,
    LeftShift,
    RightShift,
    BitAnd,
    BitXor,
    BitOr,
    Comparison(Comparison),
}

impl BinOpKind {
    fn str(self) -> &'static str {
        match self {
            BinOpKind::Multiply => "multiply",
            BinOpKind::Divide => "divide",
            BinOpKind::Modulo => "modulo",
            BinOpKind::Add => "add",
            BinOpKind::Subtract => "subtract",
            BinOpKind::LeftShift => "left-shift",
            BinOpKind::RightShift => "right-shift",
            BinOpKind::BitAnd => "bit-and",
            BinOpKind::BitXor => "bit-xor",
            BinOpKind::BitOr => "bit-or",
            BinOpKind::Comparison(cmp) => cmp.str(),
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

fn handle_parse_error<'a, T: FromError<'a>>(
    sess: &ast::Session<'a>,
) -> impl FnOnce(ParseError<(), Token<'a>, &'a Error<'a>>) -> T {
    move |err| {
        let err = match err {
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
        };
        sess.emit(err)
    }
}

pub fn parse<'a>(
    sess: &'a ast::Session<'a>,
    filename: &'a Path,
    typedef_names: &'a RefCell<TypedefNames<'a>>,
    is_in_typedef: &'a Cell<bool>,
    tokens: LexerHacked<'a, impl Iterator<Item = Token<'a>>>,
) -> Option<ast::TranslationUnit<'a>> {
    let parser = grammar::TranslationUnitParser::new();
    let parse_tree = parser
        .parse(sess, typedef_names, is_in_typedef, filename, tokens)
        .map_err(handle_parse_error::<()>(sess))
        .ok()?;
    Some(ast::from_parse_tree(sess, parse_tree))
}
