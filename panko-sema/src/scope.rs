use std::assert_matches;
use std::bstr::ByteStr;
use std::fmt;
use std::path::Path;

use ariadne::Color::Blue;
use ariadne::Color::Red;
use ariadne::Fmt as _;
use itertools::Itertools as _;
use panko_lex::Loc;
use panko_lex::Token;
use panko_parser as cst;
use panko_parser::ArrayDeclarator;
use panko_parser::BinOp;
use panko_parser::DirectDeclarator;
use panko_parser::FunctionDeclarator;
use panko_parser::IncrementOp;
use panko_parser::LogicalOp;
use panko_parser::MemberAccessOp;
use panko_parser::MemberAccessOpKind;
use panko_parser::NO_VALUE;
use panko_parser::ParameterTypeList;
use panko_parser::StorageClassSpecifierKind;
use panko_parser::TypeName;
use panko_parser::TypeQualifier;
use panko_parser::UnaryOp;
use panko_parser::UnaryOpKind;
use panko_parser::ast;
use panko_parser::ast::DeclarationSpecifiers;
use panko_parser::ast::FromError;
use panko_parser::ast::FunctionSpecifiers;
use panko_parser::ast::FunctionStorageClass;
use panko_parser::ast::Qualifiers;
use panko_parser::ast::Session;
use panko_parser::ast::Struct;
use panko_parser::ast::reject_function_specifiers;
use panko_parser::error_todo;
use panko_parser::sexpr_builder::SExpr;
use panko_parser::unimplemented_todo;
use panko_report::Report;
use panko_report::Sliced as _;

use crate::fake_trait_impls::HashEqIgnored;
use crate::fake_trait_impls::NoHashEq;
use crate::scope::scopes::Scopes;
use crate::scope::scopes::Tag;
use crate::scope::scopes::Tagged;
use crate::ty;
use crate::ty::ParameterDeclaration;

mod as_sexpr;
mod scopes;

#[derive(Debug, Report)]
#[exit_code(1)]
pub(crate) enum Diagnostic<'a> {
    #[error("duplicate definition for `{at}`")]
    #[diagnostics(
        previous_definition(colour = Blue, label = "previously defined here"),
        at(colour = Red, label = "duplicate definition"),
    )]
    AlreadyDefined {
        at: Loc<'a>,
        previous_definition: Loc<'a>,
    },

    #[error("expected `;` after declaration (or did you mean to declare a function: `{at}()`?)")]
    #[diagnostics(at(colour = Red, label = "in this declaration"))]
    FunctionDeclaratorDoesNotHaveFunctionType { at: Token<'a> },

    #[error("use of undeclared identifier `{at}`")]
    #[diagnostics(at(colour = Red, label = "this name has not been declared"))]
    UndeclaredName { at: Token<'a> },

    #[error("`{typedef}` name `{at}` redeclared as {kind} name")]
    #[diagnostics(
        at(colour = Red, label = "redeclared here as a {kind} name"),
        ty(colour = Blue, label = "originally declared here as a `{typedef}` name"),
    )]
    #[with(typedef = "typedef".fg(Blue))]
    TypedefRedeclaredAsValue {
        at: Token<'a>,
        ty: QualifiedType<'a>,
        kind: &'a str,
    },

    #[error("{kind} name `{name}` redeclared as `{typedef}` name")]
    #[diagnostics(
        at(colour = Red, label = "redeclared here as a `{typedef}` name"),
        reference(colour = Blue, label = "originally declared here as a {kind} name"),
    )]
    #[with(
        typedef = "typedef".fg(Red),
        name = reference.name,
    )]
    ValueRedeclaredAsTypedef {
        at: QualifiedType<'a>,
        reference: Reference<'a>,
        kind: &'a str,
    },

    #[error("redeclaration of `{previous_ty}` with different tag `{actual}`")]
    #[diagnostics(
        at(colour = Red, label = "redeclared here as `{actual}`"),
        previous_decl(colour = Blue, label = "previously declared here as `{expected}`"),
    )]
    #[with(
        previous_ty = previous_ty.fg(Blue),
        actual = actual.fg(Red),
        expected = expected.fg(Blue),
    )]
    TagMismatchInRedeclaration {
        at: Loc<'a>,
        previous_decl: Loc<'a>,
        previous_ty: Type<'a>,
        expected: Tag,
        actual: Tag,
    },

    #[error("invalid storage class `{at}` applied to definition of function `{function}`")]
    #[diagnostics(
        at(colour = Red, label = "`{at}` is invalid for functions"),
        function(colour = Blue, label = "in the definition of function `{function}`"),
    )]
    InvalidStorageClassForFunctionDefinition { at: Token<'a>, function: Token<'a> },

    #[error("function defined without a name")]
    #[diagnostics(at(colour = Red, label = "this definition lacks a name"))]
    FunctionDefinedWithoutName { at: QualifiedType<'a> },

    #[error("declaration does not specify a name")]
    #[diagnostics(at(colour = Red, label = "this looks like a declaration with type `{ty}`"))]
    #[with(ty = ty.fg(Red))]
    DeclarationWithoutName { at: Loc<'a>, ty: QualifiedType<'a> },

    #[error(
        "parameter `{name}` declared with storage class `{at}` (only `register` is allowed for parameters)"
    )]
    #[diagnostics(
        at(colour = Red, label = "this storage class is not allowed for function parameters"),
        parameter(colour = Blue, label = "in this parameter declaration"),
    )]
    #[with(name = name.fg(Blue))]
    StorageClassInParameterDeclaration {
        at: Token<'a>,
        name: &'a str,
        parameter: Loc<'a>,
    },

    #[error("cannot use type qualifier `{at}` in non-parameter array declarator")]
    #[diagnostics(at(colour = Red, label = "help: remove this `{at}`"))]
    InvalidTypeQualifierInArrayBrackets { at: TypeQualifier<'a> },
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Member<'a> {
    pub(crate) name: Option<Token<'a>>,
    pub(crate) bitfield_width: Option<BitfieldWidth<'a>>,
    pub(crate) ty: QualifiedType<'a>,
}

impl<'a> Member<'a> {
    pub(crate) fn loc(&self) -> Loc<'a> {
        let Self { name, bitfield_width: _, ty } = *self;
        match name {
            Some(name) => name.loc(),
            None => ty.loc(),
        }
    }

    pub(crate) fn slice(&self) -> String {
        let Self { name, bitfield_width: _, ty } = self;
        match name {
            Some(name) => name.slice().to_string(),
            None => format!("<unnamed struct member of type `{ty}`>"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct BitfieldWidth<'a> {
    pub(crate) width: Expression<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Scope {}

impl ty::Step for Scope {
    type LengthExpr<'a> = NoHashEq<Option<&'a Expression<'a>>>;
    type Member<'a> = NoHashEq<Member<'a>>;
    type TypeofExpr<'a> = NoHashEq<Typeof<'a>>;
}

pub(crate) type ArrayType<'a> = ty::ArrayType<'a, Scope>;
pub(crate) type FunctionType<'a> = ty::FunctionType<'a, Scope>;
pub(crate) type Type<'a> = ty::Type<'a, Scope>;
pub(crate) type QualifiedType<'a> = ty::QualifiedType<'a, Scope>;

#[derive(Debug)]
enum OpenNewScope {
    Yes,
    No,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(pub(crate) u64);

#[derive(Debug, Clone, Copy)]
pub struct TranslationUnit<'a> {
    pub(crate) filename: &'a Path,
    pub(crate) decls: &'a [ExternalDeclaration<'a>],
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ExternalDeclaration<'a> {
    FunctionDefinition(FunctionDefinition<'a>),
    Declaration(Declarators<'a>),
    Error(&'a dyn Report),
}

impl<'a> FromError<'a> for ExternalDeclaration<'a> {
    fn from_error(error: &'a dyn Report) -> Self {
        Self::Error(error)
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Redeclared<'a> {
    ValueAsTypedef {
        at: QualifiedType<'a>,
        reference: Reference<'a>,
    },
    TypedefAsValue {
        at: Token<'a>,
        typedef_ty: QualifiedType<'a>,
        value_ty: QualifiedType<'a>,
    },
}

impl<'a> Redeclared<'a> {
    pub(crate) fn ty(&self) -> &QualifiedType<'a> {
        match self {
            Self::ValueAsTypedef { at: _, reference } => &reference.ty,
            Self::TypedefAsValue { at: _, typedef_ty: _, value_ty } => value_ty,
        }
    }

    fn name(&self) -> &'a str {
        match self {
            Self::ValueAsTypedef { at: _, reference } => reference.name,
            Self::TypedefAsValue { at, typedef_ty: _, value_ty: _ } => at.slice(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Declarator<'a> {
    Typedef(Typedef<'a>),
    Declaration(Declaration<'a>),
    Redeclared(Redeclared<'a>),
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Declarators<'a> {
    pub(crate) ty: QualifiedType<'a>,
    pub(crate) unresolved_ty: ast::QualifiedType<'a>,
    pub(crate) declarators: &'a [Declarator<'a>],
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Declaration<'a> {
    pub(crate) function_specifiers: FunctionSpecifiers<'a>,
    pub(crate) reference: Reference<'a>,
    pub(crate) initialiser: Option<&'a Initialiser<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Typedef<'a> {
    pub(crate) ty: QualifiedType<'a>,
    pub(crate) name: Token<'a>,
    pub(crate) previously_declared_as: Option<QualifiedType<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Initialiser<'a> {
    Braced {
        open_brace: Token<'a>,
        initialiser_list: &'a [DesignatedInitialiser<'a>],
        close_brace: Token<'a>,
    },
    Expression(Expression<'a>),
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct DesignatedInitialiser<'a> {
    pub(crate) designation: Option<Designation<'a>>,
    pub(crate) initialiser: &'a Initialiser<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Designation<'a>(pub(crate) &'a [Designator<'a>]);

#[derive(Debug, Clone, Copy)]
pub(crate) enum Designator<'a> {
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
pub(crate) struct FunctionDefinition<'a> {
    pub(crate) reference: Reference<'a>,
    pub(crate) return_slot: Reference<'a>,
    pub(crate) params: ParamRefs<'a>,
    pub(crate) inline: Option<cst::FunctionSpecifier<'a>>,
    pub(crate) noreturn: Option<cst::FunctionSpecifier<'a>>,
    pub(crate) varargs: Option<Varargs<'a>>,
    pub(crate) body: CompoundStatement<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct ParamRefs<'a>(pub(crate) &'a [Reference<'a>]);

#[derive(Debug, Clone, Copy)]
pub(crate) struct Varargs<'a> {
    pub(crate) reg_save_area: Reference<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct CompoundStatement<'a>(pub(crate) &'a [Statement<'a>]);

#[derive(Debug, Clone, Copy)]
pub(crate) enum Statement<'a> {
    Declaration(Declarators<'a>),
    Expression(Option<Expression<'a>>),
    Compound(CompoundStatement<'a>),
    Return {
        return_: Token<'a>,
        expr: Option<Expression<'a>>,
    },
    HoistedCompoundLiteral(Reference<'a>),
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Expression<'a> {
    Error(&'a dyn Report),
    Name(Reference<'a>),
    Integer {
        value: &'a str,
        token: Token<'a>,
    },
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
        target_temporary: Reference<'a>,
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
        fixity: IncrementFixity<'a>,
        reference: Reference<'a>,
    },
    MemberAccess {
        lhs: &'a Expression<'a>,
        op: MemberAccessOp<'a>,
        member: Token<'a>,
    },
    BuiltinOffsetof {
        builtin_offsetof: Token<'a>,
        ty: QualifiedType<'a>,
        member: Token<'a>,
        close_paren: Token<'a>,
    },
    BuiltinName(BuiltinName<'a>),
    CompoundLiteral {
        open_paren: Token<'a>,
        decl: Declaration<'a>,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct Reference<'a> {
    pub(crate) name: &'a str,
    pub(crate) decl_loc: Loc<'a>,
    pub(crate) ty: QualifiedType<'a>,
    pub(crate) id: Id,
    pub(crate) usage_loc: Loc<'a>,
    pub(crate) storage_duration: StorageDuration<Option<Linkage>>,
    pub(crate) previous_definition: Option<&'a Self>,
    pub(crate) is_parameter: IsParameter,
    pub(crate) is_in_global_scope: IsInGlobalScope,
    pub(crate) initialiser: Option<RefInitialiser<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum RefKind {
    Declaration,
    TentativeDefinition,
    Definition,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum IsParameter {
    Yes,
    No,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum IsInGlobalScope {
    Yes,
    No,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum StorageDuration<Linkage> {
    Static(Linkage),
    Automatic,
    // TODO: thread local
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Linkage {
    External,
    Internal,
    None,
    Inline,
}

impl Linkage {
    pub(crate) fn in_sexpr(self, sexpr: SExpr) -> SExpr {
        match self {
            Self::External => sexpr.inline_string("external".to_string()),
            Self::Internal => sexpr.inline_string("internal".to_string()),
            Self::None => sexpr,
            Self::Inline => sexpr.inline_string("inline".to_string()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum RefInitialiser<'a> {
    Initialiser(&'a Initialiser<'a>),
    FunctionBody,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct GenericAssocList<'a>(pub(crate) &'a [GenericAssociation<'a>]);

#[derive(Debug, Clone, Copy)]
pub(crate) enum GenericAssociation<'a> {
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
pub(crate) enum Typeof<'a> {
    Expr(&'a Expression<'a>),
    Ty(&'a QualifiedType<'a>),
}

#[derive(Debug, Clone, Copy)]
pub struct BuiltinName<'a> {
    pub kind: BuiltinNameKind<'a>,
    pub loc: Loc<'a>,
}

#[derive(Debug, Clone, Copy)]
pub enum BuiltinNameKind<'a> {
    GpOffset,
    OverflowArgArea,
    Func(&'a ByteStr),
}

impl fmt::Display for BuiltinNameKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::GpOffset => "gp_offset".to_owned(),
            Self::OverflowArgArea => "overflow_arg_area".to_owned(),
            Self::Func(func) => format!("__func__ {func:?}"),
        };
        write!(f, "{s}")
    }
}

impl<'a> Typedef<'a> {
    pub(crate) fn loc(&self) -> Loc<'a> {
        // TODO: should this be the whole declaration?
        self.name.loc()
    }
}

impl<'a> Initialiser<'a> {
    pub(crate) fn loc(&self) -> Loc<'a> {
        match self {
            Self::Braced {
                open_brace,
                initialiser_list: _,
                close_brace,
            } => open_brace.loc().until(close_brace.loc()),
            Self::Expression(expression) => expression.loc(),
        }
    }
}

impl<'a> Designator<'a> {
    pub(crate) fn loc(&self) -> Loc<'a> {
        match self {
            Self::Bracketed { open_bracket, index: _, close_bracket } =>
                open_bracket.loc().until(close_bracket.loc()),
            Self::Identifier { dot, ident } => dot.loc().until(ident.loc()),
        }
    }
}

impl<'a> FunctionDefinition<'a> {
    pub(crate) fn return_ty(&self) -> &'a QualifiedType<'a> {
        match self.reference.ty.ty {
            Type::Function(function_ty) => function_ty.return_type,
            _ => unreachable!(),
        }
    }

    pub(crate) fn loc(&self) -> Loc<'a> {
        self.reference.loc()
    }
}

impl<'a> Expression<'a> {
    pub(crate) fn loc(&self) -> Loc<'a> {
        match self {
            Expression::Error(error) => error.location(),
            Expression::Name(name) => name.loc(),
            Expression::Integer { value: _, token } => token.loc(),
            Expression::CharConstant(char) => char.loc(),
            Expression::String(tokens) => tokens
                .first()
                .unwrap()
                .loc()
                .until(tokens.last().unwrap().loc()),
            Expression::Nullptr(nullptr) => nullptr.loc(),
            Expression::Parenthesised { open_paren, expr: _, close_paren } =>
                open_paren.loc().until(close_paren.loc()),
            Expression::Assign { target, value } => target.loc().until(value.loc()),
            Expression::CompoundAssign {
                target,
                target_temporary: _,
                op: _,
                value,
            } => target.loc().until(value.loc()),
            Expression::BinOp { lhs, op: _, rhs } => lhs.loc().until(rhs.loc()),
            Expression::UnaryOp { operator, operand } => operator.loc().until(operand.loc()),
            Expression::Call { callee, args: _, close_paren } =>
                callee.loc().until(close_paren.loc()),
            Expression::Sizeof { sizeof, ty: _, close_paren } =>
                sizeof.loc().until(close_paren.loc()),
            Expression::Lengthof { lengthof, ty: _, close_paren } =>
                lengthof.loc().until(close_paren.loc()),
            Expression::Alignof { alignof, ty: _, close_paren } =>
                alignof.loc().until(close_paren.loc()),
            Expression::Cast { open_paren, ty: _, expr } => open_paren.loc().until(expr.loc()),
            Expression::Subscript { lhs, rhs: _, close_bracket } =>
                lhs.loc().until(close_bracket.loc()),
            Expression::Generic {
                generic,
                selector: _,
                assocs: _,
                close_paren,
            } => generic.loc().until(close_paren.loc()),
            Expression::Logical { lhs, op: _, rhs } => lhs.loc().until(rhs.loc()),
            Expression::Conditional {
                condition,
                question_mark: _,
                then: _,
                or_else,
            } => condition.loc().until(or_else.loc()),
            Expression::Comma { lhs, rhs } => lhs.loc().until(rhs.loc()),
            Expression::Increment {
                operator,
                operand,
                fixity: _,
                reference: _,
            } => operator.loc().until(operand.loc()),
            Expression::MemberAccess { lhs, op: _, member } => lhs.loc().until(member.loc()),
            Expression::BuiltinOffsetof {
                builtin_offsetof,
                ty: _,
                member: _,
                close_paren,
            } => builtin_offsetof.loc().until(close_paren.loc()),
            Expression::BuiltinName(BuiltinName { kind: _, loc }) => *loc,
            Expression::CompoundLiteral { open_paren, decl } =>
                open_paren.loc().until(decl.initialiser.unwrap().loc()),
        }
    }
}

impl<'a> FromError<'a> for Expression<'a> {
    fn from_error(error: &'a dyn Report) -> Self {
        Self::Error(error)
    }
}

impl<'a> Reference<'a> {
    pub fn unique_name(&self) -> String {
        format!("{}~{}", self.name, self.id.0)
    }

    pub fn loc(&self) -> Loc<'a> {
        self.usage_loc
    }

    pub(crate) fn slice(&self) -> &'a str {
        self.name
    }

    fn at(&self, usage_loc: Loc<'a>) -> Self {
        assert_eq!(self.name, usage_loc.slice());
        Self { usage_loc, ..*self }
    }

    pub(crate) fn linkage(&self) -> Option<Linkage> {
        match self.storage_duration {
            StorageDuration::Static(linkage) => linkage,
            StorageDuration::Automatic => None,
        }
    }
}

impl RefKind {
    pub(crate) fn str(&self) -> &'static str {
        match self {
            RefKind::Declaration => "declaration",
            RefKind::TentativeDefinition => "tentative-definition",
            RefKind::Definition => "definition",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum IncrementFixity<'a> {
    Prefix,
    Postfix {
        member: Option<&'a Token<'a>>,
        pointer: Reference<'a>,
        copy: Reference<'a>,
    },
}

impl IncrementFixity<'_> {
    fn str(&self) -> &'static str {
        match self {
            IncrementFixity::Prefix => "pre",
            IncrementFixity::Postfix { member: _, pointer: _, copy: _ } => "post",
        }
    }
}

fn parse_declarator<'a>(
    scopes: &mut Scopes<'a>,
    mut ty: QualifiedType<'a>,
    mut declarator: cst::Declarator<'a>,
    is_parameter: IsParameter,
) -> (QualifiedType<'a>, Option<Token<'a>>) {
    let sess = scopes.sess;
    let name = loop {
        for pointer in declarator.pointers.unwrap_or_default() {
            let Qualifiers { const_qualifier, volatile_qualifier } =
                Qualifiers::parse(sess, pointer.qualifiers);
            ty = QualifiedType {
                is_const: const_qualifier.is_some(),
                is_volatile: volatile_qualifier.is_some(),
                ty: Type::Pointer(sess.alloc(ty)),
                loc: HashEqIgnored(pointer.star.loc().until(ty.loc.0)),
            };
        }
        match declarator.direct_declarator {
            DirectDeclarator::Abstract => break None,
            DirectDeclarator::Identifier(name) => break Some(name),
            DirectDeclarator::Parenthesised { declarator: decl, close_paren: _ } =>
                declarator = *decl,
            DirectDeclarator::ArrayDeclarator(ArrayDeclarator {
                direct_declarator,
                type_qualifiers,
                length,
                close_bracket,
            }) => {
                let Qualifiers { const_qualifier, volatile_qualifier } = match is_parameter {
                    IsParameter::Yes => Qualifiers::parse(sess, type_qualifiers),
                    IsParameter::No => {
                        for qualifier in type_qualifiers {
                            sess.emit(Diagnostic::InvalidTypeQualifierInArrayBrackets {
                                at: *qualifier,
                            })
                        }
                        Qualifiers::default()
                    }
                };
                declarator = cst::Declarator {
                    pointers: None,
                    direct_declarator: *direct_declarator,
                };
                let loc = HashEqIgnored(ty.loc.0.until(close_bracket.loc()));
                ty = QualifiedType {
                    is_const: const_qualifier.is_some(),
                    is_volatile: volatile_qualifier.is_some(),
                    ty: Type::Array(ArrayType {
                        length: NoHashEq(try { sess.alloc(resolve_expr(scopes, &length?)) }),
                        // the `length` expr can complete the element type, so we have to hack
                        // around the Ast being immutable
                        ty: sess.alloc(reresolve_ty(scopes, &ty)),
                        loc,
                    }),
                    loc,
                }
            }
            DirectDeclarator::FunctionDeclarator(FunctionDeclarator {
                direct_declarator,
                parameter_type_list: ParameterTypeList { parameter_list, is_varargs },
                close_paren,
            }) => {
                declarator = cst::Declarator {
                    pointers: None,
                    direct_declarator: *direct_declarator,
                };
                ty = QualifiedType {
                    is_const: false,
                    is_volatile: false,
                    ty: Type::Function(resolve_function_ty(scopes, parameter_list, ty, is_varargs)),
                    loc: HashEqIgnored(ty.loc.0.until(close_paren.loc())),
                };
            }
        }
    };
    (ty, name)
}

fn resolve_typename<'a>(
    scopes: &mut Scopes<'a>,
    TypeName { ty, declarator }: &TypeName<'a>,
) -> QualifiedType<'a> {
    let ty = resolve_ty(scopes, ty);
    match declarator {
        Some(declarator) => {
            let (ty, name) = parse_declarator(scopes, ty, **declarator, IsParameter::No);
            assert_matches!(name, None);
            ty
        }
        None => ty,
    }
}

fn reresolve_ty<'a>(scopes: &mut Scopes<'a>, ty: &QualifiedType<'a>) -> QualifiedType<'a> {
    let QualifiedType { is_const, is_volatile, ty, loc } = *ty;
    let ty = match ty {
        Type::Arithmetic(_) | Type::Void | Type::Nullptr => ty,
        Type::Pointer(ty) => Type::Pointer(scopes.sess.alloc(reresolve_ty(scopes, ty))),
        Type::Array(ArrayType { ty, length, loc }) => Type::Array(ArrayType {
            ty: scopes.sess.alloc(reresolve_ty(scopes, ty)),
            length,
            loc,
        }),
        Type::Function(ty::FunctionType { params, return_type, is_varargs }) => {
            let sess = scopes.sess;
            let params = params
                .iter()
                .map(|&ParameterDeclaration { loc, ty, name }| {
                    let ty = reresolve_ty(scopes, &ty);
                    ParameterDeclaration { loc, ty, name }
                });
            let params = sess.alloc_slice_fill_iter(params);
            let return_type = sess.alloc(reresolve_ty(scopes, return_type));
            let function_ty = ty::FunctionType { params, return_type, is_varargs };
            Type::Function(function_ty)
        }
        Type::Typeof { expr, unqual, allow_bitfields } => {
            let expr = match expr.0 {
                Typeof::Expr(expr) => Typeof::Expr(expr),
                Typeof::Ty(ty) => Typeof::Ty(scopes.sess.alloc(reresolve_ty(scopes, ty))),
            };
            Type::Typeof {
                expr: NoHashEq(expr),
                unqual,
                allow_bitfields,
            }
        }
        Type::Struct(r#struct @ ty::Struct::Complete(_)) => Type::Struct(r#struct),
        Type::Struct(ty::Struct::Incomplete { name, id: _, kind }) =>
            scopes
                .lookup_or_add_struct(
                    Token::from_str(scopes.sess.bump(), panko_lex::TokenKind::Identifier, name),
                    kind,
                )
                .ty,
    };
    QualifiedType { is_const, is_volatile, ty, loc }
}

fn resolve_ty<'a>(scopes: &mut Scopes<'a>, ty: &ast::QualifiedType<'a>) -> QualifiedType<'a> {
    let ast::QualifiedType { is_const, is_volatile, ty, loc } = *ty;
    let ty = match ty {
        ast::Type::Arithmetic(arithmetic) => Type::Arithmetic(arithmetic),
        ast::Type::Void => Type::Void,
        ast::Type::Typedef(name) => {
            let QualifiedType {
                is_const: typedef_is_const,
                is_volatile: typedef_is_volatile,
                ty,
                loc,
            } = scopes.lookup_ty(name.slice()).unwrap_or_else(|| {
                unreachable!(
                    "the lexer hack makes sure that only typedef’d names will be a `typedef`’s name",
                )
            });
            return QualifiedType {
                is_const: is_const | typedef_is_const,
                is_volatile: is_volatile | typedef_is_volatile,
                ty,
                loc,
            };
        }
        ast::Type::Typeof { unqual, expr } => Type::Typeof {
            expr: NoHashEq(Typeof::Expr(scopes.sess.alloc(resolve_expr(scopes, expr)))),
            unqual,
            allow_bitfields: false,
        },
        ast::Type::TypeofTy { unqual, ty } => Type::Typeof {
            expr: NoHashEq(Typeof::Ty(scopes.sess.alloc(resolve_typename(scopes, ty)))),
            unqual,
            allow_bitfields: false,
        },
        ast::Type::Struct(r#struct) => resolve_struct(scopes, &r#struct),
    };
    let loc = HashEqIgnored(loc);
    QualifiedType { is_const, is_volatile, ty, loc }
}

fn resolve_function_ty<'a>(
    scopes: &mut Scopes<'a>,
    params: &[cst::ParameterDeclaration<'a>],
    return_type: QualifiedType<'a>,
    is_varargs: bool,
) -> FunctionType<'a> {
    let sess = scopes.sess;
    scopes.open_new_scope();
    let params = params.iter().map(
        |&cst::ParameterDeclaration { declaration_specifiers, declarator }| {
            let DeclarationSpecifiers { storage_class, function_specifiers, ty } =
                ast::parse_declaration_specifiers(sess, declaration_specifiers);
            let ty = resolve_ty(scopes, &ty);
            let (ty, name) = declarator.map_or((ty, None), |declarator| {
                parse_declarator(scopes, ty, declarator, IsParameter::Yes)
            });
            let loc = name.map_or_else(|| declaration_specifiers.loc(), |name| name.loc());

            match storage_class {
                None => (),
                Some(storage_class)
                    if let StorageClassSpecifierKind::Register = storage_class.kind =>
                    unimplemented_todo!(storage_class, "register storage class in parameters",),
                Some(storage_class) => sess.emit(Diagnostic::StorageClassInParameterDeclaration {
                    at: storage_class.token,
                    name: try { name?.slice() }.unwrap_or(NO_VALUE),
                    parameter: loc,
                }),
            }

            reject_function_specifiers(sess, &function_specifiers, loc, "function parameter");

            ParameterDeclaration { loc, ty, name }
        },
    );
    let params = sess.alloc_slice_fill_iter(params);
    scopes.exit_scope();
    let return_type = sess.alloc(return_type);

    // TODO: this makes a bunch of unnecessary allocations
    let params_by_name = params
        .iter()
        .filter_map(|param| Some((param.name?.slice(), param)))
        .into_group_map();
    for params in params_by_name.values() {
        let (param, duplicates) = params.split_first().expect("`params` is nonempty");
        for duplicate in duplicates {
            scopes.sess.emit(Diagnostic::AlreadyDefined {
                at: duplicate.loc(),
                previous_definition: param.loc(),
            })
        }
    }

    FunctionType { params, return_type, is_varargs }
}

fn resolve_struct<'a>(scopes: &mut Scopes<'a>, r#struct: &Struct<'a>) -> Type<'a> {
    let (Tagged { ty, tag, loc }, previous_decl) = match *r#struct {
        Struct::Incomplete { name, kind } => (scopes.lookup_or_add_struct(name, kind), None),
        Struct::Complete { name, kind, members } => {
            // TODO: if redeclared, check that redeclaration is valid
            scopes.lookup_or_add_complete_struct(name, kind, members)
        }
    };
    let expected = try { previous_decl?.tag }.unwrap_or(tag);
    let actual = r#struct.kind().into();
    if expected != actual {
        scopes.sess.emit(Diagnostic::TagMismatchInRedeclaration {
            at: r#struct.loc(),
            previous_decl: try { previous_decl?.loc? }
                .or(loc)
                .expect("only named structs are redeclared")
                .loc(),
            previous_ty: try { previous_decl?.ty }.unwrap_or(ty),
            expected,
            actual,
        })
    }
    ty
}

fn resolve_struct_members<'a>(
    scopes: &mut Scopes<'a>,
    members: &'a [ast::Declaration<'a, ast::Member<'a>>],
) -> &'a [NoHashEq<Member<'a>>] {
    let sess = scopes.sess;
    let members = gen {
        for member in members {
            let ast::Declaration {
                storage_class,
                function_specifiers,
                ty: unqualified_ty,
                declarators,
            } = member;
            assert_matches!(storage_class, None);
            assert_matches!(
                function_specifiers,
                FunctionSpecifiers { inline: None, noreturn: None },
            );

            let ty = resolve_ty(scopes, unqualified_ty);
            if let ast::Type::Struct(Struct::Complete { name: None, kind: _, members: _ }) =
                unqualified_ty.ty
                && declarators.is_empty()
            {
                yield NoHashEq(Member { name: None, bitfield_width: None, ty })
            }

            for ast::Member { declarator, bitfield_width } in *declarators {
                let (ty, name) = parse_declarator(scopes, ty, *declarator, IsParameter::No);
                let bitfield_width = try {
                    BitfieldWidth {
                        width: resolve_expr(scopes, bitfield_width.as_ref()?),
                    }
                };
                yield NoHashEq(Member { name, bitfield_width, ty })
            }
        }
    };
    sess.alloc_slice_collect(members)
}

fn resolve_function_definition<'a>(
    scopes: &mut Scopes<'a>,
    def: &ast::FunctionDefinition<'a>,
) -> ExternalDeclaration<'a> {
    let ast::FunctionDefinition {
        declarator,
        storage_class,
        inline,
        noreturn,
        ty,
        body,
    } = def;
    let ty = resolve_ty(scopes, ty);
    let (ty, name) = parse_declarator(scopes, ty, *declarator, IsParameter::No);
    let name = name.unwrap_or_else(|| {
        let error = Diagnostic::FunctionDefinedWithoutName { at: ty };
        let () = scopes.sess.emit(error);
        // TODO: this should be a unique name for each function for error recovery
        Token::from_str(
            scopes.sess.bump(),
            panko_lex::TokenKind::Identifier,
            "unnamed.function",
        )
    });
    let linkage = match storage_class {
        None => match inline {
            Some(_) => Linkage::Inline,
            None => Linkage::External,
        },
        Some(FunctionStorageClass::Extern) => Linkage::External,
        Some(FunctionStorageClass::Static) => Linkage::Internal,
        Some(FunctionStorageClass::Invalid(storage_class)) => {
            let error = Diagnostic::InvalidStorageClassForFunctionDefinition {
                at: storage_class.token,
                function: name,
            };
            let () = scopes.sess.emit(error);
            Linkage::External
        }
    };
    let maybe_reference = scopes.add_function(name.slice(), name.loc(), ty, linkage);
    let reference = match maybe_reference {
        Ok(reference) => {
            let initialiser = Some(RefInitialiser::FunctionBody);
            scopes.add_initialiser(&reference, initialiser);
            Reference { initialiser, ..reference }
        }
        Err(ty) =>
            return scopes.sess.emit(Diagnostic::TypedefRedeclaredAsValue {
                at: name,
                ty,
                kind: "function",
            }),
    };
    scopes.push(name.slice());

    let FunctionType { params, return_type, is_varargs } = match ty {
        QualifiedType {
            is_const: false,
            is_volatile: false,
            ty: Type::Function(function_ty),
            loc: _,
        } => function_ty,
        QualifiedType { ty: Type::Function(_), .. } =>
            unreachable!("function types cannot be qualified"),
        non_function_ty => {
            // TODO: this should be `Type::Error`
            let () = scopes
                .sess
                .emit(Diagnostic::FunctionDeclaratorDoesNotHaveFunctionType { at: name });
            FunctionType {
                params: &[],
                return_type: scopes.sess.alloc(non_function_ty),
                is_varargs: false,
            }
        }
    };

    let return_slot = scopes
        .add(
            "return",
            reference.decl_loc,
            Type::Pointer(return_type).unqualified(),
            StorageDuration::Automatic,
            IsParameter::No,
            IsInGlobalScope::No,
        )
        .expect("`return` is a keyword, so there are no types with that name");

    let varargs = is_varargs.then(|| {
        let length = NoHashEq(Some(scopes.sess.alloc(Expression::Integer {
            value: "304",
            token: Token::synthesised(
                panko_lex::TokenKind::Integer(panko_lex::Integer {
                    suffix: panko_lex::IntegerSuffix::None,
                    suffix_len: 0,
                    base: 10,
                    prefix_len: 0,
                }),
                name.loc(),
            ),
        })));
        let reg_save_area = scopes
            .add(
                "__panko_reg_save_area",
                reference.decl_loc,
                Type::Array(ArrayType {
                    ty: const { &Type::char().unqualified() },
                    length,
                    loc: HashEqIgnored(reference.decl_loc),
                })
                .unqualified(),
                StorageDuration::Automatic,
                IsParameter::No,
                IsInGlobalScope::No,
            )
            .expect("`__panko_reg_save_area` is a reserved name");
        Varargs { reg_save_area }
    });

    let sess = scopes.sess;
    let params = sess.alloc_slice_fill_iter(params.iter().enumerate().map(|(i, param)| {
        let name = param.name.map_or_else(
            || sess.alloc_str(&format!("{}.unnamed_parameter.{i}", name.slice())),
            |name| name.slice(),
        );
        scopes
            .add(
                name,
                param.loc,
                param.ty,
                StorageDuration::Automatic,
                IsParameter::Yes,
                IsInGlobalScope::No,
            )
            .expect("duplicate parameter names are filtered out by `resolve_function_ty`")
    }));

    let body = resolve_compound_statement(scopes, body, OpenNewScope::No);
    scopes.pop();
    ExternalDeclaration::FunctionDefinition(FunctionDefinition {
        reference,
        return_slot,
        params: ParamRefs(params),
        inline: *inline,
        noreturn: *noreturn,
        varargs,
        body,
    })
}

fn resolve_compound_statement<'a>(
    scopes: &mut Scopes<'a>,
    stmts: &ast::CompoundStatement<'a>,
    open_new_scope: OpenNewScope,
) -> CompoundStatement<'a> {
    if let OpenNewScope::Yes = open_new_scope {
        scopes.open_new_scope();
    }
    let sess = scopes.sess;
    let stmts = gen {
        for stmt in stmts.0 {
            for stmt in resolve_stmt(scopes, stmt) {
                yield stmt;
            }
        }
    };
    let stmts = CompoundStatement(sess.alloc_slice_collect(stmts));
    if let OpenNewScope::Yes = open_new_scope {
        scopes.exit_scope();
    }
    stmts
}

fn resolve_designated_initialiser<'a>(
    scopes: &mut Scopes<'a>,
    initialiser: &ast::DesignatedInitialiser<'a>,
) -> DesignatedInitialiser<'a> {
    let ast::DesignatedInitialiser { designation, initialiser } = initialiser;
    let designation = try {
        Designation(
            scopes
                .sess
                .alloc_slice_fill_iter(designation.as_ref()?.0.iter().map(|designator| {
                    match designator {
                        cst::Designator::Bracketed { open_bracket, index, close_bracket } =>
                            Designator::Bracketed {
                                open_bracket: *open_bracket,
                                index: resolve_expr(scopes, index),
                                close_bracket: *close_bracket,
                            },
                        cst::Designator::Identifier { dot, ident } =>
                            Designator::Identifier { dot: *dot, ident: *ident },
                    }
                })),
        )
    };
    let initialiser = match initialiser {
        ast::Initialiser::Braced {
            open_brace,
            initialiser_list,
            close_brace,
        } => Initialiser::Braced {
            open_brace: *open_brace,
            initialiser_list: scopes.sess.alloc_slice_fill_iter(
                initialiser_list
                    .iter()
                    .map(|initialiser| resolve_designated_initialiser(scopes, initialiser)),
            ),
            close_brace: *close_brace,
        },
        ast::Initialiser::Expression(expression) =>
            Initialiser::Expression(resolve_expr(scopes, expression)),
    };
    DesignatedInitialiser {
        designation,
        initialiser: scopes.sess.alloc(initialiser),
    }
}

struct InitDeclarator<'a> {
    ty: QualifiedType<'a>,
    name: Token<'a>,
    bitfield_width: Option<cst::Expression<'a>>,
    initialiser: Option<ast::Initialiser<'a>>,
}

fn reject_bitfield_width<'a>(
    scopes: &mut Scopes<'a>,
    bitfield_width: Option<&cst::Expression<'a>>,
    decl_loc: Loc<'a>,
) {
    if let Some(bitfield_width) = bitfield_width {
        let _ = resolve_expr(scopes, bitfield_width);
        scopes
            .sess
            .emit(cst::BitfieldDiagnostic::NonStructMemberBitfield {
                at: *bitfield_width,
                decl_loc,
                kind: match scopes.is_in_global_scope() {
                    IsInGlobalScope::Yes => "global variable",
                    IsInGlobalScope::No => "local variable",
                },
            })
    }
}

fn resolve_initialiser<'a>(
    scopes: &mut Scopes<'a>,
    initialiser: &ast::Initialiser<'a>,
) -> Initialiser<'a> {
    match initialiser {
        ast::Initialiser::Braced {
            open_brace,
            initialiser_list,
            close_brace,
        } => Initialiser::Braced {
            open_brace: *open_brace,
            initialiser_list: scopes.sess.alloc_slice_fill_iter(
                initialiser_list
                    .iter()
                    .map(|initialiser| resolve_designated_initialiser(scopes, initialiser)),
            ),
            close_brace: *close_brace,
        },
        ast::Initialiser::Expression(initialiser) =>
            Initialiser::Expression(resolve_expr(scopes, initialiser)),
    }
}

fn resolve_typedef_declaration<'a>(
    scopes: &mut Scopes<'a>,
    storage_duration: StorageDuration<Option<Linkage>>,
    function_specifiers: &FunctionSpecifiers<'a>,
    declarator: InitDeclarator<'a>,
) -> Declarator<'a> {
    let InitDeclarator { ty, name, bitfield_width, initialiser } = declarator;
    assert_matches!(
        storage_duration,
        StorageDuration::Static(Some(Linkage::None))
    );
    reject_function_specifiers(scopes.sess, function_specifiers, name.loc(), "type alias");

    let previously_declared_as = scopes.add_ty(name.slice(), ty);

    reject_bitfield_width(scopes, bitfield_width.as_ref(), name.loc());
    if let Some(initialiser) = initialiser {
        let initialiser = resolve_initialiser(scopes, &initialiser);
        error_todo!(initialiser, "typedef with initialiser")
    }

    match previously_declared_as {
        Ok(previously_declared_as) =>
            Declarator::Typedef(Typedef { ty, name, previously_declared_as }),
        Err(reference) => Declarator::Redeclared(Redeclared::ValueAsTypedef { at: ty, reference }),
    }
}

fn resolve_value_declaration<'a>(
    scopes: &mut Scopes<'a>,
    storage_duration: StorageDuration<Option<Linkage>>,
    function_specifiers: &FunctionSpecifiers<'a>,
    declarator: InitDeclarator<'a>,
) -> Declarator<'a> {
    let InitDeclarator { ty, name, bitfield_width, initialiser } = declarator;
    let maybe_reference = scopes.add(
        name.slice(),
        name.loc(),
        ty,
        storage_duration,
        IsParameter::No,
        scopes.is_in_global_scope(),
    );
    let reference = match maybe_reference {
        Ok(reference) => reference,
        Err(typedef_ty) => {
            return Declarator::Redeclared(Redeclared::TypedefAsValue {
                at: name,
                typedef_ty,
                value_ty: ty,
            });
        }
    };
    // TODO: move resolving the initialiser into `Scopes::add` so that the `add_initialiser` call
    // cannot be forgotten
    let initialiser = try {
        scopes
            .sess
            .alloc(resolve_initialiser(scopes, &initialiser?))
    };
    let ref_initialiser = initialiser.map(RefInitialiser::Initialiser);
    scopes.add_initialiser(&reference, ref_initialiser);

    reject_bitfield_width(
        scopes,
        bitfield_width.as_ref(),
        reference.ty.loc().until(reference.decl_loc),
    );

    Declarator::Declaration(Declaration {
        function_specifiers: *function_specifiers,
        reference: Reference {
            initialiser: ref_initialiser,
            ..reference
        },
        initialiser,
    })
}

fn resolve_declaration<'a>(
    scopes: &mut Scopes<'a>,
    decl: &ast::Declaration<'a, cst::InitDeclarator<'a>>,
) -> Declarators<'a> {
    let sess = scopes.sess;
    let ast::Declaration {
        storage_class,
        function_specifiers,
        ty: unresolved_ty,
        declarators,
    } = decl;

    let ty = resolve_ty(scopes, unresolved_ty);

    let linkage = match try { storage_class.as_ref()?.kind } {
        Some(StorageClassSpecifierKind::Typedef) => Some(Linkage::None),
        Some(StorageClassSpecifierKind::Extern) => Some(Linkage::External),
        Some(StorageClassSpecifierKind::Static) => match scopes.is_in_global_scope() {
            IsInGlobalScope::Yes => Some(Linkage::Internal),
            IsInGlobalScope::No => Some(Linkage::None),
        },
        Some(kind) => unimplemented_todo!(
            storage_class.unwrap(),
            "not implemented: storage class {:?}",
            kind,
        ),
        None => None,
    };
    let storage_duration = match linkage {
        Some(linkage) => StorageDuration::Static(Some(linkage)),
        None => match scopes.is_in_global_scope() {
            IsInGlobalScope::Yes => StorageDuration::Static(None),
            IsInGlobalScope::No => StorageDuration::Automatic,
        },
    };
    let resolve = match try { storage_class.as_ref()?.kind } {
        Some(StorageClassSpecifierKind::Typedef) => resolve_typedef_declaration,
        _ => resolve_value_declaration,
    };

    let declarators = declarators.iter().map(|&init_declarator| {
        let cst::InitDeclarator { declarator, bitfield_width, initialiser } = init_declarator;
        let (ty, name) = parse_declarator(scopes, ty, declarator, IsParameter::No);
        let name = match name {
            Some(name) => name,
            // TODO: 6.7.1: reject struct/enum decl without tag, allow enum decl without tag that
            // contains an enumerator list
            None => {
                let loc = unresolved_ty.loc().until_maybe(
                    try { initialiser.as_ref()?.loc() }
                        .or_else(|| declarator.direct_declarator.maybe_end_loc()),
                );
                // TODO: use this error
                let () = sess.emit(Diagnostic::DeclarationWithoutName { at: loc, ty });
                // TODO: this should be a unique name for each value for error recovery
                Token::from_str(
                    scopes.sess.bump(),
                    panko_lex::TokenKind::Identifier,
                    "unnamed.declarator",
                )
            }
        };
        resolve(
            scopes,
            storage_duration,
            function_specifiers,
            InitDeclarator { ty, name, bitfield_width, initialiser },
        )
    });
    Declarators {
        ty,
        unresolved_ty: *unresolved_ty,
        declarators: sess.alloc_slice_fill_iter(declarators),
    }
}

gen fn resolve_stmt<'a>(scopes: &mut Scopes<'a>, stmt: &ast::Statement<'a>) -> Statement<'a> {
    let stmt = match stmt {
        ast::Statement::Declaration(decl) =>
            Statement::Declaration(resolve_declaration(scopes, decl)),
        ast::Statement::Expression(expr) =>
            Statement::Expression(try { resolve_expr(scopes, expr.as_ref()?) }),
        ast::Statement::Compound(stmts) =>
            Statement::Compound(resolve_compound_statement(scopes, stmts, OpenNewScope::Yes)),
        ast::Statement::Return { return_, expr } => Statement::Return {
            return_: *return_,
            expr: try { resolve_expr(scopes, expr.as_ref()?) },
        },
    };
    for hoisted_ref in scopes.take_hoisted_compound_literals() {
        yield Statement::HoistedCompoundLiteral(hoisted_ref);
    }
    yield stmt;
}

fn resolve_assoc<'a>(
    scopes: &mut Scopes<'a>,
    assoc: &ast::GenericAssociation<'a>,
) -> GenericAssociation<'a> {
    match assoc {
        ast::GenericAssociation::Ty { ty, expr } => GenericAssociation::Ty {
            ty: resolve_typename(scopes, ty),
            expr: resolve_expr(scopes, expr),
        },
        ast::GenericAssociation::Default { default, expr } => GenericAssociation::Default {
            default: *default,
            expr: resolve_expr(scopes, expr),
        },
    }
}

fn resolve_expr<'a>(scopes: &mut Scopes<'a>, expr: &ast::Expression<'a>) -> Expression<'a> {
    match expr {
        ast::Expression::Error(error) => Expression::Error(*error),
        ast::Expression::Name(name) => try {
            scopes
                .lookup(name.slice(), name.loc())?
                .map_left(Expression::Name)
                .map_right(Expression::BuiltinName)
                .into_inner()
        }
        .unwrap_or_else(|| scopes.sess.emit(Diagnostic::UndeclaredName { at: *name })),
        ast::Expression::Integer(token) =>
            Expression::Integer { value: token.slice(), token: *token },
        ast::Expression::CharConstant(char) => Expression::CharConstant(*char),
        ast::Expression::String(tokens) => Expression::String(tokens),
        ast::Expression::Nullptr(nullptr) => Expression::Nullptr(*nullptr),
        ast::Expression::Parenthesised { open_paren, expr, close_paren } =>
            Expression::Parenthesised {
                open_paren: *open_paren,
                expr: scopes.sess.alloc(resolve_expr(scopes, expr)),
                close_paren: *close_paren,
            },
        ast::Expression::Assign { target, value } => Expression::Assign {
            target: scopes.sess.alloc(resolve_expr(scopes, target)),
            value: scopes.sess.alloc(resolve_expr(scopes, value)),
        },
        ast::Expression::CompoundAssign { target, op, value } => {
            let target = scopes.sess.alloc(resolve_expr(scopes, target));
            let target_temporary = scopes.temporary(target.loc(), Type::Void.unqualified());
            Expression::CompoundAssign {
                target,
                target_temporary,
                op: *op,
                value: scopes.sess.alloc(resolve_expr(scopes, value)),
            }
        }
        ast::Expression::BinOp { lhs, op, rhs } => Expression::BinOp {
            lhs: scopes.sess.alloc(resolve_expr(scopes, lhs)),
            op: *op,
            rhs: scopes.sess.alloc(resolve_expr(scopes, rhs)),
        },
        ast::Expression::UnaryOp { operator, operand } => Expression::UnaryOp {
            operator: *operator,
            operand: scopes.sess.alloc(resolve_expr(scopes, operand)),
        },
        ast::Expression::Call { callee, args, close_paren } => Expression::Call {
            callee: scopes.sess.alloc(resolve_expr(scopes, callee)),
            args: scopes
                .sess
                .alloc_slice_fill_iter(args.iter().map(|arg| resolve_expr(scopes, arg))),
            close_paren: *close_paren,
        },
        ast::Expression::Sizeof { sizeof, ty, close_paren } => Expression::Sizeof {
            sizeof: *sizeof,
            ty: resolve_typename(scopes, ty),
            close_paren: *close_paren,
        },
        ast::Expression::Lengthof { lengthof, ty, close_paren } => Expression::Lengthof {
            lengthof: *lengthof,
            ty: resolve_typename(scopes, ty),
            close_paren: *close_paren,
        },
        ast::Expression::Alignof { alignof, ty, close_paren } => Expression::Alignof {
            alignof: *alignof,
            ty: resolve_typename(scopes, ty),
            close_paren: *close_paren,
        },
        ast::Expression::Cast { open_paren, ty, expr } => Expression::Cast {
            open_paren: *open_paren,
            ty: resolve_typename(scopes, ty),
            expr: scopes.sess.alloc(resolve_expr(scopes, expr)),
        },
        ast::Expression::Subscript { lhs, rhs, close_bracket } => Expression::Subscript {
            lhs: scopes.sess.alloc(resolve_expr(scopes, lhs)),
            rhs: scopes.sess.alloc(resolve_expr(scopes, rhs)),
            close_bracket: *close_bracket,
        },
        ast::Expression::Generic { generic, selector, assocs, close_paren } =>
            Expression::Generic {
                generic: *generic,
                selector: scopes.sess.alloc(resolve_expr(scopes, selector)),
                assocs: GenericAssocList(scopes.sess.alloc_slice_fill_iter(
                    assocs.0.iter().map(|assoc| resolve_assoc(scopes, assoc)),
                )),
                close_paren: *close_paren,
            },
        ast::Expression::Logical { lhs, op, rhs } => Expression::Logical {
            lhs: scopes.sess.alloc(resolve_expr(scopes, lhs)),
            op: *op,
            rhs: scopes.sess.alloc(resolve_expr(scopes, rhs)),
        },
        ast::Expression::Conditional { condition, question_mark, then, or_else } =>
            Expression::Conditional {
                condition: scopes.sess.alloc(resolve_expr(scopes, condition)),
                question_mark: *question_mark,
                then: scopes.sess.alloc(resolve_expr(scopes, then)),
                or_else: scopes.sess.alloc(resolve_expr(scopes, or_else)),
            },
        ast::Expression::Comma { lhs, rhs } => Expression::Comma {
            lhs: scopes.sess.alloc(resolve_expr(scopes, lhs)),
            rhs: scopes.sess.alloc(resolve_expr(scopes, rhs)),
        },
        ast::Expression::Increment { operator, operand, fixity } => {
            let operand = scopes.sess.alloc(resolve_expr(scopes, operand));
            let typeof_copy = Type::Typeof {
                expr: NoHashEq(Typeof::Expr(operand)),
                unqual: true,
                allow_bitfields: true,
            };
            let (member, operand) = match operand {
                Expression::MemberAccess { lhs, op, member }
                    if matches!(fixity, cst::IncrementFixity::Postfix) =>
                {
                    let operand = match op.kind {
                        MemberAccessOpKind::Dot => lhs,
                        MemberAccessOpKind::Arrow => scopes.sess.alloc(Expression::UnaryOp {
                            operator: UnaryOp {
                                kind: UnaryOpKind::Deref,
                                token: op.token,
                            },
                            operand: lhs,
                        }),
                    };
                    (Some(member), operand)
                }
                _ => (None, operand),
            };
            let reference = scopes.temporary(operand.loc(), Type::Void.unqualified());
            let typeof_pointer = Type::Typeof {
                expr: NoHashEq(Typeof::Expr(operand)),
                unqual: false,
                allow_bitfields: false,
            };
            Expression::Increment {
                operator: *operator,
                operand,
                fixity: match fixity {
                    cst::IncrementFixity::Prefix => IncrementFixity::Prefix,
                    cst::IncrementFixity::Postfix => IncrementFixity::Postfix {
                        member,
                        pointer: scopes.temporary(
                            operand.loc(),
                            Type::Pointer(scopes.sess.alloc(typeof_pointer.unqualified()))
                                .unqualified(),
                        ),
                        copy: scopes.temporary(operand.loc(), typeof_copy.unqualified()),
                    },
                },
                reference,
            }
        }
        ast::Expression::MemberAccess { lhs, op, member } => Expression::MemberAccess {
            lhs: scopes.sess.alloc(resolve_expr(scopes, lhs)),
            op: *op,
            member: *member,
        },
        ast::Expression::BuiltinOffsetof {
            builtin_offsetof,
            ty,
            member,
            close_paren,
        } => Expression::BuiltinOffsetof {
            builtin_offsetof: *builtin_offsetof,
            ty: resolve_typename(scopes, ty),
            member: *member,
            close_paren: *close_paren,
        },
        ast::Expression::CompoundLiteral {
            open_paren,
            storage_class_specifiers,
            ty: TypeName { ty, declarator },
            initialiser,
        } => {
            if storage_class_specifiers.len() > 1 {
                error_todo!(
                    expr,
                    "multiple storage class specifiers not implemented for compound literals",
                )
            }
            // TODO: reject invalid storage class specifiers
            let storage_class = storage_class_specifiers.first().copied();
            let name = format!("compound_literal.{}", scopes.id().0);
            let name = scopes.sess.alloc_str(&name);
            let name = Token::from_str(scopes.sess.bump(), panko_lex::TokenKind::Identifier, name);
            let cst::Declarator { pointers, direct_declarator } =
                *declarator.unwrap_or(&cst::Declarator {
                    pointers: None,
                    direct_declarator: DirectDeclarator::Abstract,
                });
            let declarators = scopes.sess.alloc([ast::InitDeclarator {
                declarator: cst::Declarator {
                    pointers,
                    direct_declarator: direct_declarator.with_name(scopes.sess, name).unwrap(),
                },
                bitfield_width: None,
                initialiser: Some(**initialiser),
            }]);
            let decl = ast::Declaration {
                storage_class,
                function_specifiers: FunctionSpecifiers { inline: None, noreturn: None },
                ty: *ty,
                declarators,
            };
            let decl = match resolve_declaration(scopes, &decl) {
                Declarators {
                    ty: _,
                    unresolved_ty: _,
                    declarators: [Declarator::Declaration(decl)],
                } => Some(*decl),
                _ => None,
            }
            .into_iter()
            .exactly_one()
            .unwrap_or_else(|_| panic!("compound literals have exactly one declarator"));
            scopes.hoist_compound_literal(decl.reference);
            Expression::CompoundLiteral { open_paren: *open_paren, decl }
        }
    }
}

fn resolve_external_declaration<'a>(
    scopes: &mut Scopes<'a>,
    decl: &ast::ExternalDeclaration<'a>,
) -> ExternalDeclaration<'a> {
    match decl {
        ast::ExternalDeclaration::FunctionDefinition(def) =>
            resolve_function_definition(scopes, def),
        ast::ExternalDeclaration::Declaration(decl) =>
            ExternalDeclaration::Declaration(resolve_declaration(scopes, decl)),
    }
}

pub fn resolve_names<'a>(
    sess: &'a Session<'a>,
    translation_unit: ast::TranslationUnit<'a>,
) -> TranslationUnit<'a> {
    let scopes = &mut Scopes::new(sess);
    let ast::TranslationUnit { filename, decls } = translation_unit;
    TranslationUnit {
        filename,
        decls: sess.alloc_slice_fill_iter(
            decls
                .iter()
                .map(|decl| resolve_external_declaration(scopes, decl)),
        ),
    }
}
