use std::cmp::Ordering;
use std::fmt;

use ariadne::Color::Blue;
use ariadne::Color::Green;
use ariadne::Color::Red;
use ariadne::Fmt as _;
use itertools::EitherOrBoth;
use itertools::Itertools as _;
use panko_lex::Integer;
use panko_lex::IntegerSuffix;
use panko_lex::Loc;
use panko_lex::Token;
use panko_lex::TokenKind;
use panko_parser as cst;
use panko_parser::ast::Arithmetic;
use panko_parser::ast::Integral;
use panko_parser::ast::IntegralKind;
use panko_parser::ast::Session;
use panko_parser::ast::Signedness;
use panko_parser::sexpr_builder::AsSExpr as _;
use panko_parser::BinOpKind;
use panko_parser::UnaryOp;
use panko_parser::UnaryOpKind;
use panko_report::Report;
use variant_types::IntoVariant as _;

use crate::scope;
use crate::scope::GenericAssociation;
use crate::scope::ParamRefs;
use crate::scope::Reference;
use crate::ty::FunctionType;
use crate::ty::QualifiedType;
use crate::ty::Type;

mod as_sexpr;
#[cfg(test)]
mod tests;

// TODO: this should be in `panko_report`
trait Sliced {
    fn slice(&self) -> String;
}

impl<T> Sliced for T
where
    T: ToString,
{
    fn slice(&self) -> String {
        self.to_string()
    }
}

#[derive(Debug, Report)]
#[exit_code(1)]
enum Diagnostic<'a> {
    // TODO: colourise types
    // TODO: types should be printed in C syntax, not in their SExpr debug repr
    #[error("invalid {kind} conversion from `{from_ty}` to `{to_ty}`")]
    #[diagnostics(at(colour = Red, label = "this is of type `{from_ty}`, which cannot be {kind}ly converted to `{to_ty}`"))]
    InvalidConversion {
        at: TypedExpression<'a>,
        from_ty: Type<'a>,
        to_ty: Type<'a>,
        kind: ConversionKind,
    },

    #[error("`{return_}` statement without value in non-`void` function `{name}` returning `{return_ty}`")]
    #[diagnostics(
        at(colour = Red, label = "`{return_}` statement here"),
        function(colour = Blue, label = "function `{name}` declared here"),
    )]
    #[with(
        return_ = at.return_,
        name = function.reference,
        return_ty = function.return_ty(),
    )]
    ReturnWithoutValueInNonVoidFunction {
        at: scope::StatementTypes::Return<'a>,
        function: scope::FunctionDefinition<'a>,
    },

    #[error("cannot assign to `const` value `{decl}`")]
    #[diagnostics(
        at(colour = Red, label = "this value is `const`"),
        decl(colour = Blue, label = "note: `{decl}` declared here")
    )]
    AssignmentToConstName {
        at: &'a Expression<'a>,
        decl: Reference<'a>,
    },

    #[error("cannot assign to `const` expression")]
    #[diagnostics(
        at(colour = Red, label = "this value is `const`"),
    )]
    AssignmentToConst { at: Expression<'a> },

    #[error("cannot assign to this expression because it is not an lvalue")]
    #[diagnostics(at(colour = Red, label = "this expression is not an lvalue"))]
    AssignmentToNonLValue { at: &'a Expression<'a> },

    #[error("dereference of pointer to `void`")]
    #[diagnostics(at(colour = Red, label = "this expression has type `{ty}`"))]
    #[with(ty = at.ty.ty)]
    DerefOfVoidPtr { at: TypedExpression<'a> },

    #[error("invalid integer suffix `{suffix}`")]
    #[diagnostics(at(colour = Red, label = "invalid integer suffix"))]
    #[with(suffix = suffix.fg(Red))]
    InvalidIntegerSuffix { at: Token<'a>, suffix: &'a str },

    #[error("invalid application of `{op}` to {kind} `{ty}`")]
    #[diagnostics(at(colour = Red, label = "in this expression"), op(colour = Red))]
    #[with(ty = ty.fg(Blue))]
    InvalidSizeofOrAlignof {
        op: Token<'a>,
        at: scope::Expression<'a>,
        kind: &'static str,
        ty: QualifiedType<'a>,
    },

    #[error(
        "cannot dereference this expression with type `{ty}` (pointer or array type required)"
    )]
    #[diagnostics(at(colour = Red, label = "in this expression"))]
    #[with(ty = ty.fg(Red))]
    CannotDeref {
        at: scope::Expression<'a>,
        ty: QualifiedType<'a>,
    },

    #[error("duplicate `{at}` association in {generic} selection")]
    #[diagnostics(
        generic(colour = Green, label = "in this {generic} selection"),
        previous_default(colour = Blue, label = "first `{previous_default}` here"),
        at(colour = Red, label = "duplicate `{at}`"),
    )]
    GenericWithDuplicateDefault {
        at: Token<'a>,
        previous_default: Token<'a>,
        generic: Token<'a>,
    },

    #[error("duplicate association for type `{at}` in {generic} selection")]
    #[diagnostics(
        generic(colour = Green, label = "in this {generic} selection"),
        previous(colour = Blue, label = "previous association for `{previous}` here"),
        at(colour = Red, label = "this type is duplicated in this {generic} selection"),
    )]
    GenericWithDuplicateType {
        at: QualifiedType<'a>,
        previous: QualifiedType<'a>,
        generic: Token<'a>,
    },

    #[error("controlling expression does not match any type in this {generic} selection")]
    #[diagnostics(
        at(colour = Red, label = "controlling expression has type `{ty}`"),
        expr(colour = Blue, label = "in this {generic} selection"),
    )]
    #[with(ty = at.ty.ty, generic = "_Generic".fg(Blue))]
    GenericWithoutMatch {
        at: TypedExpression<'a>,
        expr: scope::Expression<'a>,
    },

    #[error("the type of a {generic} association shall be a complete object type other than a variably modified type")]
    #[diagnostics(
        generic(colour = Green, label = "in this {generic} selection"),
        at(colour = Red, label = "this type is not allowed in a {generic} association"),
    )]
    GenericWithInvalidType {
        at: QualifiedType<'a>,
        generic: Token<'a>,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct TranslationUnit<'a> {
    pub decls: &'a [ExternalDeclaration<'a>],
}

#[derive(Debug, Clone, Copy)]
pub enum ExternalDeclaration<'a> {
    FunctionDefinition(FunctionDefinition<'a>),
    Declaration(Declaration<'a>),
}

#[derive(Debug, Clone, Copy)]
pub struct Declaration<'a> {
    pub reference: Reference<'a>,
    pub initialiser: Option<TypedExpression<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionDefinition<'a> {
    pub(crate) reference: Reference<'a>,
    pub(crate) params: ParamRefs<'a>,
    pub(crate) storage_class: Option<cst::StorageClassSpecifier<'a>>,
    pub(crate) inline: Option<cst::FunctionSpecifier<'a>>,
    pub(crate) noreturn: Option<cst::FunctionSpecifier<'a>>,
    pub(crate) is_varargs: bool,
    pub(crate) body: CompoundStatement<'a>,
}

#[derive(Debug, Clone, Copy)]
pub struct CompoundStatement<'a>(pub &'a [Statement<'a>]);

#[derive(Debug, Clone, Copy)]
pub enum Statement<'a> {
    Declaration(Declaration<'a>),
    Expression(Option<TypedExpression<'a>>),
    Compound(CompoundStatement<'a>),
    Return(Option<TypedExpression<'a>>),
}

#[derive(Debug, Clone, Copy)]
pub struct TypedExpression<'a> {
    pub(crate) ty: QualifiedType<'a>,
    pub expr: Expression<'a>,
}

#[derive(Debug, Clone, Copy)]
pub enum Expression<'a> {
    Name(Reference<'a>),
    Integer {
        value: u64,
        token: Token<'a>,
    },
    NoopTypeConversion(&'a TypedExpression<'a>),
    // TODO: `Truncate`, `SignExtend`, `ZeroExtend` and `VoidCast` lose location information when
    // representing an explicit cast. They should take an optional location (or better:
    // `TypedExpression` should have a `loc`).
    Truncate(&'a TypedExpression<'a>),
    SignExtend(&'a TypedExpression<'a>),
    ZeroExtend(&'a TypedExpression<'a>),
    VoidCast(&'a TypedExpression<'a>),
    Parenthesised {
        open_paren: Token<'a>,
        expr: &'a TypedExpression<'a>,
        close_paren: Token<'a>,
    },
    Assign {
        target: &'a TypedExpression<'a>,
        value: &'a TypedExpression<'a>,
    },
    IntegralBinOp {
        ty: Integral,
        lhs: &'a TypedExpression<'a>,
        kind: BinOpKind,
        rhs: &'a TypedExpression<'a>,
    },
    PtrAdd {
        pointer: &'a TypedExpression<'a>,
        integral: &'a TypedExpression<'a>,
        pointee_size: u64,
        order: PtrAddOrder,
    },
    PtrSub {
        pointer: &'a TypedExpression<'a>,
        integral: &'a TypedExpression<'a>,
        pointee_size: u64,
    },
    PtrCmp {
        lhs: &'a TypedExpression<'a>,
        kind: PtrCmpKind,
        rhs: &'a TypedExpression<'a>,
    },
    Addressof {
        ampersand: Token<'a>,
        operand: &'a TypedExpression<'a>,
    },
    Deref {
        star: Token<'a>,
        operand: &'a TypedExpression<'a>,
    },
    Call {
        callee: &'a TypedExpression<'a>,
        args: &'a [TypedExpression<'a>],
        is_varargs: bool,
        close_paren: Token<'a>,
    },
    Negate {
        minus: Token<'a>,
        operand: &'a TypedExpression<'a>,
    },
    Compl {
        compl: Token<'a>,
        operand: &'a TypedExpression<'a>,
    },
    Not {
        not: Token<'a>,
        operand: &'a TypedExpression<'a>,
    },
    Sizeof {
        sizeof: Token<'a>,
        operand: &'a TypedExpression<'a>,
        size: u64,
    },
    SizeofTy {
        sizeof: Token<'a>,
        ty: QualifiedType<'a>,
        size: u64,
        close_paren: Token<'a>,
    },
    Alignof {
        alignof: Token<'a>,
        ty: QualifiedType<'a>,
        align: u64,
        close_paren: Token<'a>,
    },
}

impl<'a> TypedExpression<'a> {
    fn loc(&self) -> Loc<'a> {
        self.expr.loc()
    }

    fn is_modifiable_lvalue(&self) -> bool {
        self.is_lvalue() && self.is_modifiable()
    }

    fn is_lvalue(&self) -> bool {
        match self.expr {
            Expression::Name(reference) =>
                reference.ty.ty.is_object() && !matches!(reference.ty.ty, Type::Void),
            Expression::Parenthesised { open_paren: _, expr, close_paren: _ } => expr.is_lvalue(),
            Expression::Deref { .. } => self.ty.ty.is_object() && !matches!(self.ty.ty, Type::Void),
            Expression::Integer { .. }
            | Expression::NoopTypeConversion(_)
            | Expression::Truncate(_)
            | Expression::SignExtend(_)
            | Expression::ZeroExtend(_)
            | Expression::VoidCast(_)
            | Expression::Assign { .. }
            | Expression::IntegralBinOp { .. }
            | Expression::PtrAdd { .. }
            | Expression::PtrSub { .. }
            | Expression::PtrCmp { .. }
            | Expression::Addressof { .. }
            | Expression::Call { .. }
            | Expression::Negate { .. }
            | Expression::Compl { .. }
            | Expression::Not { .. }
            | Expression::Sizeof { .. }
            | Expression::SizeofTy { .. }
            | Expression::Alignof { .. } => false,
        }
    }

    fn is_modifiable(&self) -> bool {
        !self.ty.is_const
    }
}

impl<'a> Expression<'a> {
    fn loc(&self) -> Loc<'a> {
        match self {
            Expression::Name(reference) => reference.loc(),
            Expression::Integer { value: _, token } => token.loc(),
            Expression::NoopTypeConversion(inner)
            | Expression::Truncate(inner)
            | Expression::SignExtend(inner)
            | Expression::ZeroExtend(inner)
            | Expression::VoidCast(inner) => inner.loc(),
            Expression::Assign { target, value } => target.loc().until(value.loc()),
            Expression::Parenthesised { open_paren, expr: _, close_paren } =>
                open_paren.loc().until(close_paren.loc()),
            Expression::IntegralBinOp { ty: _, lhs, kind: _, rhs } => lhs.loc().until(rhs.loc()),
            Expression::PtrAdd {
                pointer,
                integral,
                pointee_size: _,
                order,
            } => {
                let (lhs, rhs) = order.select(pointer, integral);
                lhs.loc().until(rhs.loc())
            }
            Expression::PtrSub { pointer, integral, pointee_size: _ } =>
                pointer.loc().until(integral.loc()),
            Expression::PtrCmp { lhs, kind: _, rhs } => lhs.loc().until(rhs.loc()),
            Expression::Addressof { ampersand, operand } => ampersand.loc().until(operand.loc()),
            Expression::Deref { star, operand } => star.loc().until(operand.loc()),
            Expression::Call {
                callee,
                args: _,
                is_varargs: _,
                close_paren,
            } => callee.loc().until(close_paren.loc()),
            Expression::Negate { minus, operand } => minus.loc().until(operand.loc()),
            Expression::Compl { compl, operand } => compl.loc().until(operand.loc()),
            Expression::Not { not, operand } => not.loc().until(operand.loc()),
            Expression::Sizeof { sizeof, operand, size: _ } => sizeof.loc().until(operand.loc()),
            Expression::SizeofTy { sizeof, ty: _, size: _, close_paren } =>
                sizeof.loc().until(close_paren.loc()),
            Expression::Alignof { alignof, ty: _, align: _, close_paren } =>
                alignof.loc().until(close_paren.loc()),
        }
    }

    fn unwrap_parens(&self) -> &Self {
        match self {
            Expression::Parenthesised { open_paren: _, expr, close_paren: _ } =>
                expr.expr.unwrap_parens(),
            _ => self,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ConversionKind {
    Implicit,
    Explicit,
}

impl fmt::Display for ConversionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Self::Implicit => "implicit",
            Self::Explicit => "explicit",
        };
        write!(f, "{s}")
    }
}

fn convert<'a>(
    sess: &'a Session<'a>,
    target: QualifiedType<'a>,
    expr: TypedExpression<'a>,
    kind: ConversionKind,
) -> TypedExpression<'a> {
    // TODO: forbid ptr <=> float
    // TODO: only allow nullptr => {bool, void, ptr<T>}
    // TODO: if target is nullptr_t, expr must be nullptr or a null pointer constant
    // TODO: check that target is a scalar type or void
    // TODO: check that expr_ty is a scalar type when target_ty != void
    let target_ty = target.ty;
    let expr_ty = expr.ty.ty;
    let extend_kind = match expr_ty {
        Type::Arithmetic(arithmetic) => match arithmetic.signedness() {
            Signedness::Signed => Expression::SignExtend,
            Signedness::Unsigned => Expression::ZeroExtend,
        },
        _ => Expression::ZeroExtend,
    };
    let convert = || {
        let cast = match target_ty.size().cmp(&expr_ty.size()) {
            Ordering::Less => Expression::Truncate,
            Ordering::Equal => Expression::NoopTypeConversion,
            Ordering::Greater => extend_kind,
        };
        cast(sess.alloc(expr))
    };

    let expr = match (target_ty, expr_ty) {
        (Type::Void, Type::Void) => return expr,
        (Type::Void, _) if kind == ConversionKind::Explicit =>
            Expression::VoidCast(sess.alloc(expr)),

        (Type::Arithmetic(_), Type::Arithmetic(_)) | (Type::Pointer(_), Type::Pointer(_)) =>
            if kind == ConversionKind::Implicit && target_ty == expr_ty {
                return expr;
            }
            else {
                convert()
            },
        // TODO: clang (but not gcc) allows implicitly converting `Type::Function(_)` to
        // `Type::Pointer(_)` (with a warning).
        // TODO: handle nullptr literals
        (Type::Function(_), _) => todo!("[{}] = {}", target.as_sexpr(), expr.as_sexpr()),

        (Type::Arithmetic(Arithmetic::Integral(_)), Type::Pointer(_))
        | (Type::Pointer(_), Type::Arithmetic(Arithmetic::Integral(_)))
            if kind == ConversionKind::Explicit =>
            convert(),

        _ => {
            sess.emit(Diagnostic::InvalidConversion {
                at: expr,
                from_ty: expr_ty,
                to_ty: target_ty,
                kind,
            });
            expr.expr
        }
    };
    TypedExpression { ty: target_ty.unqualified(), expr }
}

fn convert_as_if_by_assignment<'a>(
    sess: &'a Session<'a>,
    target: QualifiedType<'a>,
    expr: TypedExpression<'a>,
) -> TypedExpression<'a> {
    convert(sess, target, expr, ConversionKind::Implicit)
}

fn typeck_function_definition<'a>(
    sess: &'a Session<'a>,
    definition: &scope::FunctionDefinition<'a>,
) -> FunctionDefinition<'a> {
    let scope::FunctionDefinition {
        reference,
        params,
        storage_class,
        inline,
        noreturn,
        is_varargs,
        body,
    } = *definition;

    FunctionDefinition {
        reference,
        params,
        storage_class,
        inline,
        noreturn,
        is_varargs,
        body: typeck_compound_statement(sess, &body, definition),
    }
}

fn typeck_declaration<'a>(
    sess: &'a Session<'a>,
    declaration: &scope::Declaration<'a>,
) -> Declaration<'a> {
    let scope::Declaration { reference, initialiser } = *declaration;
    let initialiser = initialiser.as_ref().map(|initialiser| {
        convert_as_if_by_assignment(
            sess,
            reference.ty,
            typeck_expression(sess, initialiser, Context::Default),
        )
    });
    Declaration { reference, initialiser }
}

fn typeck_compound_statement<'a>(
    sess: &'a Session<'a>,
    stmt: &scope::CompoundStatement<'a>,
    function: &scope::FunctionDefinition<'a>,
) -> CompoundStatement<'a> {
    CompoundStatement(
        sess.alloc_slice_fill_iter(
            stmt.0
                .iter()
                .map(|stmt| typeck_statement(sess, stmt, function)),
        ),
    )
}

fn typeck_statement<'a>(
    sess: &'a Session<'a>,
    stmt: &scope::Statement<'a>,
    function: &scope::FunctionDefinition<'a>,
) -> Statement<'a> {
    match stmt {
        scope::Statement::Declaration(decl) =>
            Statement::Declaration(typeck_declaration(sess, decl)),
        scope::Statement::Expression(expr) =>
            Statement::Expression(try { typeck_expression(sess, expr.as_ref()?, Context::Default) }),
        scope::Statement::Compound(stmt) =>
            Statement::Compound(typeck_compound_statement(sess, stmt, function)),
        scope::Statement::Return { return_: _, expr } => {
            let expr = expr
                .as_ref()
                .map(|expr| typeck_expression(sess, expr, Context::Default));
            let expr = match expr {
                Some(expr) => Some(convert_as_if_by_assignment(
                    sess,
                    *function.return_ty(),
                    expr,
                )),
                None => {
                    if !matches!(function.return_ty().ty, Type::Void) {
                        sess.emit(Diagnostic::ReturnWithoutValueInNonVoidFunction {
                            at: stmt.into_variant(),
                            function: *function,
                        });
                    }
                    None
                }
            };
            Statement::Return(expr)
        }
    }
}

type Comparator = impl Fn(&Arithmetic) -> (impl Ord + use<>);
const SIZE_WITH_UNSIGNED_AS_TIE_BREAKER: Comparator = |ty| (ty.conversion_rank(), ty.signedness());

fn integral_promote(ty: Arithmetic) -> Arithmetic {
    match ty {
        Arithmetic::Integral(Integral {
            signedness: _,
            kind: IntegralKind::PlainChar | IntegralKind::Char | IntegralKind::Short,
        }) => Arithmetic::Integral(Integral {
            signedness: Signedness::Signed,
            kind: IntegralKind::Int,
        }),
        ty => ty,
    }
}

fn perform_usual_arithmetic_conversions(lhs_ty: Arithmetic, rhs_ty: Arithmetic) -> Arithmetic {
    // TODO: handle floats
    // TODO: convert enumerations to their underlying types

    let lhs_ty = integral_promote(lhs_ty);
    let rhs_ty = integral_promote(rhs_ty);

    if lhs_ty == rhs_ty {
        lhs_ty
    }
    else if lhs_ty.signedness() == rhs_ty.signedness() {
        std::cmp::max_by_key(lhs_ty, rhs_ty, |ty| ty.conversion_rank())
    }
    else {
        let [smaller_ty, larger_ty] =
            std::cmp::minmax_by_key(lhs_ty, rhs_ty, SIZE_WITH_UNSIGNED_AS_TIE_BREAKER);

        if matches!(larger_ty.signedness(), Signedness::Unsigned) {
            larger_ty
        }
        else if larger_ty.size() > smaller_ty.size() {
            assert!(matches!(larger_ty.signedness(), Signedness::Signed));
            larger_ty
        }
        else {
            assert!(matches!(larger_ty.signedness(), Signedness::Signed));
            #[expect(irrefutable_let_patterns)]
            let Arithmetic::Integral(Integral { signedness: _, kind }) = larger_ty
            else {
                unreachable!()
            };

            Arithmetic::Integral(Integral { signedness: Signedness::Unsigned, kind })
        }
    }
}

fn typeck_arithmetic_binop<'a>(
    sess: &'a Session<'a>,
    kind: BinOpKind,
    lhs: TypedExpression<'a>,
    rhs: TypedExpression<'a>,
    lhs_ty: Arithmetic,
    rhs_ty: Arithmetic,
) -> TypedExpression<'a> {
    let Arithmetic::Integral(integral_ty) = perform_usual_arithmetic_conversions(lhs_ty, rhs_ty);
    let common_ty = Type::Arithmetic(Arithmetic::Integral(integral_ty)).unqualified();
    let ty = match kind {
        BinOpKind::Multiply
        | BinOpKind::Divide
        | BinOpKind::Modulo
        | BinOpKind::Add
        | BinOpKind::Subtract
        | BinOpKind::BitAnd
        | BinOpKind::BitXor
        | BinOpKind::BitOr => common_ty,
        BinOpKind::LeftShift | BinOpKind::RightShift =>
            return typeck_integral_shift(sess, kind, lhs, rhs, lhs_ty, rhs_ty),
        BinOpKind::Equal
        | BinOpKind::NotEqual
        | BinOpKind::Less
        | BinOpKind::LessEqual
        | BinOpKind::Greater
        | BinOpKind::GreaterEqual => Type::int().unqualified(),
    };
    let lhs = convert_as_if_by_assignment(sess, common_ty, lhs);
    let rhs = convert_as_if_by_assignment(sess, common_ty, rhs);
    TypedExpression {
        ty,
        expr: Expression::IntegralBinOp {
            ty: integral_ty,
            lhs: sess.alloc(lhs),
            kind,
            rhs: sess.alloc(rhs),
        },
    }
}

fn typeck_integral_shift<'a>(
    sess: &'a Session<'a>,
    kind: BinOpKind,
    lhs: TypedExpression<'a>,
    rhs: TypedExpression<'a>,
    lhs_ty: Arithmetic,
    rhs_ty: Arithmetic,
) -> TypedExpression<'a> {
    assert!(matches!(kind, BinOpKind::LeftShift | BinOpKind::RightShift));
    let lhs_ty @ Arithmetic::Integral(lhs_integral) = integral_promote(lhs_ty);
    let lhs_ty = Type::Arithmetic(lhs_ty).unqualified();
    let rhs_ty = Type::Arithmetic(integral_promote(rhs_ty)).unqualified();
    let lhs = convert_as_if_by_assignment(sess, lhs_ty, lhs);
    let rhs = convert_as_if_by_assignment(sess, rhs_ty, rhs);
    TypedExpression {
        ty: lhs_ty,
        expr: Expression::IntegralBinOp {
            ty: lhs_integral,
            lhs: sess.alloc(lhs),
            kind,
            rhs: sess.alloc(rhs),
        },
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PtrAddOrder {
    PtrFirst,
    IntegralFirst,
}

impl PtrAddOrder {
    pub fn select<T>(self, pointer: T, integral: T) -> (T, T) {
        match self {
            Self::PtrFirst => (pointer, integral),
            Self::IntegralFirst => (integral, pointer),
        }
    }
}

fn typeck_ptradd<'a>(
    sess: &'a Session<'a>,
    pointer: TypedExpression<'a>,
    pointee_ty: &QualifiedType<'a>,
    integral: TypedExpression<'a>,
    order: PtrAddOrder,
) -> TypedExpression<'a> {
    assert!(matches!(pointer.ty.ty, Type::Pointer(_)));
    assert!(matches!(
        integral.ty.ty,
        Type::Arithmetic(Arithmetic::Integral(_)),
    ));
    let integral = convert_as_if_by_assignment(sess, Type::ullong().unqualified(), integral);
    TypedExpression {
        ty: pointer.ty.ty.unqualified(),
        expr: Expression::PtrAdd {
            pointer: sess.alloc(pointer),
            integral: sess.alloc(integral),
            pointee_size: pointee_ty.ty.size(),
            order,
        },
    }
}

fn typeck_ptrsub<'a>(
    sess: &'a Session<'a>,
    pointer: TypedExpression<'a>,
    pointee_ty: &QualifiedType<'a>,
    integral: TypedExpression<'a>,
) -> TypedExpression<'a> {
    assert!(matches!(pointer.ty.ty, Type::Pointer(_)));
    assert!(matches!(
        integral.ty.ty,
        Type::Arithmetic(Arithmetic::Integral(_)),
    ));
    let integral = convert_as_if_by_assignment(sess, Type::ullong().unqualified(), integral);
    TypedExpression {
        ty: pointer.ty.ty.unqualified(),
        expr: Expression::PtrSub {
            pointer: sess.alloc(pointer),
            integral: sess.alloc(integral),
            pointee_size: pointee_ty.ty.size(),
        },
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PtrCmpKind {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

impl PtrCmpKind {
    pub(crate) fn str(&self) -> &'static str {
        match self {
            PtrCmpKind::Equal => "ptr-equal",
            PtrCmpKind::NotEqual => "ptr-not-equal",
            PtrCmpKind::Less => "ptr-less",
            PtrCmpKind::LessEqual => "ptr-less-equal",
            PtrCmpKind::Greater => "ptr-greater",
            PtrCmpKind::GreaterEqual => "ptr-greater-equal",
        }
    }
}

fn typeck_ptrcmp<'a>(
    sess: &'a Session<'a>,
    lhs: TypedExpression<'a>,
    kind: BinOpKind,
    rhs: TypedExpression<'a>,
) -> TypedExpression<'a> {
    // TODO: should check for type compatibility, not exact equality
    if lhs.ty.ty != rhs.ty.ty {
        todo!("type error: cannot compare pointers of incompatible types");
    }
    let kind = match kind {
        BinOpKind::Multiply
        | BinOpKind::Divide
        | BinOpKind::Modulo
        | BinOpKind::Add
        | BinOpKind::Subtract
        | BinOpKind::LeftShift
        | BinOpKind::RightShift
        | BinOpKind::BitAnd
        | BinOpKind::BitXor
        | BinOpKind::BitOr => unreachable!(),
        BinOpKind::Equal => PtrCmpKind::Equal,
        BinOpKind::NotEqual => PtrCmpKind::NotEqual,
        BinOpKind::Less => PtrCmpKind::Less,
        BinOpKind::LessEqual => PtrCmpKind::LessEqual,
        BinOpKind::Greater => PtrCmpKind::Greater,
        BinOpKind::GreaterEqual => PtrCmpKind::GreaterEqual,
    };
    TypedExpression {
        ty: Type::int().unqualified(),
        expr: Expression::PtrCmp {
            lhs: sess.alloc(lhs),
            kind,
            rhs: sess.alloc(rhs),
        },
    }
}

fn check_ty_can_sizeof<'a>(
    sess: &'a Session<'a>,
    ty: &QualifiedType<'a>,
    at: &scope::Expression<'a>,
    op: &Token<'a>,
) -> QualifiedType<'a> {
    if !ty.ty.is_complete() {
        sess.emit(Diagnostic::InvalidSizeofOrAlignof {
            op: *op,
            at: *at,
            kind: "incomplete type",
            ty: *ty,
        });
        Type::int().unqualified()
    }
    else if ty.ty.is_function() {
        sess.emit(Diagnostic::InvalidSizeofOrAlignof {
            op: *op,
            at: *at,
            kind: "function type",
            ty: *ty,
        });
        Type::int().unqualified()
    }
    else {
        *ty
    }
}

#[derive(Debug, Clone, Copy)]
enum Context {
    Default,
    Addressof,
    Sizeof,
}

fn typeck_expression<'a>(
    sess: &'a Session<'a>,
    expr: &scope::Expression<'a>,
    context: Context,
) -> TypedExpression<'a> {
    let expr = match expr {
        scope::Expression::Name(reference) => TypedExpression {
            ty: reference.ty,
            expr: Expression::Name(*reference),
        },
        scope::Expression::Integer(token) => {
            let TokenKind::Integer(Integer { suffix, suffix_len, base, prefix_len }) = token.kind
            else {
                unreachable!()
            };
            let number = &token.slice()[prefix_len..token.slice().len() - suffix_len];
            let number: String = number.chars().filter(|&c| c != '\'').collect();
            let value = u64::from_str_radix(&number, base).unwrap_or_else(|err| {
                todo!("emit diagnostic: integer constant too large: {err:?}")
            });
            let (signedness, kind) = match suffix {
                IntegerSuffix::None => (Signedness::Signed, IntegralKind::Int),
                IntegerSuffix::Unsigned => (Signedness::Unsigned, IntegralKind::Int),
                IntegerSuffix::UnsignedLong => (Signedness::Unsigned, IntegralKind::Long),
                IntegerSuffix::UnsignedLongLong => (Signedness::Unsigned, IntegralKind::LongLong),
                IntegerSuffix::Long => (Signedness::Signed, IntegralKind::Long),
                IntegerSuffix::LongLong => (Signedness::Signed, IntegralKind::LongLong),
                IntegerSuffix::BitInt => todo!("unimplemented: `_BitInt`"),
                IntegerSuffix::UnsignedBitInt => todo!("unimplemented: `unsigned _BitInt`"),
                IntegerSuffix::Invalid => {
                    sess.emit(Diagnostic::InvalidIntegerSuffix {
                        at: *token,
                        suffix: &token.slice()[token.slice().len() - suffix_len..],
                    });
                    (Signedness::Signed, IntegralKind::Int)
                }
            };
            let integral_ty = grow_to_fit(signedness, kind, base, value);
            TypedExpression {
                ty: Type::Arithmetic(Arithmetic::Integral(integral_ty)).unqualified(),
                expr: Expression::Integer { value, token: *token },
            }
        }
        scope::Expression::Parenthesised { open_paren, expr, close_paren } => {
            let expr = sess.alloc(typeck_expression(sess, expr, context));
            TypedExpression {
                ty: expr.ty,
                expr: Expression::Parenthesised {
                    open_paren: *open_paren,
                    expr,
                    close_paren: *close_paren,
                },
            }
        }
        scope::Expression::Assign { target, value } => {
            let target = sess.alloc(typeck_expression(sess, target, Context::Default));
            if !target.is_modifiable_lvalue() {
                if !target.is_lvalue() {
                    sess.emit(Diagnostic::AssignmentToNonLValue { at: &target.expr })
                }
                else {
                    assert!(!target.is_modifiable());
                    match target.expr.unwrap_parens() {
                        Expression::Name(reference) =>
                            sess.emit(Diagnostic::AssignmentToConstName {
                                at: &target.expr,
                                decl: reference.at_decl(),
                            }),
                        Expression::Deref { .. } =>
                            sess.emit(Diagnostic::AssignmentToConst { at: target.expr }),
                        _ => todo!("for const struct members etc."),
                    }
                }
            }
            let value = typeck_expression(sess, value, Context::Default);
            let value = sess.alloc(convert_as_if_by_assignment(sess, target.ty, value));
            TypedExpression {
                // TODO: `ty` is the type that `target` would have after lvalue conversion, so it
                // might be necessary to add a `NoopTypeConversion` here
                ty: target.ty,
                expr: Expression::Assign { target, value },
            }
        }
        scope::Expression::BinOp { lhs, kind, rhs } => {
            let lhs = typeck_expression(sess, lhs, Context::Default);
            let rhs = typeck_expression(sess, rhs, Context::Default);
            match (lhs.ty.ty, rhs.ty.ty) {
                (Type::Arithmetic(lhs_ty), Type::Arithmetic(rhs_ty)) =>
                    typeck_arithmetic_binop(sess, *kind, lhs, rhs, lhs_ty, rhs_ty),
                (Type::Arithmetic(Arithmetic::Integral(_)), Type::Pointer(pointee_ty))
                    if matches!(kind, BinOpKind::Add) =>
                    typeck_ptradd(sess, rhs, pointee_ty, lhs, PtrAddOrder::IntegralFirst),
                (Type::Pointer(pointee_ty), Type::Arithmetic(Arithmetic::Integral(_)))
                    if matches!(kind, BinOpKind::Add) =>
                    typeck_ptradd(sess, lhs, pointee_ty, rhs, PtrAddOrder::PtrFirst),
                (Type::Pointer(pointee_ty), Type::Arithmetic(Arithmetic::Integral(_)))
                    if matches!(kind, BinOpKind::Subtract) =>
                    typeck_ptrsub(sess, lhs, pointee_ty, rhs),
                (Type::Pointer(_), Type::Pointer(_))
                    if matches!(
                        kind,
                        BinOpKind::Equal
                            | BinOpKind::NotEqual
                            | BinOpKind::Less
                            | BinOpKind::LessEqual
                            | BinOpKind::Greater
                            | BinOpKind::GreaterEqual
                    ) =>
                    typeck_ptrcmp(sess, lhs, *kind, rhs),
                (Type::Pointer(_), Type::Pointer(_)) if matches!(kind, BinOpKind::Subtract) =>
                    todo!(),
                _ => todo!("type error"),
            }
        }
        scope::Expression::UnaryOp { operator, operand } => {
            let context = match operator.kind {
                UnaryOpKind::Addressof => Context::Addressof,
                UnaryOpKind::Sizeof => Context::Sizeof,
                _ => Context::Default,
            };
            let operand = typeck_expression(sess, operand, context);
            match operator.kind {
                UnaryOpKind::Addressof => {
                    let is_function_designator = operand.ty.ty.is_function();
                    // requirement “the result of a [] or unary * operator” is checked by
                    // `is_lvalue`.
                    let is_lvalue = operand.is_lvalue()
                        // TODO
                        /* && !operand.is_bitfield() && !operand.has_register_storage_class() */;
                    if !(is_function_designator || is_lvalue) {
                        todo!("type error: cannot take address of this expr");
                    }
                    TypedExpression {
                        ty: Type::Pointer(sess.alloc(operand.ty)).unqualified(),
                        expr: Expression::Addressof {
                            ampersand: operator.token,
                            operand: sess.alloc(operand),
                        },
                    }
                }
                UnaryOpKind::Deref => match operand.ty.ty {
                    Type::Pointer(pointee_ty) => {
                        if matches!(pointee_ty.ty, Type::Void) {
                            sess.emit(Diagnostic::DerefOfVoidPtr { at: operand });
                        }
                        TypedExpression {
                            ty: *pointee_ty,
                            expr: Expression::Deref {
                                star: operator.token,
                                operand: sess.alloc(operand),
                            },
                        }
                    }
                    _ => {
                        sess.emit(Diagnostic::CannotDeref { at: *expr, ty: operand.ty });
                        // TODO: This should be an error expression
                        TypedExpression {
                            ty: Type::int().unqualified(),
                            // TODO: this location is fake
                            expr: Expression::Integer { value: 0, token: operator.token },
                        }
                    }
                },
                UnaryOpKind::Plus => match operand.ty.ty {
                    Type::Arithmetic(arithmetic) => {
                        let result_ty =
                            Type::Arithmetic(integral_promote(arithmetic)).unqualified();
                        convert_as_if_by_assignment(sess, result_ty, operand)
                    }
                    _ => todo!("type error: cannot be operand to unary plus"),
                },
                UnaryOpKind::Negate => match operand.ty.ty {
                    Type::Arithmetic(arithmetic) => {
                        let result_ty =
                            Type::Arithmetic(integral_promote(arithmetic)).unqualified();
                        let operand = convert_as_if_by_assignment(sess, result_ty, operand);
                        TypedExpression {
                            ty: result_ty,
                            expr: Expression::Negate {
                                minus: operator.token,
                                operand: sess.alloc(operand),
                            },
                        }
                    }
                    _ => todo!("type error: cannot negate"),
                },
                UnaryOpKind::Compl => match operand.ty.ty {
                    Type::Arithmetic(Arithmetic::Integral(integral)) => {
                        let result_ty =
                            Type::Arithmetic(integral_promote(Arithmetic::Integral(integral)))
                                .unqualified();
                        let operand = convert_as_if_by_assignment(sess, result_ty, operand);
                        TypedExpression {
                            ty: result_ty,
                            expr: Expression::Compl {
                                compl: operator.token,
                                operand: sess.alloc(operand),
                            },
                        }
                    }
                    _ => todo!("type error: cannot be operand to unary compl"),
                },
                UnaryOpKind::Not =>
                    if operand.ty.ty.is_scalar() {
                        TypedExpression {
                            ty: Type::int().unqualified(),
                            expr: Expression::Not {
                                not: operator.token,
                                operand: sess.alloc(operand),
                            },
                        }
                    }
                    else {
                        todo!("type error: cannot be operand to unary not");
                    },
                UnaryOpKind::Sizeof => {
                    let ty = check_ty_can_sizeof(sess, &operand.ty, expr, &operator.token);
                    TypedExpression {
                        ty: Type::ulong().unqualified(),
                        expr: Expression::Sizeof {
                            sizeof: operator.token,
                            operand: sess.alloc(operand),
                            size: ty.ty.size(),
                        },
                    }
                }
            }
        }
        scope::Expression::Call { callee, args, close_paren } => {
            let callee = typeck_expression(sess, callee, Context::Default);

            let (Type::Function(ty)
            | Type::Pointer(QualifiedType {
                is_const: _,
                is_volatile: _,
                ty: Type::Function(ty),
                loc: _,
            })) = &callee.ty.ty
            else {
                todo!("type error: uncallable");
            };

            let FunctionType { params, return_type, is_varargs } = *ty;

            if is_varargs {
                if params.len() > args.len() {
                    todo!("type error: argument count mismatch");
                }
            }
            else if params.len() != args.len() {
                todo!("type error: argument count mismatch");
            }

            let default_argument_promote = |arg: TypedExpression<'a>| match arg.ty.ty {
                Type::Arithmetic(ty @ Arithmetic::Integral(_)) =>
                    Type::Arithmetic(integral_promote(ty)),
                ty => ty,
            };

            let args = args
                .iter()
                .zip_longest(params)
                .map(|arg_param| match arg_param {
                    EitherOrBoth::Both(arg, param) => {
                        let arg = typeck_expression(sess, arg, Context::Default);
                        convert_as_if_by_assignment(sess, param.ty, arg)
                    }
                    EitherOrBoth::Left(arg) => {
                        let arg = typeck_expression(sess, arg, Context::Default);
                        let ty = default_argument_promote(arg).unqualified();
                        convert_as_if_by_assignment(sess, ty, arg)
                    }
                    EitherOrBoth::Right(_param) => unreachable!(),
                });

            TypedExpression {
                ty: *return_type,
                expr: Expression::Call {
                    callee: sess.alloc(callee),
                    args: sess.alloc_slice_fill_iter(args),
                    is_varargs,
                    close_paren: *close_paren,
                },
            }
        }
        scope::Expression::Sizeof { sizeof, ty, close_paren } => {
            let ty = check_ty_can_sizeof(sess, ty, expr, sizeof);
            TypedExpression {
                ty: Type::ulong().unqualified(),
                expr: Expression::SizeofTy {
                    sizeof: *sizeof,
                    ty,
                    size: ty.ty.size(),
                    close_paren: *close_paren,
                },
            }
        }
        scope::Expression::Alignof { alignof, ty, close_paren } => {
            let ty = check_ty_can_sizeof(sess, ty, expr, alignof);
            TypedExpression {
                ty: Type::ulong().unqualified(),
                expr: Expression::Alignof {
                    alignof: *alignof,
                    ty,
                    align: ty.ty.align(),
                    close_paren: *close_paren,
                },
            }
        }
        scope::Expression::Cast { open_paren: _, ty, expr } => {
            let expr = typeck_expression(sess, expr, Context::Default);
            let ty = ty.ty.unqualified();
            convert(sess, ty, expr, ConversionKind::Explicit)
        }
        scope::Expression::Subscript { lhs, rhs, close_bracket } => typeck_expression(
            sess,
            &scope::Expression::UnaryOp {
                operator: UnaryOp {
                    kind: UnaryOpKind::Deref,
                    // TODO: Is this cheating?
                    token: *close_bracket,
                },
                operand: sess.alloc(scope::Expression::BinOp { lhs, kind: BinOpKind::Add, rhs }),
            },
            Context::Default,
        ),
        scope::Expression::Generic {
            generic,
            selector,
            assocs,
            close_paren: _,
        } => {
            let selector = typeck_expression(sess, selector, Context::Default);
            let selector_ty = selector.ty.ty.unqualified();
            let default = assocs
                .0
                .iter()
                .filter_map(|assoc| match assoc {
                    GenericAssociation::Ty { ty: _, expr: _ } => None,
                    GenericAssociation::Default { default, expr } => Some((default, expr)),
                })
                .at_most_one()
                .unwrap_or_else(|mut duplicate_defaults| {
                    let first = duplicate_defaults.next();
                    for (default, _) in duplicate_defaults {
                        sess.emit(Diagnostic::GenericWithDuplicateDefault {
                            at: *default,
                            previous_default: *first.unwrap().0,
                            generic: *generic,
                        });
                    }
                    first
                })
                .map(|(_default_token, expr)| expr);

            let assocs = assocs
                .0
                .iter()
                .filter_map(|assoc| match assoc {
                    GenericAssociation::Ty { ty, expr } => {
                        // TODO: `ty` shall not be a variably modified type.
                        if !(ty.ty.is_object() && ty.ty.is_complete()) {
                            sess.emit(Diagnostic::GenericWithInvalidType {
                                at: *ty,
                                generic: *generic,
                            });
                        }
                        Some((ty, expr))
                    }
                    GenericAssociation::Default { default: _, expr: _ } => None,
                })
                .into_grouping_map_by(|(&ty, _expr)| ty)
                .reduce(|first, _ty, duplicate| {
                    sess.emit(Diagnostic::GenericWithDuplicateType {
                        at: *duplicate.0,
                        previous: *first.0,
                        generic: *generic,
                    });
                    first
                });

            assocs
                .get(&selector_ty)
                .map(|(_ty, expr)| expr)
                .or(default.as_ref())
                .map(|expr| typeck_expression(sess, expr, context))
                .unwrap_or_else(|| {
                    sess.emit(Diagnostic::GenericWithoutMatch { at: selector, expr: *expr });
                    // TODO: this should be an error expression
                    TypedExpression {
                        ty: Type::int().unqualified(),
                        expr: Expression::Integer { value: 0, token: *generic },
                    }
                })
        }
    };

    match context {
        Context::Default => match expr.ty.ty {
            ty @ Type::Function(_) => TypedExpression {
                ty: Type::Pointer(sess.alloc(ty.unqualified())).unqualified(),
                expr: Expression::Addressof {
                    ampersand: Token::synthesised(panko_lex::TokenKind::And, expr.loc()),
                    operand: sess.alloc(expr),
                },
            },
            _ => expr,
        },
        Context::Sizeof | Context::Addressof => expr,
    }
}

fn grow_to_fit(signedness: Signedness, kind: IntegralKind, base: u32, value: u64) -> Integral {
    const POSSIBLE_TYS: [Integral; 6] = [
        Integral {
            signedness: Signedness::Signed,
            kind: IntegralKind::Int,
        },
        Integral {
            signedness: Signedness::Unsigned,
            kind: IntegralKind::Int,
        },
        Integral {
            signedness: Signedness::Signed,
            kind: IntegralKind::Long,
        },
        Integral {
            signedness: Signedness::Unsigned,
            kind: IntegralKind::Long,
        },
        Integral {
            signedness: Signedness::Signed,
            kind: IntegralKind::LongLong,
        },
        Integral {
            signedness: Signedness::Unsigned,
            kind: IntegralKind::LongLong,
        },
    ];
    POSSIBLE_TYS
        .into_iter()
        .filter(|ty| base != 10 || ty.signedness == signedness)
        .filter(|ty| ty.kind >= kind)
        .find(|ty| ty.can_represent(value))
        .unwrap_or_else(|| todo!("emit error: integer constant cannot be represented"))
}

pub fn resolve_types<'a>(
    sess: &'a Session<'a>,
    translation_unit: scope::TranslationUnit<'a>,
) -> TranslationUnit<'a> {
    TranslationUnit {
        decls: sess.alloc_slice_fill_iter(translation_unit.decls.iter().map(|decl| match decl {
            scope::ExternalDeclaration::FunctionDefinition(def) =>
                ExternalDeclaration::FunctionDefinition(typeck_function_definition(sess, def)),
            scope::ExternalDeclaration::Declaration(decl) =>
                ExternalDeclaration::Declaration(typeck_declaration(sess, decl)),
        })),
    }
}
