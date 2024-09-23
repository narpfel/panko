use std::cmp::Ordering;

use ariadne::Color::Blue;
use ariadne::Color::Red;
use panko_lex::Loc;
use panko_lex::Token;
use panko_parser as cst;
use panko_parser::ast::Arithmetic;
use panko_parser::ast::Integral;
use panko_parser::ast::IntegralKind;
use panko_parser::ast::QualifiedType;
use panko_parser::ast::Session;
use panko_parser::ast::Signedness;
use panko_parser::ast::Type;
use panko_parser::sexpr_builder::AsSExpr as _;
use panko_parser::BinOpKind;
use panko_report::Report;
use variant_types::IntoVariant as _;

use crate::scope;
use crate::scope::ParamRefs;
use crate::scope::Reference;

mod as_sexpr;
#[cfg(test)]
mod tests;

#[derive(Debug, Report)]
#[exit_code(1)]
enum Diagnostic<'a> {
    // TODO: colourise types
    // TODO: types should be printed in C syntax, not in their SExpr debug repr
    #[error("invalid implicit conversion from `{from_ty}` to `{to_ty}`")]
    #[diagnostics(at(colour = Red, label = "this is of type `{from_ty}`, which cannot be implicitly converted to `{to_ty}`"))]
    InvalidImplicitConversion {
        at: TypedExpression<'a>,
        from_ty: Type<'a>,
        to_ty: Type<'a>,
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
    AssignmentToConst {
        at: &'a Expression<'a>,
        decl: Reference<'a>,
    },

    #[error("cannot assign to this expression because it is not an lvalue")]
    #[diagnostics(at(colour = Red, label = "this expression is not an lvalue"))]
    AssignmentToNonLValue { at: &'a Expression<'a> },
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
    Integer(Token<'a>),
    NoopTypeConversion(&'a TypedExpression<'a>),
    Truncate(&'a TypedExpression<'a>),
    SignExtend(&'a TypedExpression<'a>),
    ZeroExtend(&'a TypedExpression<'a>),
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
    PtrEq {
        lhs: &'a TypedExpression<'a>,
        kind: PtrCmpKind,
        rhs: &'a TypedExpression<'a>,
    },
}

impl TypedExpression<'_> {
    fn loc(&self) -> Loc {
        self.expr.loc()
    }

    fn is_modifiable_lvalue(&self) -> bool {
        self.is_lvalue() && self.is_modifiable()
    }

    fn is_lvalue(&self) -> bool {
        match self.expr {
            Expression::Name(_) => true,
            Expression::Parenthesised { open_paren: _, expr, close_paren: _ } => expr.is_lvalue(),
            Expression::Integer(_)
            | Expression::NoopTypeConversion(_)
            | Expression::Truncate(_)
            | Expression::SignExtend(_)
            | Expression::ZeroExtend(_)
            | Expression::Assign { .. }
            | Expression::IntegralBinOp { .. }
            | Expression::PtrAdd { .. }
            | Expression::PtrSub { .. }
            | Expression::PtrEq { .. } => false,
        }
    }

    fn is_modifiable(&self) -> bool {
        !self.ty.is_const
    }
}

impl<'a> Expression<'a> {
    fn loc(&self) -> Loc {
        match self {
            Expression::Name(reference) => reference.loc(),
            Expression::Integer(token) => token.loc(),
            Expression::NoopTypeConversion(inner)
            | Expression::Truncate(inner)
            | Expression::SignExtend(inner)
            | Expression::ZeroExtend(inner) => inner.loc(),
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
            Expression::PtrEq { lhs, kind: _, rhs } => lhs.loc().until(rhs.loc()),
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

fn convert_as_if_by_assignment<'a>(
    sess: &'a Session<'a>,
    target: QualifiedType<'a>,
    expr: TypedExpression<'a>,
) -> TypedExpression<'a> {
    let target_ty = target.ty;
    let expr_ty = expr.ty.ty;
    let conversion = match (target_ty, expr_ty) {
        (Type::Arithmetic(_), Type::Arithmetic(_)) | (Type::Pointer(_), Type::Pointer(_))
            if expr_ty == target_ty =>
            return expr,
        (Type::Arithmetic(_), Type::Arithmetic(source_arithmetic)) => {
            let expr_kind = match target_ty.size().cmp(&expr_ty.size()) {
                Ordering::Less => Expression::Truncate,
                Ordering::Equal => Expression::NoopTypeConversion,
                Ordering::Greater => match source_arithmetic.signedness() {
                    Signedness::Signed => Expression::SignExtend,
                    Signedness::Unsigned => Expression::ZeroExtend,
                },
            };
            expr_kind(sess.alloc(expr))
        }
        (Type::Pointer(_), Type::Pointer(_)) => Expression::NoopTypeConversion(sess.alloc(expr)),
        // TODO: clang (but not gcc) allows implicitly converting `Type::Function(_)` to
        // `Type::Pointer(_)` (with a warning).
        // TODO: handle nullptr literals
        (Type::Function(_), _) => todo!("[{}] = {}", target.as_sexpr(), expr.as_sexpr()),
        _ => {
            sess.emit(Diagnostic::InvalidImplicitConversion {
                at: expr,
                from_ty: expr_ty,
                to_ty: target_ty,
            });
            expr.expr
        }
    };
    TypedExpression {
        ty: target_ty.unqualified(),
        expr: conversion,
    }
}

fn typeck_function_definition<'a>(
    sess: &'a Session<'a>,
    definition: &scope::FunctionDefinition<'a>,
) -> FunctionDefinition<'a> {
    FunctionDefinition {
        reference: definition.reference,
        params: definition.params,
        storage_class: definition.storage_class,
        inline: definition.inline,
        noreturn: definition.noreturn,
        body: typeck_compound_statement(sess, &definition.body, definition),
    }
}

fn typeck_declaration<'a>(
    sess: &'a Session<'a>,
    declaration: &scope::Declaration<'a>,
) -> Declaration<'a> {
    let scope::Declaration { reference, initialiser } = *declaration;
    let initialiser = initialiser.as_ref().map(|initialiser| {
        convert_as_if_by_assignment(sess, reference.ty, typeck_expression(sess, initialiser))
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
            Statement::Expression(try { typeck_expression(sess, expr.as_ref()?) }),
        scope::Statement::Compound(stmt) =>
            Statement::Compound(typeck_compound_statement(sess, stmt, function)),
        scope::Statement::Return { return_: _, expr } => {
            let expr = expr.as_ref().map(|expr| typeck_expression(sess, expr));
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

fn perform_usual_arithmetic_conversions(lhs_ty: Arithmetic, rhs_ty: Arithmetic) -> Arithmetic {
    // TODO: handle floats
    // TODO: convert enumerations to their underlying types

    let promote = |ty| match ty {
        Arithmetic::Integral(Integral {
            signedness: _,
            kind: IntegralKind::PlainChar | IntegralKind::Char | IntegralKind::Short,
        }) => Arithmetic::Integral(Integral {
            signedness: Signedness::Signed,
            kind: IntegralKind::Int,
        }),
        ty => ty,
    };

    let lhs_ty = promote(lhs_ty);
    let rhs_ty = promote(rhs_ty);

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
        BinOpKind::Add | BinOpKind::Subtract => common_ty,
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
        BinOpKind::Add | BinOpKind::Subtract => unreachable!(),
        BinOpKind::Equal => PtrCmpKind::Equal,
        BinOpKind::NotEqual => PtrCmpKind::NotEqual,
        BinOpKind::Less => PtrCmpKind::Less,
        BinOpKind::LessEqual => PtrCmpKind::LessEqual,
        BinOpKind::Greater => PtrCmpKind::Greater,
        BinOpKind::GreaterEqual => PtrCmpKind::GreaterEqual,
    };
    TypedExpression {
        ty: Type::int().unqualified(),
        expr: Expression::PtrEq {
            lhs: sess.alloc(lhs),
            kind,
            rhs: sess.alloc(rhs),
        },
    }
}

fn typeck_expression<'a>(
    sess: &'a Session<'a>,
    expr: &scope::Expression<'a>,
) -> TypedExpression<'a> {
    match expr {
        scope::Expression::Name(reference) => TypedExpression {
            ty: reference.ty,
            expr: Expression::Name(*reference),
        },
        scope::Expression::Integer(token) => TypedExpression {
            // TODO: resolve to correct type depending on the actual value in `_token`
            ty: Type::int().unqualified(),
            expr: Expression::Integer(*token),
        },
        scope::Expression::Parenthesised { open_paren, expr, close_paren } => {
            let expr = sess.alloc(typeck_expression(sess, expr));
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
            let target = sess.alloc(typeck_expression(sess, target));
            if !target.is_modifiable_lvalue() {
                if !target.is_lvalue() {
                    sess.emit(Diagnostic::AssignmentToNonLValue { at: &target.expr })
                }
                else {
                    assert!(!target.is_modifiable());
                    match target.expr.unwrap_parens() {
                        Expression::Name(reference) => sess.emit(Diagnostic::AssignmentToConst {
                            at: &target.expr,
                            decl: reference.at_decl(),
                        }),
                        _ => todo!("for pointer derefs and const struct members etc."),
                    }
                }
            }
            let value = typeck_expression(sess, value);
            let value = sess.alloc(convert_as_if_by_assignment(sess, target.ty, value));
            TypedExpression {
                // TODO: `ty` is the type that `target` would have after lvalue conversion, so it
                // might be necessary to add a `NoopTypeConversion` here
                ty: target.ty,
                expr: Expression::Assign { target, value },
            }
        }
        scope::Expression::BinOp { lhs, kind, rhs } => {
            let lhs = typeck_expression(sess, lhs);
            let rhs = typeck_expression(sess, rhs);
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
    }
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
