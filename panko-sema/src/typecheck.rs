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
use panko_report::Report;
use variant_types::IntoVariant as _;

use crate::scope;
use crate::scope::ParamRefs;
use crate::scope::Reference;

mod as_sexpr;

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
    Assign {
        target: &'a TypedExpression<'a>,
        value: &'a TypedExpression<'a>,
    },
}

impl TypedExpression<'_> {
    fn loc(&self) -> Loc {
        match self.expr {
            Expression::Name(name) => name.loc(),
            Expression::Integer(token) => token.loc(),
            Expression::NoopTypeConversion(expr) => expr.loc(),
            Expression::Truncate(truncate) => truncate.loc(),
            Expression::SignExtend(sign_extend) => sign_extend.loc(),
            Expression::ZeroExtend(zero_extend) => zero_extend.loc(),
            Expression::Assign { target, value } => target.loc().until(value.loc()),
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
        (Type::Arithmetic(target_arithmetic), Type::Arithmetic(_)) => {
            let expr_kind = match target_ty.size().cmp(&expr_ty.size()) {
                Ordering::Less => Expression::Truncate,
                Ordering::Equal => Expression::NoopTypeConversion,
                Ordering::Greater => {
                    let signedness = match target_arithmetic {
                        Arithmetic::Char => Signedness::Signed,
                        Arithmetic::Integral(integral) =>
                            integral.signedness.unwrap_or(Signedness::Signed),
                    };
                    match signedness {
                        Signedness::Signed => Expression::SignExtend,
                        Signedness::Unsigned => Expression::ZeroExtend,
                    }
                }
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
            ty: Type::Arithmetic(Arithmetic::Integral(Integral {
                signedness: None,
                kind: IntegralKind::Int,
            }))
            .unqualified(),
            expr: Expression::Integer(*token),
        },
        scope::Expression::Assign { target, value } => {
            // TODO: check that `target` is a modifiable lvalue
            let target = sess.alloc(typeck_expression(sess, target));
            let value = typeck_expression(sess, value);
            let value = sess.alloc(convert_as_if_by_assignment(sess, target.ty, value));
            TypedExpression {
                // TODO: `ty` is the type that `target` would have after lvalue conversion, so it
                // might be necessary to add a `NoopTypeConversion` here
                ty: target.ty,
                expr: Expression::Assign { target, value },
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
