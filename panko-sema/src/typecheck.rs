use ariadne::Color::Red;
use panko_lex::Loc;
use panko_lex::Token;
use panko_parser as cst;
use panko_parser::ast::Arithmetic;
use panko_parser::ast::Integral;
use panko_parser::ast::IntegralKind;
use panko_parser::ast::QualifiedType;
use panko_parser::ast::Session;
use panko_parser::ast::Type;
use panko_parser::sexpr_builder::AsSExpr as _;
use panko_report::Report;

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
    reference: Reference<'a>,
    initialiser: Option<TypedExpression<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionDefinition<'a> {
    reference: Reference<'a>,
    params: ParamRefs<'a>,
    #[expect(unused)]
    storage_class: Option<cst::StorageClassSpecifier<'a>>,
    #[expect(unused)]
    inline: Option<cst::FunctionSpecifier<'a>>,
    #[expect(unused)]
    noreturn: Option<cst::FunctionSpecifier<'a>>,
    body: CompoundStatement<'a>,
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
    ty: QualifiedType<'a>,
    expr: Expression<'a>,
}

#[derive(Debug, Clone, Copy)]
enum Expression<'a> {
    Name(Reference<'a>),
    Integer(Token<'a>),
    ImplicitConversion(ImplicitConversion<'a>),
}

#[derive(Debug, Clone, Copy)]
struct ImplicitConversion<'a> {
    #[expect(unused)]
    ty: Type<'a>,
    from: &'a TypedExpression<'a>,
}

impl TypedExpression<'_> {
    fn loc(&self) -> Loc {
        match self.expr {
            Expression::Name(name) => name.loc(),
            Expression::Integer(token) => token.loc(),
            Expression::ImplicitConversion(implicit_conversion) => implicit_conversion.from.loc(),
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
    match (target_ty, expr_ty) {
        (Type::Arithmetic(_), Type::Arithmetic(_)) | (Type::Pointer(_), Type::Pointer(_)) =>
            if expr_ty != target_ty {
                TypedExpression {
                    ty: target_ty.unqualified(),
                    expr: Expression::ImplicitConversion(ImplicitConversion {
                        ty: target_ty,
                        from: sess.alloc(expr),
                    }),
                }
            }
            else {
                expr
            },
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
            TypedExpression {
                ty: target_ty.unqualified(),
                expr: Expression::ImplicitConversion(ImplicitConversion {
                    ty: target_ty,
                    from: sess.alloc(expr),
                }),
            }
        }
    }
}

fn typeck_function_definition<'a>(
    sess: &'a Session<'a>,
    definition: &scope::FunctionDefinition<'a>,
) -> FunctionDefinition<'a> {
    let return_ty = match definition.reference.ty.ty {
        Type::Function(function_ty) => function_ty.return_type,
        _ => unreachable!(),
    };
    FunctionDefinition {
        reference: definition.reference,
        params: definition.params,
        storage_class: definition.storage_class,
        inline: definition.inline,
        noreturn: definition.noreturn,
        body: typeck_compound_statement(sess, &definition.body, return_ty),
    }
}

fn typeck_declaration<'a>(
    sess: &'a Session<'a>,
    declaration: &scope::Declaration<'a>,
) -> Declaration<'a> {
    let scope::Declaration { reference, initialiser } = *declaration;
    let initialiser = initialiser.as_ref().map(|initialiser| {
        convert_as_if_by_assignment(sess, reference.ty, typeck_expression(initialiser))
    });
    Declaration { reference, initialiser }
}

fn typeck_compound_statement<'a>(
    sess: &'a Session<'a>,
    stmt: &scope::CompoundStatement<'a>,
    return_ty: &QualifiedType<'a>,
) -> CompoundStatement<'a> {
    CompoundStatement(
        sess.alloc_slice_fill_iter(
            stmt.0
                .iter()
                .map(|stmt| typeck_statement(sess, stmt, return_ty)),
        ),
    )
}

fn typeck_statement<'a>(
    sess: &'a Session<'a>,
    stmt: &scope::Statement<'a>,
    return_ty: &QualifiedType<'a>,
) -> Statement<'a> {
    match stmt {
        scope::Statement::Declaration(decl) =>
            Statement::Declaration(typeck_declaration(sess, decl)),
        scope::Statement::Expression(expr) =>
            Statement::Expression(try { typeck_expression(expr.as_ref()?) }),
        scope::Statement::Compound(stmt) =>
            Statement::Compound(typeck_compound_statement(sess, stmt, return_ty)),
        scope::Statement::Return(expr) => {
            let expr = expr.as_ref().map(|expr| typeck_expression(expr));
            let expr = match expr {
                Some(expr) => Some(convert_as_if_by_assignment(sess, *return_ty, expr)),
                None => todo!("assert we are in a `void` function"),
            };
            Statement::Return(expr)
        }
    }
}

fn typeck_expression<'a>(expr: &scope::Expression<'a>) -> TypedExpression<'a> {
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
