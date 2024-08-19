use panko_parser as cst;
use panko_parser::ast::Integral;
use panko_parser::ast::IntegralKind;
use panko_parser::ast::QualifiedType;
use panko_parser::ast::Session;

use crate::scope;
use crate::scope::ParamRefs;
use crate::scope::Reference;

mod as_sexpr;

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
    initialiser: Option<Expression<'a>>,
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
    Expression(Option<Expression<'a>>),
    Compound(CompoundStatement<'a>),
    Return(Option<Expression<'a>>),
}

#[derive(Debug, Clone, Copy)]
pub struct Expression<'a> {
    ty: QualifiedType<'a>,
    expr: scope::Expression<'a>,
}

fn typeck_function_definition<'a>(
    sess: &'a Session<'a>,
    definition: &scope::FunctionDefinition<'a>,
) -> FunctionDefinition<'a> {
    let return_ty = match definition.reference.ty.ty {
        panko_parser::ast::Type::Function(function_ty) => function_ty.return_type,
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

fn typeck_declaration<'a>(declaration: &scope::Declaration<'a>) -> Declaration<'a> {
    let scope::Declaration { reference, initialiser } = *declaration;
    let initialiser = initialiser
        .as_ref()
        .map(|initialiser| typeck_expression(initialiser));
    if let Some(initialiser) = initialiser {
        assert_eq!(reference.ty, initialiser.ty, "TODO: real typeck here",);
    }
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
        scope::Statement::Declaration(decl) => Statement::Declaration(typeck_declaration(decl)),
        scope::Statement::Expression(expr) =>
            Statement::Expression(try { typeck_expression(expr.as_ref()?) }),
        scope::Statement::Compound(stmt) =>
            Statement::Compound(typeck_compound_statement(sess, stmt, return_ty)),
        scope::Statement::Return(expr) => {
            let expr = expr.as_ref().map(|expr| typeck_expression(expr));
            match expr {
                Some(expr) => assert_eq!(&expr.ty, return_ty, "TODO: real typeck here"),
                None => todo!("assert we are in a `void` function"),
            }
            Statement::Return(expr)
        }
    }
}

fn typeck_expression<'a>(expr: &scope::Expression<'a>) -> Expression<'a> {
    match expr {
        scope::Expression::Name(reference) => Expression { ty: reference.ty, expr: *expr },
        scope::Expression::Integer(_token) => Expression {
            // TODO: resolve to correct type depending on the actual value in `_token`
            ty: QualifiedType {
                is_const: false,
                is_volatile: false,
                ty: panko_parser::ast::Type::Integral(Integral {
                    signedness: None,
                    kind: IntegralKind::Int,
                }),
            },
            expr: *expr,
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
                ExternalDeclaration::Declaration(typeck_declaration(decl)),
        })),
    }
}
