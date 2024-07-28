use std::collections::HashMap;

use bumpalo::Bump;
use panko_lex::Token;
use panko_parser as cst;
use panko_parser::ast;
use panko_parser::ast::QualifiedType;
use panko_parser::ast::Type;

use crate::nonempty;

mod as_sexpr;

#[derive(Debug, Clone, Copy)]
pub struct TranslationUnit<'a> {
    decls: &'a [ExternalDeclaration<'a>],
}

#[derive(Debug, Clone, Copy)]
enum ExternalDeclaration<'a> {
    FunctionDefinition(FunctionDefinition<'a>),
    Declaration(Declaration<'a>),
}

#[derive(Debug, Clone, Copy)]
struct Declaration<'a> {
    reference: Reference<'a>,
    initialiser: Option<Expression<'a>>,
}

#[derive(Debug, Clone, Copy)]
struct FunctionDefinition<'a> {
    reference: Reference<'a>,
    #[expect(unused)]
    storage_class: Option<cst::StorageClassSpecifier<'a>>,
    #[expect(unused)]
    inline: Option<cst::FunctionSpecifier<'a>>,
    #[expect(unused)]
    noreturn: Option<cst::FunctionSpecifier<'a>>,
    body: CompoundStatement<'a>,
}

#[derive(Debug, Clone, Copy)]
struct CompoundStatement<'a>(&'a [Statement<'a>]);

#[derive(Debug, Clone, Copy)]
enum Statement<'a> {
    Declaration(Declaration<'a>),
    Expression(Option<Expression<'a>>),
    Compound(CompoundStatement<'a>),
    Return(Option<Expression<'a>>),
}

#[derive(Debug, Clone, Copy)]
enum Expression<'a> {
    Name(Reference<'a>),
    Integer(Token<'a>),
}

#[derive(Debug, Clone, Copy)]
struct Reference<'a> {
    name: Token<'a>,
    ty: QualifiedType<'a>,
}

impl<'a> Reference<'a> {
    fn ident(&self) -> &'a str {
        self.name.slice()
    }
}

#[derive(Debug, Default)]
struct Scope<'a> {
    names: nonempty::Vec<HashMap<&'a str, Reference<'a>>>,
}

impl<'a> Scope<'a> {
    fn lookup(&self, name: &'a str) -> Option<Reference<'a>> {
        self.names
            .iter()
            .rev()
            .find_map(|names| names.get(name))
            .copied()
    }

    fn lookup_innermost(&self, name: &'a str) -> Option<Reference<'a>> {
        self.names.last().get(name).copied()
    }

    fn push(&mut self) {
        self.names.push(HashMap::default());
    }

    fn pop(&mut self) {
        self.names.pop();
    }
}

#[derive(Debug)]
struct Scopes<'a> {
    bump: &'a Bump,
    /// at most two elements: the global scope and a function scope
    scopes: nonempty::Vec<Scope<'a>>,
}

impl<'a> Scopes<'a> {
    fn add(&mut self, reference: Reference<'a>) {
        match self.lookup_innermost(reference.ident()) {
            Some(_) => todo!("already declared: {reference:#?}"),
            None => {
                let maybe_old_value = self
                    .scopes
                    .last_mut()
                    .names
                    .last_mut()
                    .insert(reference.ident(), reference);
                assert!(maybe_old_value.is_none());
            }
        }
    }

    fn lookup(&self, name: &'a str) -> Option<Reference<'a>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.lookup(name))
    }

    fn lookup_innermost(&self, name: &'a str) -> Option<Reference<'a>> {
        self.scopes.last().lookup_innermost(name)
    }

    fn push(&mut self) {
        self.scopes.push(Scope::default());
        assert!(self.scopes.len() <= 2);
    }

    fn pop(&mut self) {
        self.scopes.pop().unwrap();
    }
}

fn resolve_function_definition<'a>(
    scopes: &mut Scopes<'a>,
    def: &ast::FunctionDefinition<'a>,
) -> FunctionDefinition<'a> {
    let reference = Reference { name: def.name, ty: def.ty };
    scopes.add(reference);
    scopes.push();
    let QualifiedType {
        is_const: false,
        is_volatile: false,
        ty: Type::Function(function_ty),
    } = def.ty
    else {
        unreachable!()
    };
    // FIXME: the params need to be resolved in the same scope as the body so that redeclarations
    // of parameters properly produce an error.
    for param in function_ty.params {
        if let Some(name) = param.name {
            scopes.add(Reference { ty: param.ty, name });
        }
    }
    let body = resolve_compound_statement(scopes, &def.body);
    scopes.pop();
    FunctionDefinition {
        reference,
        storage_class: def.storage_class,
        inline: def.inline,
        noreturn: def.noreturn,
        body,
    }
}

fn resolve_compound_statement<'a>(
    scopes: &mut Scopes<'a>,
    stmts: &ast::CompoundStatement<'a>,
) -> CompoundStatement<'a> {
    scopes.scopes.last_mut().push();
    let stmts = CompoundStatement(
        scopes
            .bump
            .alloc_slice_fill_iter(stmts.0.iter().map(|stmt| resolve_stmt(scopes, stmt))),
    );
    scopes.scopes.last_mut().pop();
    stmts
}

fn resolve_declaration<'a>(
    scopes: &mut Scopes<'a>,
    decl: &ast::Declaration<'a>,
) -> Declaration<'a> {
    let reference = Reference { name: decl.name, ty: decl.ty };
    scopes.add(reference);
    Declaration {
        reference,
        initialiser: decl
            .initialiser
            .as_ref()
            .map(|initialiser| resolve_expr(scopes, initialiser)),
    }
}

fn resolve_stmt<'a>(scopes: &mut Scopes<'a>, stmt: &ast::Statement<'a>) -> Statement<'a> {
    match stmt {
        ast::Statement::Declaration(decl) =>
            Statement::Declaration(resolve_declaration(scopes, decl)),
        ast::Statement::Expression(expr) =>
            Statement::Expression(expr.as_ref().map(|expr| resolve_expr(scopes, expr))),
        ast::Statement::Compound(stmts) =>
            Statement::Compound(resolve_compound_statement(scopes, stmts)),
        ast::Statement::Return(expr) =>
            Statement::Return(expr.as_ref().map(|expr| resolve_expr(scopes, expr))),
    }
}

fn resolve_expr<'a>(scopes: &mut Scopes<'a>, expr: &ast::Expression<'a>) -> Expression<'a> {
    match expr {
        ast::Expression::Name(name) => Expression::Name(
            scopes
                .lookup(name.slice())
                .unwrap_or_else(|| todo!("name error: {name:#?}")),
        ),
        ast::Expression::Integer(integer) => Expression::Integer(*integer),
    }
}

pub fn resolve_names<'a>(
    bump: &'a Bump,
    translation_unit: ast::TranslationUnit<'a>,
) -> TranslationUnit<'a> {
    let scopes = &mut Scopes { bump, scopes: nonempty::Vec::default() };
    TranslationUnit {
        decls: bump.alloc_slice_fill_iter(translation_unit.decls.iter().map(|decl| match decl {
            ast::ExternalDeclaration::FunctionDefinition(def) =>
                ExternalDeclaration::FunctionDefinition(resolve_function_definition(scopes, def)),
            ast::ExternalDeclaration::Declaration(decl) =>
                ExternalDeclaration::Declaration(resolve_declaration(scopes, decl)),
        })),
    }
}