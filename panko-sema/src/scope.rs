use std::collections::HashMap;

use ariadne::Color::Blue;
use ariadne::Color::Red;
use itertools::Itertools as _;
use panko_lex::Loc;
use panko_lex::Token;
use panko_parser as cst;
use panko_parser::ast;
use panko_parser::ast::FunctionType;
use panko_parser::ast::QualifiedType;
use panko_parser::ast::Session;
use panko_parser::ast::Type;
use panko_report::Report;

use crate::nonempty;

mod as_sexpr;

#[derive(Debug, Report)]
#[exit_code(1)]
enum Diagnostic<'a> {
    #[error("duplicate declaration for `{at}`")]
    #[diagnostics(
        previous_definition(colour = Blue, label = "previously defined here"),
        at(colour = Red, label = "duplicate definition"),
    )]
    AlreadyDefined {
        at: Reference<'a>,
        previous_definition: Reference<'a>,
    },

    #[error("expected `;` after declaration (or did you mean to declare a function: `{at}()`?)")]
    #[diagnostics(at(colour = Red, label = "in this declaration"))]
    FunctionDeclaratorDoesNotHaveFunctionType { at: Token<'a> },
}

#[derive(Debug)]
enum OpenNewScope {
    Yes,
    No,
}

#[derive(Debug, Clone, Copy)]
struct Id(u64);

#[derive(Debug, Clone, Copy)]
pub struct TranslationUnit<'a> {
    pub(crate) decls: &'a [ExternalDeclaration<'a>],
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ExternalDeclaration<'a> {
    FunctionDefinition(FunctionDefinition<'a>),
    Declaration(Declaration<'a>),
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Declaration<'a> {
    pub(crate) reference: Reference<'a>,
    pub(crate) initialiser: Option<Expression<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct FunctionDefinition<'a> {
    pub(crate) reference: Reference<'a>,
    pub(crate) params: ParamRefs<'a>,
    pub(crate) storage_class: Option<cst::StorageClassSpecifier<'a>>,
    pub(crate) inline: Option<cst::FunctionSpecifier<'a>>,
    pub(crate) noreturn: Option<cst::FunctionSpecifier<'a>>,
    pub(crate) body: CompoundStatement<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct ParamRefs<'a>(pub(crate) &'a [Reference<'a>]);

#[derive(Debug, Clone, Copy)]
pub(crate) struct CompoundStatement<'a>(pub(crate) &'a [Statement<'a>]);

#[derive(Debug, Clone, Copy)]
pub(crate) enum Statement<'a> {
    Declaration(Declaration<'a>),
    Expression(Option<Expression<'a>>),
    Compound(CompoundStatement<'a>),
    Return(Option<Expression<'a>>),
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Expression<'a> {
    Name(Reference<'a>),
    Integer(Token<'a>),
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Reference<'a> {
    name: Token<'a>,
    pub(crate) ty: QualifiedType<'a>,
    id: Id,
}

impl<'a> Reference<'a> {
    pub fn unique_name(&self) -> String {
        format!("{}~{}", self.ident(), self.id.0)
    }

    fn ident(&self) -> &'a str {
        self.name.slice()
    }

    fn loc(&self) -> Loc<'a> {
        self.name.loc()
    }

    fn slice(&self) -> &'a str {
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
    sess: &'a Session<'a>,
    /// at most two elements: the global scope and a function scope
    scopes: nonempty::Vec<Scope<'a>>,
    next_id: u64,
}

impl<'a> Scopes<'a> {
    fn add(&mut self, reference: Reference<'a>) {
        match self.lookup_innermost(reference.ident()) {
            Some(previous_definition) => self
                .sess
                .emit(Diagnostic::AlreadyDefined { at: reference, previous_definition }),
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

    fn id(&mut self) -> Id {
        let id = Id(self.next_id);
        self.next_id += 1;
        id
    }
}

fn resolve_function_definition<'a>(
    scopes: &mut Scopes<'a>,
    def: &'a ast::FunctionDefinition<'a>,
) -> FunctionDefinition<'a> {
    let reference = Reference {
        name: def.name,
        ty: def.ty,
        id: scopes.id(),
    };
    scopes.add(reference);
    scopes.push();

    let function_ty = match &def.ty {
        QualifiedType {
            is_const: false,
            is_volatile: false,
            ty: Type::Function(function_ty),
        } => *function_ty,
        QualifiedType { ty: Type::Function(_), .. } =>
            unreachable!("function types cannot be qualified"),
        non_function_ty => {
            scopes
                .sess
                .emit(Diagnostic::FunctionDeclaratorDoesNotHaveFunctionType { at: def.name });
            FunctionType {
                params: &[],
                return_type: non_function_ty,
            }
        }
    };

    let params = scopes.sess.alloc_slice_copy(
        &function_ty
            .params
            .iter()
            .filter_map(|param| {
                let name = param.name?;
                let reference = Reference { ty: param.ty, name, id: scopes.id() };
                scopes.add(reference);
                Some(reference)
            })
            .collect_vec(),
    );

    let body = resolve_compound_statement(scopes, &def.body, OpenNewScope::No);
    scopes.pop();
    FunctionDefinition {
        reference,
        params: ParamRefs(params),
        storage_class: def.storage_class,
        inline: def.inline,
        noreturn: def.noreturn,
        body,
    }
}

fn resolve_compound_statement<'a>(
    scopes: &mut Scopes<'a>,
    stmts: &ast::CompoundStatement<'a>,
    open_new_scope: OpenNewScope,
) -> CompoundStatement<'a> {
    if let OpenNewScope::Yes = open_new_scope {
        scopes.scopes.last_mut().push();
    }
    let stmts = CompoundStatement(
        scopes
            .sess
            .alloc_slice_fill_iter(stmts.0.iter().map(|stmt| resolve_stmt(scopes, stmt))),
    );
    if let OpenNewScope::Yes = open_new_scope {
        scopes.scopes.last_mut().pop();
    }
    stmts
}

fn resolve_declaration<'a>(
    scopes: &mut Scopes<'a>,
    decl: &ast::Declaration<'a>,
) -> Declaration<'a> {
    let reference = Reference {
        name: decl.name,
        ty: decl.ty,
        id: scopes.id(),
    };
    scopes.add(reference);
    Declaration {
        reference,
        initialiser: try { resolve_expr(scopes, &decl.initialiser?) },
    }
}

fn resolve_stmt<'a>(scopes: &mut Scopes<'a>, stmt: &ast::Statement<'a>) -> Statement<'a> {
    match stmt {
        ast::Statement::Declaration(decl) =>
            Statement::Declaration(resolve_declaration(scopes, decl)),
        ast::Statement::Expression(expr) =>
            Statement::Expression(try { resolve_expr(scopes, expr.as_ref()?) }),
        ast::Statement::Compound(stmts) =>
            Statement::Compound(resolve_compound_statement(scopes, stmts, OpenNewScope::Yes)),
        ast::Statement::Return(expr) =>
            Statement::Return(try { resolve_expr(scopes, expr.as_ref()?) }),
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
    sess: &'a Session<'a>,
    translation_unit: ast::TranslationUnit<'a>,
) -> TranslationUnit<'a> {
    let scopes = &mut Scopes {
        sess,
        scopes: nonempty::Vec::default(),
        next_id: 0,
    };
    TranslationUnit {
        decls: sess.alloc_slice_fill_iter(translation_unit.decls.iter().map(|decl| match decl {
            ast::ExternalDeclaration::FunctionDefinition(def) =>
                ExternalDeclaration::FunctionDefinition(resolve_function_definition(scopes, def)),
            ast::ExternalDeclaration::Declaration(decl) =>
                ExternalDeclaration::Declaration(resolve_declaration(scopes, decl)),
        })),
    }
}
