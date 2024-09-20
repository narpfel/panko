use std::collections::hash_map::Entry;
use std::collections::HashMap;

use ariadne::Color::Blue;
use ariadne::Color::Red;
use itertools::Itertools as _;
use panko_lex::Loc;
use panko_lex::Token;
use panko_lex::TokenKind;
use panko_parser as cst;
use panko_parser::ast;
use panko_parser::ast::FunctionType;
use panko_parser::ast::QualifiedType;
use panko_parser::ast::Session;
use panko_parser::ast::Type;
use panko_parser::BinOpKind;
use panko_report::Report;

use crate::nonempty;

mod as_sexpr;

#[derive(Debug, Report)]
#[exit_code(1)]
enum Diagnostic<'a> {
    #[error("duplicate definition for `{at}`")]
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

    #[error("redeclaration of `{at}` with different type: `{original_ty}` vs. `{new_ty}`")]
    #[with(original_ty = previous_definition.ty, new_ty = at.ty)]
    #[diagnostics(
        previous_definition(colour = Blue, label = "previously declared here with type `{original_ty}`"),
        at(colour = Red, label = "new declaration with different type `{new_ty}`"),
    )]
    AlreadyDefinedWithDifferentType {
        at: Reference<'a>,
        previous_definition: Reference<'a>,
    },
}

#[derive(Debug)]
enum OpenNewScope {
    Yes,
    No,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Id(pub(crate) u64);

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

// TODO: this is a hack required by the `variant_types` derive macro
type MaybeExpr<'a> = Option<Expression<'a>>;

#[variant_types::derive_variant_types]
#[derive(Debug, Clone, Copy)]
pub(crate) enum Statement<'a> {
    Declaration(Declaration<'a>),
    Expression(MaybeExpr<'a>),
    Compound(CompoundStatement<'a>),
    Return {
        return_: Token<'a>,
        expr: MaybeExpr<'a>,
    },
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Expression<'a> {
    Name(Reference<'a>),
    Integer(Token<'a>),
    Parenthesised {
        open_paren: Token<'a>,
        expr: &'a Expression<'a>,
        close_paren: Token<'a>,
    },
    Assign {
        target: &'a Expression<'a>,
        value: &'a Expression<'a>,
    },
    BinOp {
        lhs: &'a Expression<'a>,
        kind: BinOpKind,
        rhs: &'a Expression<'a>,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct Reference<'a> {
    // TODO: The location of `name` points to where this name was declared. This is unused for now,
    // but should be used in error messages to print e. g. “note: [...] was declared here:”.
    pub(crate) name: Token<'a>,
    pub(crate) ty: QualifiedType<'a>,
    pub(crate) id: Id,
    pub(crate) usage_location: Token<'a>,
    pub(crate) kind: RefKind,
    pub(crate) storage_duration: StorageDuration,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum RefKind {
    Declaration,
    TentativeDefinition,
    Definition,
}

#[derive(Debug, Clone, Copy)]
pub enum StorageDuration {
    Static,
    Automatic,
    // TODO: thread local
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

impl<'a> Statement<'a> {
    pub(crate) fn loc(&self) -> Loc<'a> {
        match self {
            Statement::Declaration(decl) => decl.reference.loc(),
            Statement::Expression(expr) => expr.as_ref().unwrap().loc(),
            Statement::Compound(_) => todo!(),
            Statement::Return { return_, expr } => {
                let loc = return_.loc();
                if let Some(expr) = expr {
                    loc.until(expr.loc())
                }
                else {
                    loc
                }
            }
        }
    }

    fn slice(&self) -> &'static str {
        unimplemented!("TODO: `variant-types` requires this, but `panko` does not really need this")
    }
}

impl<'a> Expression<'a> {
    fn loc(&self) -> Loc<'a> {
        match self {
            Expression::Name(name) => name.loc(),
            Expression::Integer(token) => token.loc(),
            Expression::Parenthesised { open_paren, expr: _, close_paren } =>
                open_paren.loc().until(close_paren.loc()),
            Expression::Assign { target, value } => target.loc().until(value.loc()),
            Expression::BinOp { lhs, kind: _, rhs } => lhs.loc().until(rhs.loc()),
        }
    }
}

impl<'a> Reference<'a> {
    pub(crate) fn name(&self) -> &'a str {
        self.ident().slice()
    }

    pub fn unique_name(&self) -> String {
        format!("{}~{}", self.ident().slice(), self.id.0)
    }

    fn ident(&self) -> Token<'a> {
        self.name
    }

    pub fn loc(&self) -> Loc<'a> {
        self.usage_location.loc()
    }

    pub(crate) fn slice(&self) -> &'a str {
        self.name.slice()
    }

    fn at(&self, location: Token<'a>) -> Self {
        assert!(matches!(location.kind, TokenKind::Identifier));
        assert_eq!(self.name.slice(), location.slice());
        Self { usage_location: location, ..*self }
    }

    pub fn kind(&self) -> RefKind {
        self.kind
    }

    pub(crate) fn at_decl(&self) -> Self {
        self.at(self.name)
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

    fn lookup_innermost(&mut self, name: &'a str) -> Entry<&'a str, Reference<'a>> {
        self.names.last_mut().entry(name)
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
    fn add(
        &mut self,
        name: Token<'a>,
        ty: QualifiedType<'a>,
        kind: RefKind,
        storage_duration: StorageDuration,
    ) -> Reference<'a> {
        let sess = self.sess;
        let id = self.id();
        let reference = Reference {
            name,
            ty,
            id,
            usage_location: name,
            kind,
            storage_duration,
        };
        match self.lookup_innermost(name) {
            Entry::Occupied(mut entry) => {
                let previous_definition = entry.get_mut();
                if matches!(kind, RefKind::Definition)
                    && matches!(previous_definition.kind, RefKind::Definition)
                {
                    sess.emit(Diagnostic::AlreadyDefined {
                        at: reference,
                        previous_definition: previous_definition.at(previous_definition.name),
                    });
                }
                if previous_definition.ty != ty {
                    sess.emit(Diagnostic::AlreadyDefinedWithDifferentType {
                        at: reference,
                        previous_definition: previous_definition
                            .at(previous_definition.usage_location),
                    });
                }
                if kind > previous_definition.kind {
                    previous_definition.name = name;
                    previous_definition.kind = kind;
                }
                Reference {
                    kind,
                    usage_location: name,
                    ..*previous_definition
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(reference);
                reference
            }
        }
    }

    fn lookup(&self, name: Token<'a>) -> Option<Reference<'a>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.lookup(name.slice()))
            .map(|reference| reference.at(name))
    }

    fn lookup_innermost(&mut self, name: Token<'a>) -> Entry<&'a str, Reference<'a>> {
        self.scopes.last_mut().lookup_innermost(name.slice())
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

    fn is_in_global_scope(&self) -> bool {
        self.scopes.len() == 1
    }
}

fn resolve_function_definition<'a>(
    scopes: &mut Scopes<'a>,
    def: &'a ast::FunctionDefinition<'a>,
) -> FunctionDefinition<'a> {
    let reference = scopes.add(
        def.name,
        def.ty,
        RefKind::Definition,
        StorageDuration::Static,
    );
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
                let reference = scopes.add(
                    name,
                    param.ty,
                    RefKind::Definition,
                    StorageDuration::Automatic,
                );
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
    storage_duration: StorageDuration,
) -> Declaration<'a> {
    let kind = if decl.initialiser.is_some() {
        RefKind::Definition
    }
    else {
        match decl.ty.ty {
            Type::Function(_) => RefKind::Declaration,
            _ =>
                if scopes.is_in_global_scope() {
                    RefKind::TentativeDefinition
                }
                else {
                    RefKind::Definition
                },
        }
    };
    let storage_duration = match decl.ty.ty {
        Type::Function(_) => StorageDuration::Static,
        _ => storage_duration,
    };
    let reference = scopes.add(decl.name, decl.ty, kind, storage_duration);
    Declaration {
        reference,
        initialiser: try { resolve_expr(scopes, &decl.initialiser?) },
    }
}

fn resolve_stmt<'a>(scopes: &mut Scopes<'a>, stmt: &ast::Statement<'a>) -> Statement<'a> {
    match stmt {
        ast::Statement::Declaration(decl) => Statement::Declaration(resolve_declaration(
            scopes,
            decl,
            StorageDuration::Automatic,
        )),
        ast::Statement::Expression(expr) =>
            Statement::Expression(try { resolve_expr(scopes, expr.as_ref()?) }),
        ast::Statement::Compound(stmts) =>
            Statement::Compound(resolve_compound_statement(scopes, stmts, OpenNewScope::Yes)),
        ast::Statement::Return { return_, expr } => Statement::Return {
            return_: *return_,
            expr: try { resolve_expr(scopes, expr.as_ref()?) },
        },
    }
}

fn resolve_expr<'a>(scopes: &mut Scopes<'a>, expr: &ast::Expression<'a>) -> Expression<'a> {
    match expr {
        ast::Expression::Name(name) => Expression::Name(
            scopes
                .lookup(*name)
                .unwrap_or_else(|| todo!("name error: {name:#?}")),
        ),
        ast::Expression::Integer(integer) => Expression::Integer(*integer),
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
        ast::Expression::BinOp { lhs, kind, rhs } => Expression::BinOp {
            lhs: scopes.sess.alloc(resolve_expr(scopes, lhs)),
            kind: *kind,
            rhs: scopes.sess.alloc(resolve_expr(scopes, rhs)),
        },
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
            ast::ExternalDeclaration::Declaration(decl) => ExternalDeclaration::Declaration(
                resolve_declaration(scopes, decl, StorageDuration::Static),
            ),
        })),
    }
}
