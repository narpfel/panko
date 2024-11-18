use std::collections::hash_map::Entry;
use std::collections::HashMap;

use ariadne::Color::Blue;
use ariadne::Color::Red;
use itertools::Itertools as _;
use panko_lex::Loc;
use panko_lex::Token;
use panko_parser as cst;
use panko_parser::ast;
use panko_parser::ast::ErrorExpr;
use panko_parser::ast::Session;
use panko_parser::BinOp;
use panko_parser::IncrementOp;
use panko_parser::LogicalOp;
use panko_parser::UnaryOp;
use panko_report::Report;

use crate::nonempty;
use crate::ty;

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

    #[error("use of undeclared identifier `{at}`")]
    #[diagnostics(at(colour = Red, label = "this name has not been declared"))]
    UndeclaredName { at: Token<'a> },
}

type FunctionType<'a> = ty::FunctionType<'a, &'a Expression<'a>>;
type ParameterDeclaration<'a> = ty::ParameterDeclaration<'a, &'a Expression<'a>>;
pub(crate) type Type<'a> = ty::Type<'a, &'a Expression<'a>>;
pub(crate) type QualifiedType<'a> = ty::QualifiedType<'a, &'a Expression<'a>>;

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
    pub(crate) is_varargs: bool,
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
    Error(&'a dyn Report),
    Name(Reference<'a>),
    Integer {
        value: &'a str,
        token: Token<'a>,
    },
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
}

#[derive(Debug, Clone, Copy)]
pub struct Reference<'a> {
    // TODO: The location of `name` points to where this name was declared. This is unused for now,
    // but should be used in error messages to print e. g. “note: [...] was declared here:”.
    pub(crate) name: &'a str,
    pub(crate) loc: Loc<'a>,
    pub(crate) ty: QualifiedType<'a>,
    pub(crate) id: Id,
    pub(crate) usage_location: Loc<'a>,
    pub(crate) kind: RefKind,
    pub(crate) storage_duration: StorageDuration,
    pub(crate) previous_definition: Option<&'a Self>,
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
    pub(crate) fn loc(&self) -> Loc<'a> {
        match self {
            Expression::Error(error) => error.location(),
            Expression::Name(name) => name.loc(),
            Expression::Integer { value: _, token } => token.loc(),
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
            Expression::Conditional { condition, then: _, or_else } =>
                condition.loc().until(or_else.loc()),
            Expression::Comma { lhs, rhs } => lhs.loc().until(rhs.loc()),
            Expression::Increment {
                operator,
                operand,
                fixity: _,
                reference: _,
            } => operator.loc().until(operand.loc()),
        }
    }
}

impl<'a> ErrorExpr<'a> for Expression<'a> {
    fn from_error(error: &'a dyn Report) -> Self {
        Self::Error(error)
    }
}

impl<'a> Reference<'a> {
    pub fn unique_name(&self) -> String {
        format!("{}~{}", self.name, self.id.0)
    }

    #[expect(
        clippy::misnamed_getters,
        reason = "`loc` should actually return the `usage_location` and not the `loc` where this reference was declared"
    )]
    pub fn loc(&self) -> Loc<'a> {
        self.usage_location
    }

    pub(crate) fn slice(&self) -> &'a str {
        self.name
    }

    fn at(&self, location: Loc<'a>) -> Self {
        assert_eq!(self.name, location.slice());
        Self { usage_location: location, ..*self }
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
        pointer: Reference<'a>,
        copy: Reference<'a>,
    },
}

impl IncrementFixity<'_> {
    fn str(&self) -> &'static str {
        match self {
            IncrementFixity::Prefix => "pre",
            IncrementFixity::Postfix { pointer: _, copy: _ } => "post",
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
        name: &'a str,
        loc: Loc<'a>,
        ty: QualifiedType<'a>,
        kind: RefKind,
        storage_duration: StorageDuration,
    ) -> Reference<'a> {
        let sess = self.sess;
        let id = self.id();
        let reference = Reference {
            name,
            loc,
            ty,
            id,
            usage_location: loc,
            kind,
            storage_duration,
            previous_definition: None,
        };
        match self.lookup_innermost(name) {
            Entry::Occupied(mut entry) => {
                let previous_definition = entry.get_mut();
                if matches!(kind, RefKind::Definition)
                    && matches!(previous_definition.kind, RefKind::Definition)
                {
                    sess.emit(Diagnostic::AlreadyDefined {
                        at: reference,
                        previous_definition: previous_definition.at(previous_definition.loc),
                    })
                }

                let reference = Reference {
                    id: previous_definition.id,
                    previous_definition: Some(
                        sess.alloc(previous_definition.at(previous_definition.usage_location)),
                    ),
                    ..reference
                };

                if kind > previous_definition.kind {
                    previous_definition.name = name;
                    previous_definition.kind = kind;
                }

                reference
            }
            Entry::Vacant(entry) => {
                entry.insert(reference);
                reference
            }
        }
    }

    fn temporary(&mut self, loc: Loc<'a>, ty: QualifiedType<'a>) -> Reference<'a> {
        let id = self.id();
        Reference {
            name: "unnamed-temporary",
            loc,
            ty,
            id,
            usage_location: loc,
            kind: RefKind::Definition,
            storage_duration: StorageDuration::Automatic,
            previous_definition: None,
        }
    }

    fn lookup(&self, name: &'a str, loc: Loc<'a>) -> Option<Reference<'a>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.lookup(name))
            .map(|reference| reference.at(loc))
    }

    fn lookup_innermost(&mut self, name: &'a str) -> Entry<&'a str, Reference<'a>> {
        self.scopes.last_mut().lookup_innermost(name)
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

fn resolve_ty<'a>(scopes: &mut Scopes<'a>, ty: &ast::QualifiedType<'a>) -> QualifiedType<'a> {
    let ast::QualifiedType { is_const, is_volatile, ty, loc } = *ty;
    let ty = match ty {
        ast::Type::Arithmetic(arithmetic) => Type::Arithmetic(arithmetic),
        ast::Type::Pointer(pointee) =>
            Type::Pointer(scopes.sess.alloc(resolve_ty(scopes, pointee))),
        ast::Type::Function(function_type) =>
            Type::Function(resolve_function_ty(scopes, &function_type)),
        ast::Type::Void => Type::Void,
    };
    QualifiedType { is_const, is_volatile, ty, loc }
}

fn resolve_function_ty<'a>(
    scopes: &mut Scopes<'a>,
    function_ty: &ast::FunctionType<'a>,
) -> FunctionType<'a> {
    let ast::FunctionType { params, return_type, is_varargs } = *function_ty;

    let params = match params {
        [ast::ParameterDeclaration {
            loc: _,
            ty:
                ast::QualifiedType {
                    is_const: false,
                    is_volatile: false,
                    ty: ast::Type::Void,
                    loc: _,
                },
            name: None,
        }] if !is_varargs => &[],
        params => params,
    };

    let params = scopes.sess.alloc_slice_fill_iter(params.iter().map(
        |&ast::ParameterDeclaration { loc, ty, name }| ParameterDeclaration {
            loc,
            ty: resolve_ty(scopes, &ty),
            name,
        },
    ));
    let return_type = scopes.sess.alloc(resolve_ty(scopes, return_type));

    FunctionType { params, return_type, is_varargs }
}

fn resolve_function_definition<'a>(
    scopes: &mut Scopes<'a>,
    def: &'a ast::FunctionDefinition<'a>,
) -> FunctionDefinition<'a> {
    let ast::FunctionDefinition {
        name,
        storage_class,
        inline,
        noreturn,
        ty,
        body,
    } = def;
    let ty = resolve_ty(scopes, ty);
    let reference = scopes.add(
        name.slice(),
        name.loc(),
        ty,
        RefKind::Definition,
        StorageDuration::Static,
    );
    scopes.push();

    let FunctionType { params, return_type: _, is_varargs } = match ty {
        QualifiedType {
            is_const: false,
            is_volatile: false,
            ty: Type::Function(function_ty),
            loc: _,
        } => function_ty,
        QualifiedType { ty: Type::Function(_), .. } =>
            unreachable!("function types cannot be qualified"),
        // TODO: what about `typeof`?
        non_function_ty => {
            // TODO: this should be `Type::Error`
            let () = scopes
                .sess
                .emit(Diagnostic::FunctionDeclaratorDoesNotHaveFunctionType { at: *name });
            FunctionType {
                params: &[],
                return_type: scopes.sess.alloc(non_function_ty),
                is_varargs: false,
            }
        }
    };

    let params = scopes.sess.alloc_slice_copy(
        &params
            .iter()
            .enumerate()
            .map(|(i, param)| {
                let name = param.name.map_or_else(
                    || {
                        scopes
                            .sess
                            .alloc_str(&format!("{}.unnamed_parameter.{i}", name.slice()))
                    },
                    |name| name.slice(),
                );
                let reference = scopes.add(
                    name,
                    param.loc,
                    param.ty,
                    RefKind::Definition,
                    StorageDuration::Automatic,
                );
                reference
            })
            .collect_vec(),
    );

    let body = resolve_compound_statement(scopes, body, OpenNewScope::No);
    scopes.pop();
    FunctionDefinition {
        reference,
        params: ParamRefs(params),
        storage_class: *storage_class,
        inline: *inline,
        noreturn: *noreturn,
        is_varargs,
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
    let ast::Declaration { ty, name, initialiser } = decl;
    let ty = resolve_ty(scopes, ty);

    let kind = if initialiser.is_some() {
        RefKind::Definition
    }
    else {
        match ty.ty {
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
    let storage_duration = match ty.ty {
        Type::Function(_) => StorageDuration::Static,
        _ => storage_duration,
    };
    Declaration {
        reference: scopes.add(name.slice(), name.loc(), ty, kind, storage_duration),
        initialiser: try { resolve_expr(scopes, initialiser.as_ref()?) },
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

fn resolve_assoc<'a>(
    scopes: &mut Scopes<'a>,
    assoc: &ast::GenericAssociation<'a>,
) -> GenericAssociation<'a> {
    match assoc {
        ast::GenericAssociation::Ty { ty, expr } => GenericAssociation::Ty {
            ty: resolve_ty(scopes, ty),
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
        ast::Expression::Name(name) => scopes
            .lookup(name.slice(), name.loc())
            .map(Expression::Name)
            .unwrap_or_else(|| scopes.sess.emit(Diagnostic::UndeclaredName { at: *name })),
        ast::Expression::Integer(token) =>
            Expression::Integer { value: token.slice(), token: *token },
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
            let target_temporary = scopes.temporary(
                target.loc(),
                Type::Pointer(
                    scopes
                        .sess
                        .alloc(Type::Typeof { expr: target, unqual: false }.unqualified()),
                )
                .unqualified(),
            );
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
            ty: resolve_ty(scopes, ty),
            close_paren: *close_paren,
        },
        ast::Expression::Alignof { alignof, ty, close_paren } => Expression::Alignof {
            alignof: *alignof,
            ty: resolve_ty(scopes, ty),
            close_paren: *close_paren,
        },
        ast::Expression::Cast { open_paren, ty, expr } => Expression::Cast {
            open_paren: *open_paren,
            ty: resolve_ty(scopes, ty),
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
        ast::Expression::Conditional { condition, then, or_else } => Expression::Conditional {
            condition: scopes.sess.alloc(resolve_expr(scopes, condition)),
            then: scopes.sess.alloc(resolve_expr(scopes, then)),
            or_else: scopes.sess.alloc(resolve_expr(scopes, or_else)),
        },
        ast::Expression::Comma { lhs, rhs } => Expression::Comma {
            lhs: scopes.sess.alloc(resolve_expr(scopes, lhs)),
            rhs: scopes.sess.alloc(resolve_expr(scopes, rhs)),
        },
        ast::Expression::Increment { operator, operand, fixity } => {
            let operand = scopes.sess.alloc(resolve_expr(scopes, operand));
            let reference = scopes.temporary(operand.loc(), Type::Void.unqualified());
            let typeof_operand_unqual = Type::Typeof { expr: operand, unqual: true };
            let typeof_operand = Type::Typeof { expr: operand, unqual: false };
            Expression::Increment {
                operator: *operator,
                operand,
                fixity: match fixity {
                    cst::IncrementFixity::Prefix => IncrementFixity::Prefix,
                    cst::IncrementFixity::Postfix => IncrementFixity::Postfix {
                        pointer: scopes.temporary(
                            operand.loc(),
                            Type::Pointer(scopes.sess.alloc(typeof_operand.unqualified()))
                                .unqualified(),
                        ),
                        copy: scopes.temporary(operand.loc(), typeof_operand_unqual.unqualified()),
                    },
                },
                reference,
            }
        }
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
