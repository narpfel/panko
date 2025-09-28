use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::path::Path;

use ariadne::Color::Blue;
use ariadne::Color::Red;
use ariadne::Fmt as _;
use itertools::Itertools as _;
use panko_lex::Loc;
use panko_lex::Token;
use panko_parser as cst;
use panko_parser::BinOp;
use panko_parser::IncrementOp;
use panko_parser::LogicalOp;
use panko_parser::StorageClassSpecifierKind;
use panko_parser::UnaryOp;
use panko_parser::ast;
use panko_parser::ast::FromError;
use panko_parser::ast::Session;
use panko_parser::nonempty;
use panko_parser::sexpr_builder::SExpr;
use panko_report::Report;
use panko_report::Sliced as _;

use crate::ty;

mod as_sexpr;

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
}

type TypeofExpr<'a> = Typeof<'a>;
type LengthExpr<'a> = Option<&'a Expression<'a>>;
type ArrayType<'a> = ty::ArrayType<'a, TypeofExpr<'a>, LengthExpr<'a>>;
type FunctionType<'a> = ty::FunctionType<'a, TypeofExpr<'a>, LengthExpr<'a>>;
type ParameterDeclaration<'a> = ty::ParameterDeclaration<'a, TypeofExpr<'a>, LengthExpr<'a>>;
pub(crate) type Type<'a> = ty::Type<'a, TypeofExpr<'a>, LengthExpr<'a>>;
pub(crate) type QualifiedType<'a> = ty::QualifiedType<'a, TypeofExpr<'a>, LengthExpr<'a>>;

#[derive(Debug)]
enum OpenNewScope {
    Yes,
    No,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Id(pub(crate) u64);

#[derive(Debug, Clone, Copy)]
pub struct TranslationUnit<'a> {
    pub(crate) filename: &'a Path,
    pub(crate) decls: &'a [ExternalDeclaration<'a>],
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ExternalDeclaration<'a> {
    FunctionDefinition(FunctionDefinition<'a>),
    Declaration(Declaration<'a>),
    Typedef(Typedef<'a>),
    Error(&'a dyn Report),
    Redeclared(Redeclared<'a>),
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

    fn loc(&self) -> Loc<'a> {
        match self {
            Self::ValueAsTypedef { at, reference: _ } => at.loc(),
            Self::TypedefAsValue { at, typedef_ty: _, value_ty: _ } => at.loc(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum DeclarationOrTypedef<'a> {
    Declaration(Declaration<'a>),
    Typedef(Typedef<'a>),
    Redeclared(Redeclared<'a>),
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Declaration<'a> {
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
    #[expect(unused, reason = "TODO: `struct`s are not implemented")]
    Identifier { dot: Token<'a>, ident: Token<'a> },
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct FunctionDefinition<'a> {
    pub(crate) reference: Reference<'a>,
    pub(crate) params: ParamRefs<'a>,
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
    Typedef(Typedef<'a>),
    Expression(MaybeExpr<'a>),
    Compound(CompoundStatement<'a>),
    Return {
        return_: Token<'a>,
        expr: MaybeExpr<'a>,
    },
    Redeclared(Redeclared<'a>),
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
    pub(crate) linkage: Option<Linkage>,
    pub(crate) storage_duration: StorageDuration,
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

#[derive(Debug, Clone, Copy)]
pub(crate) enum IsInGlobalScope {
    Yes,
    No,
}

#[derive(Debug, Clone, Copy)]
pub enum StorageDuration {
    Static,
    Automatic,
    // TODO: thread local
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Linkage {
    External,
    Internal,
    None,
}

impl Linkage {
    pub(crate) fn in_sexpr(self, sexpr: SExpr) -> SExpr {
        match self {
            Self::External => sexpr.inline_string("external".to_string()),
            Self::Internal => sexpr.inline_string("internal".to_string()),
            Self::None => sexpr,
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

impl<'a> Statement<'a> {
    pub(crate) fn loc(&self) -> Loc<'a> {
        match self {
            Statement::Declaration(decl) => decl.reference.loc(),
            Statement::Typedef(typedef) => typedef.loc(),
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
            Statement::Redeclared(redeclared) => redeclared.loc(),
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
    type_names: nonempty::Vec<HashMap<&'a str, QualifiedType<'a>>>,
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

    fn lookup_ty(&self, name: &'a str) -> Option<QualifiedType<'a>> {
        self.type_names
            .iter()
            .rev()
            .find_map(|names| names.get(name))
            .copied()
    }

    fn lookup_ty_innermost(&mut self, name: &'a str) -> Entry<&'a str, QualifiedType<'a>> {
        self.type_names.last_mut().entry(name)
    }

    fn push(&mut self) {
        self.names.push(HashMap::default());
        self.type_names.push(HashMap::default());
    }

    fn pop(&mut self) {
        self.names.pop();
        self.type_names.pop();
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
    #[expect(
        clippy::too_many_arguments,
        reason = "TODO: consolidate some parameters"
    )]
    fn add(
        &mut self,
        name: &'a str,
        loc: Loc<'a>,
        ty: QualifiedType<'a>,
        linkage: Option<Linkage>,
        storage_duration: StorageDuration,
        is_parameter: IsParameter,
        is_in_global_scope: IsInGlobalScope,
    ) -> Result<Reference<'a>, QualifiedType<'a>> {
        if let Entry::Occupied(entry) = self.lookup_ty_innermost(name) {
            return Err(*entry.get());
        }

        let sess = self.sess;
        let id = self.id();
        let reference = Reference {
            name,
            loc,
            ty,
            id,
            usage_location: loc,
            linkage,
            storage_duration,
            previous_definition: None,
            is_parameter,
            is_in_global_scope,
            initialiser: None,
        };
        match self.lookup_innermost(name) {
            Entry::Occupied(mut entry) => {
                let previous_definition = entry.get_mut();
                let reference = Reference {
                    id: previous_definition.id,
                    previous_definition: Some(
                        sess.alloc(previous_definition.at(previous_definition.usage_location)),
                    ),
                    ..reference
                };
                *previous_definition = reference;
                Ok(reference)
            }
            Entry::Vacant(entry) => {
                entry.insert(reference);
                Ok(reference)
            }
        }
    }

    #[expect(clippy::result_large_err)]
    fn add_ty(
        &mut self,
        name: &'a str,
        ty: QualifiedType<'a>,
    ) -> Result<Option<QualifiedType<'a>>, Reference<'a>> {
        if let Entry::Occupied(entry) = self.lookup_innermost(name) {
            return Err(*entry.get());
        }

        match self.lookup_ty_innermost(name) {
            Entry::Occupied(entry) => Ok(Some(*entry.get())),
            Entry::Vacant(entry) => {
                entry.insert(ty);
                Ok(None)
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
            linkage: Some(Linkage::None),
            storage_duration: StorageDuration::Automatic,
            previous_definition: None,
            is_parameter: IsParameter::No,
            is_in_global_scope: IsInGlobalScope::No,
            initialiser: None,
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

    fn lookup_ty(&self, name: &'a str) -> Option<QualifiedType<'a>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.lookup_ty(name))
    }

    fn lookup_ty_innermost(&mut self, name: &'a str) -> Entry<&'a str, QualifiedType<'a>> {
        self.scopes.last_mut().lookup_ty_innermost(name)
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

    fn is_in_global_scope(&self) -> IsInGlobalScope {
        if self.scopes.len() == 1 {
            IsInGlobalScope::Yes
        }
        else {
            IsInGlobalScope::No
        }
    }

    fn add_initialiser(
        &mut self,
        reference: &Reference<'a>,
        initialiser: Option<RefInitialiser<'a>>,
    ) {
        match self.lookup_innermost(reference.name) {
            Entry::Occupied(mut entry) => entry.get_mut().initialiser = initialiser,
            Entry::Vacant(_) => unreachable!(),
        }
    }
}

fn resolve_ty<'a>(scopes: &mut Scopes<'a>, ty: &ast::QualifiedType<'a>) -> QualifiedType<'a> {
    let ast::QualifiedType { is_const, is_volatile, ty, loc } = *ty;
    let ty = match ty {
        ast::Type::Arithmetic(arithmetic) => Type::Arithmetic(arithmetic),
        ast::Type::Pointer(pointee) =>
            Type::Pointer(scopes.sess.alloc(resolve_ty(scopes, pointee))),
        ast::Type::Array(ast::ArrayType { ty, length }) => Type::Array(ArrayType {
            ty: scopes.sess.alloc(resolve_ty(scopes, ty)),
            length: try { scopes.sess.alloc(resolve_expr(scopes, length?)) },
            loc,
        }),
        ast::Type::Function(function_type) =>
            Type::Function(resolve_function_ty(scopes, &function_type)),
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
            expr: Typeof::Expr(scopes.sess.alloc(resolve_expr(scopes, expr))),
            unqual,
        },
        ast::Type::TypeofTy { unqual, ty } => Type::Typeof {
            expr: Typeof::Ty(scopes.sess.alloc(resolve_ty(scopes, ty))),
            unqual,
        },
    };
    QualifiedType { is_const, is_volatile, ty, loc }
}

fn resolve_function_ty<'a>(
    scopes: &mut Scopes<'a>,
    function_ty: &ast::FunctionType<'a>,
) -> FunctionType<'a> {
    let ast::FunctionType { params, return_type, is_varargs } = *function_ty;

    let params = match params {
        [ast::ParameterDeclaration { loc: _, ty, name: None }]
            if !is_varargs
                && let QualifiedType {
                    is_const: false,
                    is_volatile: false,
                    ty: Type::Void,
                    loc: _,
                } = resolve_ty(scopes, ty) =>
            &[],
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

    // TODO: this makes a bunch of unnecessary allocations
    let params_by_name = params
        .iter()
        .filter_map(|param| Some((param.name?.slice(), param)))
        .into_group_map();
    for params in params_by_name.values() {
        let [param, duplicates @ ..] = &params[..]
        else {
            unreachable!()
        };
        for duplicate in duplicates {
            scopes.sess.emit(Diagnostic::AlreadyDefined {
                at: duplicate.loc(),
                previous_definition: param.loc(),
            })
        }
    }

    FunctionType { params, return_type, is_varargs }
}

fn resolve_function_definition<'a>(
    scopes: &mut Scopes<'a>,
    def: &'a ast::FunctionDefinition<'a>,
) -> ExternalDeclaration<'a> {
    let ast::FunctionDefinition {
        name,
        storage_class,
        inline,
        noreturn,
        ty,
        body,
    } = def;
    let ty = resolve_ty(scopes, ty);
    let linkage = match try { storage_class.as_ref()?.kind } {
        Some(StorageClassSpecifierKind::Extern) | None => Some(Linkage::External),
        Some(StorageClassSpecifierKind::Static) => Some(Linkage::Internal),
        kind => unreachable!("invalid or unimplemented StorageClassSpecifierKind {kind:?}"),
    };
    let maybe_reference = scopes.add(
        name.slice(),
        name.loc(),
        ty,
        linkage,
        StorageDuration::Static,
        IsParameter::No,
        IsInGlobalScope::Yes,
    );
    let reference = match maybe_reference {
        Ok(reference) => {
            let initialiser = Some(RefInitialiser::FunctionBody);
            scopes.add_initialiser(&reference, initialiser);
            Reference { initialiser, ..reference }
        }
        Err(ty) =>
            return scopes.sess.emit(Diagnostic::TypedefRedeclaredAsValue {
                at: *name,
                ty,
                kind: "function",
            }),
    };
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
                let maybe_reference = scopes.add(
                    name,
                    param.loc,
                    param.ty,
                    Some(Linkage::None),
                    StorageDuration::Automatic,
                    IsParameter::Yes,
                    IsInGlobalScope::No,
                );
                match maybe_reference {
                    Ok(reference) => reference,
                    Err(_ty) => todo!("emit error"),
                }
            })
            .collect_vec(),
    );

    let body = resolve_compound_statement(scopes, body, OpenNewScope::No);
    scopes.pop();
    ExternalDeclaration::FunctionDefinition(FunctionDefinition {
        reference,
        params: ParamRefs(params),
        inline: *inline,
        noreturn: *noreturn,
        is_varargs,
        body,
    })
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

fn resolve_initialiser<'a>(
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
                        cst::Designator::Identifier { .. } =>
                            todo!("structs are not implemented yet"),
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
                    .map(|initialiser| resolve_initialiser(scopes, initialiser)),
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

fn resolve_declaration<'a>(
    scopes: &mut Scopes<'a>,
    decl: &ast::Declaration<'a>,
    storage_duration: StorageDuration,
) -> DeclarationOrTypedef<'a> {
    let ast::Declaration { ty, name, initialiser, storage_class } = decl;
    let ty = resolve_ty(scopes, ty);

    let linkage = match try { storage_class.as_ref()?.kind } {
        Some(StorageClassSpecifierKind::Typedef) => {
            let previously_declared_as = scopes.add_ty(name.slice(), ty);
            match previously_declared_as {
                Ok(previously_declared_as) =>
                    return DeclarationOrTypedef::Typedef(Typedef {
                        ty,
                        name: *name,
                        previously_declared_as,
                    }),
                Err(reference) =>
                    return DeclarationOrTypedef::Redeclared(Redeclared::ValueAsTypedef {
                        at: ty,
                        reference,
                    }),
            }
        }
        Some(StorageClassSpecifierKind::Extern) => Some(Linkage::External),
        Some(StorageClassSpecifierKind::Static) => Some(Linkage::Internal),
        Some(storage_class) => todo!("not implemented: storage class {:?}", storage_class),
        None => None,
    };

    let maybe_reference = scopes.add(
        name.slice(),
        name.loc(),
        ty,
        linkage,
        storage_duration,
        IsParameter::No,
        scopes.is_in_global_scope(),
    );
    let reference = match maybe_reference {
        Ok(reference) => reference,
        Err(typedef_ty) =>
            return DeclarationOrTypedef::Redeclared(Redeclared::TypedefAsValue {
                at: *name,
                typedef_ty,
                value_ty: ty,
            }),
    };
    // TODO: move resolving the initialiser into `Scopes::add` so that the `add_initialiser` call
    // cannot be forgotten
    let initialiser: Option<_> = try {
        let initialiser = match initialiser.as_ref()? {
            ast::Initialiser::Braced {
                open_brace,
                initialiser_list,
                close_brace,
            } => Initialiser::Braced {
                open_brace: *open_brace,
                initialiser_list: scopes.sess.alloc_slice_fill_iter(
                    initialiser_list
                        .iter()
                        .map(|initialiser| resolve_initialiser(scopes, initialiser)),
                ),
                close_brace: *close_brace,
            },
            ast::Initialiser::Expression(initialiser) =>
                Initialiser::Expression(resolve_expr(scopes, initialiser)),
        };
        scopes.sess.alloc(initialiser)
    };
    let ref_initialiser = initialiser.map(RefInitialiser::Initialiser);
    scopes.add_initialiser(&reference, ref_initialiser);
    DeclarationOrTypedef::Declaration(Declaration {
        reference: Reference {
            initialiser: ref_initialiser,
            ..reference
        },
        initialiser,
    })
}

fn resolve_stmt<'a>(scopes: &mut Scopes<'a>, stmt: &ast::Statement<'a>) -> Statement<'a> {
    match stmt {
        ast::Statement::Declaration(decl) =>
            match resolve_declaration(scopes, decl, StorageDuration::Automatic) {
                DeclarationOrTypedef::Declaration(declaration) =>
                    Statement::Declaration(declaration),
                DeclarationOrTypedef::Typedef(typedef) => Statement::Typedef(typedef),
                DeclarationOrTypedef::Redeclared(redeclared) => Statement::Redeclared(redeclared),
            },
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
        ast::Expression::Error(error) => Expression::Error(*error),
        ast::Expression::Name(name) => scopes
            .lookup(name.slice(), name.loc())
            .map(Expression::Name)
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
            let target_temporary = scopes.temporary(
                target.loc(),
                Type::Pointer(
                    scopes.sess.alloc(
                        Type::Typeof {
                            expr: Typeof::Expr(target),
                            unqual: false,
                        }
                        .unqualified(),
                    ),
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
        ast::Expression::Lengthof { lengthof, ty, close_paren } => Expression::Lengthof {
            lengthof: *lengthof,
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
            let reference = scopes.temporary(operand.loc(), Type::Void.unqualified());
            let expr = Typeof::Expr(operand);
            let typeof_operand_unqual = Type::Typeof { expr, unqual: true };
            let typeof_operand = Type::Typeof { expr, unqual: false };
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
    let ast::TranslationUnit { filename, decls } = translation_unit;
    TranslationUnit {
        filename,
        decls: sess.alloc_slice_fill_iter(decls.iter().map(|decl| match decl {
            ast::ExternalDeclaration::FunctionDefinition(def) =>
                resolve_function_definition(scopes, def),
            ast::ExternalDeclaration::Declaration(decl) =>
                match resolve_declaration(scopes, decl, StorageDuration::Static) {
                    DeclarationOrTypedef::Declaration(declaration) =>
                        ExternalDeclaration::Declaration(declaration),
                    DeclarationOrTypedef::Typedef(typedef) => ExternalDeclaration::Typedef(typedef),
                    DeclarationOrTypedef::Redeclared(redeclared) =>
                        ExternalDeclaration::Redeclared(redeclared),
                },
        })),
    }
}
