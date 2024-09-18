use std::fmt;

use bumpalo::Bump;
use panko_lex::Token;
use panko_parser as cst;
use panko_parser::ast::QualifiedType;
use panko_parser::ast::Type;
use panko_parser::BinOpKind;

use crate::layout::stack::Stack;
use crate::scope::Id;
use crate::scope::RefKind;
use crate::typecheck;

mod as_sexpr;
mod stack;

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
    pub initialiser: Option<LayoutedExpression<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionDefinition<'a> {
    reference: Reference<'a>,
    params: ParamRefs<'a>,
    pub storage_class: Option<cst::StorageClassSpecifier<'a>>,
    #[expect(unused)]
    inline: Option<cst::FunctionSpecifier<'a>>,
    #[expect(unused)]
    noreturn: Option<cst::FunctionSpecifier<'a>>,
    pub stack_size: u64,
    pub body: CompoundStatement<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct ParamRefs<'a>(pub(crate) &'a [Reference<'a>]);

#[derive(Debug, Clone, Copy)]
pub struct CompoundStatement<'a>(pub &'a [Statement<'a>]);

#[derive(Debug, Clone, Copy)]
pub enum Statement<'a> {
    Declaration(Declaration<'a>),
    Expression(Option<LayoutedExpression<'a>>),
    Compound(CompoundStatement<'a>),
    Return(Option<LayoutedExpression<'a>>),
}

#[derive(Debug, Clone, Copy)]
pub struct LayoutedExpression<'a> {
    pub ty: QualifiedType<'a>,
    pub slot: Slot<'a>,
    pub expr: Expression<'a>,
}

#[derive(Debug, Clone, Copy)]
pub enum Expression<'a> {
    Name(Reference<'a>),
    Integer(Token<'a>),
    NoopTypeConversion(&'a LayoutedExpression<'a>),
    Truncate(&'a LayoutedExpression<'a>),
    SignExtend(&'a LayoutedExpression<'a>),
    ZeroExtend(&'a LayoutedExpression<'a>),
    Assign {
        target: &'a LayoutedExpression<'a>,
        value: &'a LayoutedExpression<'a>,
    },
    BinOp {
        lhs: &'a LayoutedExpression<'a>,
        kind: BinOpKind,
        rhs: &'a LayoutedExpression<'a>,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct Reference<'a> {
    name: Token<'a>,
    pub ty: QualifiedType<'a>,
    id: Id,
    kind: RefKind,
    slot: Slot<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Slot<'a> {
    Static(&'a str),
    Automatic(u64),
}

#[derive(Debug, Clone, Copy)]
pub struct TypedSlot<'a> {
    slot: Slot<'a>,
    ty: &'a Type<'a>,
}

impl<'a> FunctionDefinition<'a> {
    pub fn is_main(&self) -> bool {
        self.name() == "main"
    }

    pub fn name(&self) -> &'a str {
        self.reference.name()
    }
}

impl<'a> LayoutedExpression<'a> {
    pub fn typed_slot(&'a self) -> TypedSlot<'a> {
        self.slot.typed(&self.ty.ty)
    }
}

impl<'a> TypedSlot<'a> {
    pub fn ty(&self) -> &'a Type<'a> {
        self.ty
    }
}

impl fmt::Display for TypedSlot<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let size = match self.ty {
            Type::Arithmetic(_) | Type::Pointer(_) => self.ty.size(),
            Type::Function(_) | Type::Void => unreachable!(),
        };
        const PTR_TYPES: [&str; 4] = ["byte", "word", "dword", "qword"];
        let ptr_type = PTR_TYPES[usize::try_from(size.ilog2()).unwrap()];
        match self.slot {
            Slot::Static(s) => write!(f, "{ptr_type} ptr [rip + {s}]"),
            Slot::Automatic(stack_offset) => write!(f, "{ptr_type} ptr [rsp + {stack_offset}]"),
        }
    }
}

impl<'a> Slot<'a> {
    fn typed(self, ty: &'a Type<'a>) -> TypedSlot<'a> {
        TypedSlot { slot: self, ty }
    }
}

impl<'a> Reference<'a> {
    pub fn name(&self) -> &'a str {
        self.name.slice()
    }

    fn unique_name(&self) -> String {
        format!("{}~{}", self.name.slice(), self.id.0)
    }

    pub fn kind(&self) -> RefKind {
        self.kind
    }

    pub fn slot(&self) -> Slot<'a> {
        self.slot
    }

    pub fn typed_slot(&'a self) -> TypedSlot<'a> {
        self.slot.typed(&self.ty.ty)
    }
}

fn layout_function_definition<'a>(
    bump: &'a Bump,
    def: &typecheck::FunctionDefinition<'a>,
) -> FunctionDefinition<'a> {
    let mut stack = Stack::default();
    let typecheck::FunctionDefinition {
        reference,
        params,
        storage_class,
        inline,
        noreturn,
        body,
    } = *def;
    let reference = stack.add(reference);
    let params =
        ParamRefs(bump.alloc_slice_fill_iter(params.0.iter().map(|&param| stack.add(param))));
    let body = layout_compound_statement(&mut stack, bump, body);
    // TODO: This depends on the ABI
    // unsure if this is correct, for now we check for correct alignment in the function prologue
    let stack_size = stack.size().next_multiple_of(16);
    FunctionDefinition {
        reference,
        params,
        storage_class,
        inline,
        noreturn,
        stack_size,
        body,
    }
}

fn layout_declaration<'a>(
    stack: &mut Stack<'a>,
    bump: &'a Bump,
    decl: &typecheck::Declaration<'a>,
) -> Declaration<'a> {
    let typecheck::Declaration { reference, initialiser } = *decl;
    let reference = stack.add(reference);
    let initialiser = stack.with_block(|stack| try {
        layout_expression_in_slot(stack, bump, initialiser.as_ref()?, Some(reference.slot))
    });
    Declaration { reference, initialiser }
}

fn layout_compound_statement<'a>(
    stack: &mut Stack<'a>,
    bump: &'a Bump,
    stmts: typecheck::CompoundStatement<'a>,
) -> CompoundStatement<'a> {
    let stmts = stack.with_block(|stack| {
        bump.alloc_slice_fill_iter(
            stmts
                .0
                .iter()
                .map(|stmt| layout_statement(stack, bump, stmt)),
        )
    });
    CompoundStatement(stmts)
}

fn layout_statement<'a>(
    stack: &mut Stack<'a>,
    bump: &'a Bump,
    stmt: &typecheck::Statement<'a>,
) -> Statement<'a> {
    match stmt {
        typecheck::Statement::Declaration(decl) =>
            Statement::Declaration(layout_declaration(stack, bump, decl)),
        typecheck::Statement::Expression(expr) => stack.with_block(|stack| {
            Statement::Expression(try { layout_expression(stack, bump, expr.as_ref()?) })
        }),
        typecheck::Statement::Compound(stmts) =>
            Statement::Compound(layout_compound_statement(stack, bump, *stmts)),
        typecheck::Statement::Return(expr) => stack.with_block(|stack| {
            Statement::Return(try { layout_expression(stack, bump, expr.as_ref()?) })
        }),
    }
}

fn layout_expression<'a>(
    stack: &mut Stack<'a>,
    bump: &'a Bump,
    expr: &typecheck::TypedExpression<'a>,
) -> LayoutedExpression<'a> {
    layout_expression_in_slot(stack, bump, expr, None)
}

fn layout_expression_in_slot<'a>(
    stack: &mut Stack<'a>,
    bump: &'a Bump,
    expr: &typecheck::TypedExpression<'a>,
    target_slot: Option<Slot<'a>>,
) -> LayoutedExpression<'a> {
    let typecheck::TypedExpression { ty, expr } = *expr;
    let mut make_slot = || target_slot.unwrap_or_else(|| stack.temporary(ty.ty));
    let (slot, expr) = match expr {
        typecheck::Expression::Name(name) => {
            let reference = stack.add(name);
            (reference.slot(), Expression::Name(reference))
        }
        typecheck::Expression::Integer(integer) => (make_slot(), Expression::Integer(integer)),
        typecheck::Expression::NoopTypeConversion(expr) => {
            let expr = layout_expression_in_slot(stack, bump, expr, target_slot);
            (expr.slot, Expression::NoopTypeConversion(bump.alloc(expr)))
        }
        typecheck::Expression::Truncate(truncate) => (
            make_slot(),
            Expression::Truncate(bump.alloc(layout_expression(stack, bump, truncate))),
        ),
        typecheck::Expression::SignExtend(sign_extend) => (
            make_slot(),
            Expression::SignExtend(bump.alloc(layout_expression(stack, bump, sign_extend))),
        ),
        typecheck::Expression::ZeroExtend(zero_extend) => (
            make_slot(),
            Expression::ZeroExtend(bump.alloc(layout_expression(stack, bump, zero_extend))),
        ),
        typecheck::Expression::Parenthesised { open_paren: _, expr, close_paren: _ } =>
            return layout_expression_in_slot(stack, bump, expr, target_slot),
        typecheck::Expression::Assign { target, value } => {
            let target = bump.alloc(layout_expression(stack, bump, target));
            let value = layout_expression_in_slot(stack, bump, value, Some(target.slot));
            let value = bump.alloc(value);
            (target.slot, Expression::Assign { target, value })
        }
        typecheck::Expression::BinOp { lhs, kind, rhs } => {
            let slot = make_slot();
            let lhs = bump.alloc(layout_expression_in_slot(stack, bump, lhs, Some(slot)));
            let rhs = bump.alloc(stack.with_block(|stack| layout_expression(stack, bump, rhs)));
            (slot, Expression::BinOp { lhs, kind, rhs })
        }
    };
    LayoutedExpression { ty, slot, expr }
}

pub fn layout<'a>(
    bump: &'a Bump,
    translation_unit: typecheck::TranslationUnit<'a>,
) -> TranslationUnit<'a> {
    TranslationUnit {
        decls: bump.alloc_slice_fill_iter(translation_unit.decls.iter().map(|decl| match decl {
            typecheck::ExternalDeclaration::FunctionDefinition(def) =>
                ExternalDeclaration::FunctionDefinition(layout_function_definition(bump, def)),
            typecheck::ExternalDeclaration::Declaration(decl) =>
            // TODO: declarations in the global scope should not get a stack
                ExternalDeclaration::Declaration(layout_declaration(
                    &mut Stack::default(),
                    bump,
                    decl,
                )),
        })),
    }
}
