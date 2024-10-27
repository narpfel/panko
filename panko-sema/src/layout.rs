use bumpalo::Bump;
use panko_parser as cst;
use panko_parser::ast::Integral;
use panko_parser::BinOp;
use panko_report::Report;

use crate::layout::stack::Stack;
use crate::scope::Id;
use crate::scope::RefKind;
use crate::ty::QualifiedType;
use crate::typecheck;
use crate::typecheck::PtrAddOrder;
use crate::typecheck::PtrCmpKind;

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
    pub params: ParamRefs<'a>,
    pub storage_class: Option<cst::StorageClassSpecifier<'a>>,
    #[expect(unused)]
    inline: Option<cst::FunctionSpecifier<'a>>,
    #[expect(unused)]
    noreturn: Option<cst::FunctionSpecifier<'a>>,
    pub is_varargs: bool,
    pub stack_size: u64,
    pub argument_area_size: u64,
    pub body: CompoundStatement<'a>,
}

#[derive(Debug, Clone, Copy)]
pub struct ParamRefs<'a>(pub &'a [Reference<'a>]);

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
    Error(&'a dyn Report),
    Name(Reference<'a>),
    Integer(u64),
    NoopTypeConversion(&'a LayoutedExpression<'a>),
    Truncate(&'a LayoutedExpression<'a>),
    SignExtend(&'a LayoutedExpression<'a>),
    ZeroExtend(&'a LayoutedExpression<'a>),
    VoidCast(&'a LayoutedExpression<'a>),
    Assign {
        target: &'a LayoutedExpression<'a>,
        value: &'a LayoutedExpression<'a>,
    },
    IntegralBinOp {
        ty: Integral,
        lhs: &'a LayoutedExpression<'a>,
        op: BinOp<'a>,
        rhs: &'a LayoutedExpression<'a>,
    },
    PtrAdd {
        pointer: &'a LayoutedExpression<'a>,
        integral: &'a LayoutedExpression<'a>,
        pointee_size: u64,
        order: PtrAddOrder,
    },
    PtrSub {
        pointer: &'a LayoutedExpression<'a>,
        integral: &'a LayoutedExpression<'a>,
        pointee_size: u64,
    },
    PtrDiff {
        lhs: &'a LayoutedExpression<'a>,
        rhs: &'a LayoutedExpression<'a>,
        pointee_size: u64,
    },
    PtrCmp {
        lhs: &'a LayoutedExpression<'a>,
        kind: PtrCmpKind,
        rhs: &'a LayoutedExpression<'a>,
    },
    Addressof(&'a LayoutedExpression<'a>),
    Deref(&'a LayoutedExpression<'a>),
    Call {
        callee: &'a LayoutedExpression<'a>,
        args: &'a [LayoutedExpression<'a>],
        is_varargs: bool,
    },
    Negate(&'a LayoutedExpression<'a>),
    Compl(&'a LayoutedExpression<'a>),
    Not(&'a LayoutedExpression<'a>),
}

#[derive(Debug, Clone, Copy)]
pub struct Reference<'a> {
    name: &'a str,
    pub ty: QualifiedType<'a>,
    id: Id,
    kind: RefKind,
    slot: Slot<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Slot<'a> {
    Static(&'a str),
    Automatic(u64),
    Void,
}

impl<'a> FunctionDefinition<'a> {
    pub fn is_main(&self) -> bool {
        self.name() == "main"
    }

    pub fn name(&self) -> &'a str {
        self.reference.name()
    }
}

impl<'a> Reference<'a> {
    pub fn name(&self) -> &'a str {
        self.name
    }

    fn unique_name(&self) -> String {
        format!("{}~{}", self.name, self.id.0)
    }

    pub fn kind(&self) -> RefKind {
        self.kind
    }

    pub fn slot(&self) -> Slot<'a> {
        self.slot
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
        is_varargs,
        body,
    } = *def;
    let reference = stack.add(reference);
    let params =
        ParamRefs(bump.alloc_slice_fill_iter(params.0.iter().map(|&param| stack.add(param))));
    let body = layout_compound_statement(&mut stack, bump, body);
    // TODO: This depends on the ABI
    // unsure if this is correct, for now we check for correct alignment in the function prologue
    let argument_area_size = stack.argument_area_size();
    let stack_size = (stack.size() + argument_area_size).next_multiple_of(16) + 8;
    FunctionDefinition {
        reference,
        params,
        storage_class,
        inline,
        noreturn,
        stack_size,
        argument_area_size,
        is_varargs,
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
        typecheck::Expression::Error(error) => (make_slot(), Expression::Error(error)),
        typecheck::Expression::Name(name) => {
            let reference = stack.add(name);
            (reference.slot(), Expression::Name(reference))
        }
        typecheck::Expression::Integer { value, token: _ } =>
            (make_slot(), Expression::Integer(value)),
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
        typecheck::Expression::VoidCast(expr) => (
            make_slot(),
            Expression::VoidCast(bump.alloc(layout_expression(stack, bump, expr))),
        ),
        typecheck::Expression::Parenthesised { open_paren: _, expr, close_paren: _ } =>
            return layout_expression_in_slot(stack, bump, expr, target_slot),
        typecheck::Expression::Assign { target, value } => {
            let target = bump.alloc(layout_expression(stack, bump, target));
            let value_slot = match target.expr {
                // `Deref` exprs need a slot to store the pointer, so we assign a new slot to the
                // value.
                Expression::Deref(operand) => Some(stack.temporary(operand.ty.ty)),
                // For `Name` exprs, we can assign directly into the name’s slot.
                Expression::Name(_) => Some(target.slot),
                _ => unreachable!("not assignable because this expr is not an lvalue"),
            };
            let value = layout_expression_in_slot(stack, bump, value, value_slot);
            let value = bump.alloc(value);
            (target.slot, Expression::Assign { target, value })
        }
        typecheck::Expression::IntegralBinOp { ty: int, lhs, op, rhs } => {
            let slot = make_slot();
            let lhs_slot = ty.ty.is_slot_compatible(&lhs.ty.ty).then_some(slot);
            let lhs = bump.alloc(layout_expression_in_slot(stack, bump, lhs, lhs_slot));
            let rhs = bump.alloc(stack.with_block(|stack| layout_expression(stack, bump, rhs)));
            (slot, Expression::IntegralBinOp { ty: int, lhs, op, rhs })
        }
        typecheck::Expression::PtrAdd { pointer, integral, pointee_size, order } => {
            let (lhs, rhs) = order.select(pointer, integral);
            let slot = make_slot();
            let lhs = bump.alloc(layout_expression_in_slot(stack, bump, lhs, Some(slot)));
            let rhs = bump.alloc(stack.with_block(|stack| layout_expression(stack, bump, rhs)));
            let (pointer, integral) = order.select(lhs, rhs);
            (
                slot,
                Expression::PtrAdd { pointer, integral, pointee_size, order },
            )
        }
        typecheck::Expression::PtrSub { pointer, integral, pointee_size } => {
            let slot = make_slot();
            let pointer = bump.alloc(layout_expression_in_slot(stack, bump, pointer, Some(slot)));
            let integral =
                bump.alloc(stack.with_block(|stack| layout_expression(stack, bump, integral)));
            (slot, Expression::PtrSub { pointer, integral, pointee_size })
        }
        typecheck::Expression::PtrDiff { lhs, rhs, pointee_size } => {
            let slot = make_slot();
            let lhs = bump.alloc(layout_expression_in_slot(stack, bump, lhs, Some(slot)));
            let rhs = bump.alloc(stack.with_block(|stack| layout_expression(stack, bump, rhs)));
            (slot, Expression::PtrDiff { lhs, rhs, pointee_size })
        }
        typecheck::Expression::PtrCmp { lhs, kind, rhs } => {
            let slot = make_slot();
            let lhs = bump.alloc(layout_expression(stack, bump, lhs));
            let rhs = bump.alloc(stack.with_block(|stack| layout_expression(stack, bump, rhs)));
            (slot, Expression::PtrCmp { lhs, kind, rhs })
        }
        typecheck::Expression::Addressof { ampersand: _, operand } => {
            let slot = make_slot();
            // TODO: This feels iffy because this layouts the operand with type `T` into our slot
            // that should hold `ptr<T>`. Could this lead to an overlap when `T` is smaller than
            // `ptr<T>`? What happens if `T` is larger? This *should* be fine because codegen
            // doesn’t actually generate any code for the operand? Can this lead to unaligned slots
            // in child nodes of `operand`?
            let operand = bump.alloc(layout_expression_in_slot(stack, bump, operand, Some(slot)));
            (slot, Expression::Addressof(operand))
        }
        typecheck::Expression::Deref { star: _, operand } => {
            let slot = make_slot();
            let operand = bump.alloc(layout_expression(stack, bump, operand));
            (slot, Expression::Deref(operand))
        }
        typecheck::Expression::Call { callee, args, is_varargs, close_paren: _ } => {
            let slot = make_slot();
            let callee = bump.alloc(layout_expression(stack, bump, callee));
            let args = bump
                .alloc_slice_fill_iter(args.iter().map(|arg| layout_expression(stack, bump, arg)));
            // TODO: handle arguments that are not class INTEGER
            stack.function_arguments(args);
            (slot, Expression::Call { callee, args, is_varargs })
        }
        typecheck::Expression::Negate { minus: _, operand } => {
            let slot = make_slot();
            let operand = bump.alloc(layout_expression_in_slot(stack, bump, operand, Some(slot)));
            (slot, Expression::Negate(operand))
        }
        typecheck::Expression::Compl { compl: _, operand } => {
            let slot = make_slot();
            let operand = bump.alloc(layout_expression_in_slot(stack, bump, operand, Some(slot)));
            (slot, Expression::Compl(operand))
        }
        typecheck::Expression::Not { not: _, operand } => {
            let slot = make_slot();
            let operand_slot = ty.ty.is_slot_compatible(&operand.ty.ty).then_some(slot);
            let operand = layout_expression_in_slot(stack, bump, operand, operand_slot);
            (slot, Expression::Not(bump.alloc(operand)))
        }
        typecheck::Expression::Sizeof { sizeof: _, operand: _, size: value }
        | typecheck::Expression::SizeofTy {
            sizeof: _,
            ty: _,
            size: value,
            close_paren: _,
        }
        | typecheck::Expression::Alignof {
            alignof: _,
            ty: _,
            align: value,
            close_paren: _,
        } => {
            let slot = make_slot();
            (slot, Expression::Integer(value))
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
