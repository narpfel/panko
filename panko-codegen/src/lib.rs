#![feature(try_blocks)]
#![feature(unsigned_is_multiple_of)]

use std::fmt;
use std::fmt::Display;
use std::fmt::Write as _;

use indexmap::IndexMap;
use indexmap::IndexSet;
use itertools::EitherOrBoth;
use itertools::Itertools as _;
use panko_parser::ast::Arithmetic;
use panko_parser::ast::Integral;
use panko_parser::ast::IntegralKind;
use panko_parser::ast::Signedness;
use panko_parser::BinOpKind;
use panko_sema::layout::CompoundStatement;
use panko_sema::layout::Declaration;
use panko_sema::layout::Expression;
use panko_sema::layout::ExternalDeclaration;
use panko_sema::layout::FunctionDefinition;
use panko_sema::layout::LayoutedExpression;
use panko_sema::layout::Slot;
use panko_sema::layout::Statement;
use panko_sema::layout::TranslationUnit;
use panko_sema::scope::RefKind;
use panko_sema::ty::Type;
use panko_sema::typecheck::PtrCmpKind;
use Register::*;

use crate::operand::AsOperand;
use crate::operand::Index;
use crate::operand::Memory;
use crate::operand::Offset;
use crate::operand::Operand;

mod operand;

const MAX_IMUL_IMMEDIATE: u64 = 2_u64.pow(31);
const MAX_ADDRESS_OFFSET: u64 = u32::MAX as _;
const ARGUMENT_REGISTERS: [Register; 6] = [Rdi, Rsi, Rdx, Rcx, R8, R9];

#[derive(Debug, Copy, Clone)]
#[repr(u8)]
enum Register {
    Rax,
    Rcx,
    Rsp,
    Rdi,
    Rsi,
    Rdx,
    R8,
    R9,
    R10,
    Rip,
}

impl Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Rax => "rax",
            Rcx => "rcx",
            Rsp => "rsp",
            Rdi => "rdi",
            Rsi => "rsi",
            Rdx => "rdx",
            R8 => "r8",
            R9 => "r9",
            R10 => "r10",
            Rip => "rip",
        };
        write!(f, "{s}")
    }
}

#[derive(Debug, Copy, Clone)]
struct TypedRegister<'a> {
    ty: &'a Type<'a>,
    register: Register,
}

impl Register {
    fn byte(self) -> TypedRegister<'static> {
        TypedRegister {
            ty: &const { Type::uchar() },
            register: self,
        }
    }

    fn with_ty<'a>(self, ty: &'a Type<'a>) -> TypedRegister<'a> {
        TypedRegister { ty, register: self }
    }

    fn typed<'a>(self, expr: &'a LayoutedExpression<'a>) -> TypedRegister<'a> {
        TypedRegister { ty: &expr.ty.ty, register: self }
    }
}

impl Display for TypedRegister<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let size = match self.ty {
            Type::Arithmetic(_) | Type::Pointer(_) => self.ty.size(),
            Type::Function(_) | Type::Void => unreachable!(),
        };

        let names = match self.register {
            Rax => ["al", "ax", "eax", "rax"],
            Rcx => ["cl", "cx", "ecx", "rcx"],
            Rsp => ["spl", "sp", "esp", "rsp"],
            Rdi => ["dil", "di", "edi", "rdi"],
            Rsi => ["sil", "si", "esi", "rsi"],
            Rdx => ["dl", "dx", "edx", "rdx"],
            R8 => ["r8b", "r8w", "r8d", "r8"],
            R9 => ["r9b", "r9w", "r9d", "r9"],
            R10 => ["r10b", "r10w", "r10d", "r10"],
            Rip => ["there_is_no_8_bit_version_of_rip", "ip", "eip", "rip"],
        };
        let index = match size {
            1 => 0,
            2 => 1,
            4 => 2,
            8 => 3,
            _ => unreachable!(),
        };
        write!(f, "{}", names[index])
    }
}

#[derive(Debug, Default)]
struct Codegen<'a> {
    tentative_definitions: IndexMap<&'a str, Type<'a>>,
    defined: IndexSet<&'a str>,
    current_function: Option<&'a FunctionDefinition<'a>>,
    code: String,
}

impl<'a> Codegen<'a> {
    fn block(&mut self, count: u64) {
        for _ in 0..count {
            self.code.push('\n');
        }
    }

    fn directive(&mut self, name: &str, args: &[&dyn Display]) {
        writeln!(
            self.code,
            ".{name:7}{}{}",
            if args.is_empty() { "" } else { " " },
            args.iter().format(", "),
        )
        .unwrap();
    }

    fn label(&mut self, name: &str) {
        writeln!(self.code, "{name}:").unwrap();
    }

    fn zero(&mut self, size: u64) {
        writeln!(self.code, "    .zero {size}").unwrap();
    }

    fn constant(&mut self, constant: &[u8]) {
        writeln!(self.code, "    .byte {}", constant.iter().format(", ")).unwrap();
    }

    fn emit(&mut self, instr: &str) {
        writeln!(self.code, "    {instr}").unwrap();
    }

    fn emit_args(&mut self, instr: &str, operands: &[&dyn AsOperand]) {
        writeln!(
            self.code,
            "    {instr}{}{}",
            if operands.is_empty() { "" } else { " " },
            operands
                .iter()
                .map(|op| op.as_operand(try { self.current_function?.argument_area_size }))
                .format(", "),
        )
        .unwrap();
    }

    fn copy(&mut self, tgt: &impl AsOperand, src: &impl AsOperand) {
        let tgt = tgt.as_operand(try { self.current_function?.argument_area_size });
        let src = src.as_operand(try { self.current_function?.argument_area_size });
        let ty = tgt.ty();
        assert!(ty == src.ty());
        match ty {
            Type::Arithmetic(_) | Type::Pointer(_) => {
                self.emit_args("mov", &[&Rax.with_ty(ty), &src]);
                self.emit_args("mov", &[&tgt, &Rax.with_ty(ty)]);
            }
            Type::Function(_) | Type::Void => unreachable!(),
        }
    }

    fn emit_pointer_offset(
        &mut self,
        operation: &str,
        pointee_size: u64,
        integral: &LayoutedExpression,
    ) {
        assert_eq!(
            integral.ty.ty,
            Type::Arithmetic(Arithmetic::Integral(Integral {
                signedness: Signedness::Unsigned,
                kind: IntegralKind::LongLong
            })),
        );
        if pointee_size < MAX_IMUL_IMMEDIATE {
            self.emit_args("imul", &[&R10, integral, &pointee_size]);
        }
        else {
            self.emit_args("movabs", &[&R10, &pointee_size]);
            self.emit_args("imul", &[&R10, integral]);
        }
        self.emit_args(operation, &[&Rax, &R10]);
    }

    fn function_definition(&mut self, def: &'a FunctionDefinition<'a>) {
        self.block(2);
        assert!(
            def.storage_class.is_none(),
            "TODO: only non-static functions are implemented",
        );
        self.directive("globl", &[&def.name()]);
        self.directive("text", &[]);
        self.directive("type", &[&def.name(), &"@function"]);
        self.label(def.name());

        // check that rsp is correctly aligned
        self.emit("lea r10, [rsp + 8]");
        self.emit("and r10, 0xf");
        self.emit(&format!("jnz .L.{}.entry.sp_unaligned", def.name()));

        self.block(1);
        self.emit_args("sub", &[&Rsp, &def.stack_size]);
        self.current_function = Some(def);

        for (i, parameter) in def
            .params
            .0
            .iter()
            .zip_longest(ARGUMENT_REGISTERS)
            .enumerate()
        {
            match parameter {
                EitherOrBoth::Both(param, register) =>
                    self.emit_args("mov", &[param, &register.with_ty(&param.ty.ty)]),
                EitherOrBoth::Left(param) => {
                    self.copy(
                        param,
                        &Operand::stack_parameter_eightbyte(
                            param.ty.ty,
                            i - ARGUMENT_REGISTERS.len(),
                            def.stack_size,
                        ),
                    );
                }
                EitherOrBoth::Right(_) => break,
            }
        }

        self.compound_statement(def.body);
        assert_eq!(
            self.current_function.take().map(std::ptr::from_ref),
            Some(std::ptr::from_ref(def)),
            "`current_function` is not changed",
        );
        self.emit_args("add", &[&Rsp, &def.stack_size]);
        if def.is_main() {
            self.emit("xor eax, eax");
        }
        self.emit("ret");

        self.block(1);
        self.label(&format!(".L.{}.entry.sp_unaligned", def.name()));
        self.emit("int3");
    }

    fn object_definition(&mut self, name: &str, ty: Type, initialiser: Option<&Expression>) {
        self.block(2);
        self.directive("globl", &[&name]);
        self.directive("data", &[]);
        self.directive("type", &[&name, &"@object"]);
        self.directive("size", &[&name, &ty.size()]);
        self.directive("align", &[&ty.align()]);
        self.label(name);
        match initialiser {
            Some(initialiser) => match initialiser {
                Expression::Name(_) => todo!(),
                Expression::Integer(value) =>
                    self.constant(&value.to_le_bytes()[..usize::try_from(ty.size()).unwrap()]),
                Expression::NoopTypeConversion(_) => todo!(),
                Expression::Truncate(_) => todo!(),
                Expression::SignExtend(_) => todo!(),
                Expression::ZeroExtend(_) => todo!(),
                Expression::VoidCast(_) => todo!(),
                Expression::Assign { .. } => todo!(),
                Expression::IntegralBinOp { .. } => todo!(),
                Expression::PtrAdd { .. } => todo!(),
                Expression::PtrSub { .. } => todo!(),
                Expression::PtrCmp { .. } => todo!(),
                Expression::Addressof(_) => todo!(),
                Expression::Deref(_) => todo!(),
                Expression::Call { .. } => todo!(),
                Expression::Negate(_) => todo!(),
                Expression::Compl(_) => todo!(),
                Expression::Not(_) => todo!(),
            },
            None => self.zero(ty.size()),
        }
    }

    fn external_declaration(&mut self, decl: &Declaration<'a>) {
        let name = decl.reference.name();
        let ty = decl.reference.ty.ty;
        match decl.reference.kind() {
            RefKind::Declaration => (),
            RefKind::TentativeDefinition =>
                if !self.defined.contains(name) {
                    self.tentative_definitions.insert(name, ty);
                },
            RefKind::Definition => {
                self.tentative_definitions.shift_remove(name);
                self.defined.insert(name);
                self.object_definition(name, ty, try { &decl.initialiser?.expr })
            }
        }
    }

    fn compound_statement(&mut self, stmts: CompoundStatement) {
        for stmt in stmts.0 {
            self.stmt(stmt);
        }
    }

    fn stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Declaration(Declaration { reference, initialiser }) =>
                if let Some(initialiser) = initialiser.as_ref() {
                    self.expr(initialiser);
                    if reference.slot() != initialiser.slot {
                        self.copy(reference, initialiser);
                    }
                },
            Statement::Expression(expr) =>
                if let Some(expr) = expr.as_ref() {
                    self.expr(expr);
                },
            Statement::Compound(stmts) => self.compound_statement(*stmts),
            Statement::Return(expr) => {
                if let Some(expr) = expr.as_ref() {
                    self.expr(expr);
                    if expr.ty.ty != Type::Void {
                        self.emit_args("mov", &[&Rax.typed(expr), expr]);
                    }
                }
                self.emit_args("add", &[&Rsp, &self.current_function.unwrap().stack_size]);
                self.emit("ret");
            }
        }
    }

    fn expr(&mut self, expr: &LayoutedExpression) {
        match expr.expr {
            Expression::Name(_reference) => (), // already in memory
            Expression::Integer(value) => {
                self.emit_args("movabs", &[&Rax, &value]);
                self.emit_args("mov", &[expr, &Rax.typed(expr)]);
            }
            Expression::NoopTypeConversion(inner) => {
                assert_eq!(expr.ty.ty.size(), inner.ty.ty.size());
                assert_eq!(expr.slot, inner.slot);
                self.expr(inner);
            }
            Expression::Truncate(truncate) => {
                self.expr(truncate);
                self.emit_args("mov", &[&Rax.typed(truncate), truncate]);
                self.emit_args("mov", &[expr, &Rax.typed(expr)]);
            }
            Expression::SignExtend(sign_extend) => {
                self.expr(sign_extend);
                self.emit_args("movsx", &[&Rax.typed(expr), sign_extend]);
                self.emit_args("mov", &[expr, &Rax.typed(expr)]);
            }
            Expression::ZeroExtend(zero_extend) => {
                self.expr(zero_extend);
                match zero_extend.ty.ty.size() {
                    1 | 2 => self.emit_args("movzx", &[&Rax.typed(expr), zero_extend]),
                    4 => self.emit_args("mov", &[&Rax.typed(zero_extend), zero_extend]),
                    _ => unreachable!(),
                }
                self.emit_args("mov", &[expr, &Rax.typed(expr)]);
            }
            Expression::VoidCast(expr) => self.expr(expr),
            Expression::Assign { target, value } => match target.expr {
                Expression::Name(_) => {
                    self.expr(target);
                    self.expr(value);
                    if target.slot != value.slot {
                        self.copy(target, value);
                    }
                }
                Expression::Deref(operand) => {
                    self.expr(operand);
                    self.expr(value);
                    self.emit_args("mov", &[&R10.typed(operand), operand]);
                    self.copy(&Operand::pointer(R10, operand), value);
                }
                _ => unreachable!(),
            },
            Expression::IntegralBinOp { ty, lhs, kind, rhs } => {
                if !matches!(kind, BinOpKind::LeftShift | BinOpKind::RightShift) {
                    assert_eq!(lhs.ty, rhs.ty);
                }
                assert_eq!(lhs.ty.ty, Type::Arithmetic(Arithmetic::Integral(ty)));

                let emit_arithmetic = |cg: &mut Self, operation| {
                    cg.emit_args(operation, &[&Rax.typed(lhs), rhs]);
                };
                let emit_sign_dependent_arithmetic =
                    |cg: &mut Self, operation_if_signed, operation_if_unsigned| {
                        let operation = match ty.signedness {
                            Signedness::Signed => operation_if_signed,
                            Signedness::Unsigned => operation_if_unsigned,
                        };
                        emit_arithmetic(cg, operation);
                    };
                let emit_shift = |cg: &mut Self, operation_if_signed, operation_if_unsigned| {
                    let operation = match ty.signedness {
                        Signedness::Signed => operation_if_signed,
                        Signedness::Unsigned => operation_if_unsigned,
                    };
                    cg.emit_args("mov", &[&Rcx.typed(rhs), rhs]);
                    cg.emit_args(operation, &[&Rax.typed(lhs), &Rcx.byte()]);
                };
                let emit_comparison = |cg: &mut Self, operation| {
                    assert!(matches!(lhs.ty.ty, Type::Arithmetic(_) | Type::Pointer(_)));
                    emit_arithmetic(cg, "cmp");
                    cg.emit_args(operation, &[&Rax.byte()]);
                    cg.emit_args("movzx", &[&Rax.typed(expr), &Rax.byte()]);
                };
                let emit_sign_dependent_comparison =
                    |cg: &mut Self, operation_if_signed, operation_if_unsigned| {
                        let operation = match ty.signedness {
                            Signedness::Signed => operation_if_signed,
                            Signedness::Unsigned => operation_if_unsigned,
                        };
                        emit_comparison(cg, operation);
                    };

                self.expr(lhs);
                self.expr(rhs);

                self.emit_args("mov", &[&Rax.typed(lhs), lhs]);

                match kind {
                    BinOpKind::Multiply => emit_arithmetic(self, "imul"),
                    BinOpKind::Divide | BinOpKind::Modulo => {
                        let prepare_upper_half = match ty.signedness {
                            Signedness::Signed => match ty.size() {
                                1 => "movsx ax, al",
                                2 => "cwd",
                                4 => "cdq",
                                8 => "cqo",
                                _ => unreachable!(),
                            },
                            Signedness::Unsigned => match ty.size() {
                                1 => "movzx ax, al",
                                2 | 4 | 8 => "xor edx, edx",
                                _ => unreachable!(),
                            },
                        };
                        self.emit(prepare_upper_half);
                        emit_sign_dependent_arithmetic(self, "idiv", "div");
                        if matches!(kind, BinOpKind::Modulo) {
                            match ty.size() {
                                1 => self.emit("mov al, ah"),
                                2 | 4 | 8 => self.emit_args("mov", &[&Rax, &Rdx]),
                                _ => unreachable!(),
                            }
                        }
                    }
                    BinOpKind::Add => emit_arithmetic(self, "add"),
                    BinOpKind::Subtract => emit_arithmetic(self, "sub"),
                    BinOpKind::Equal => emit_comparison(self, "sete"),
                    BinOpKind::NotEqual => emit_comparison(self, "setne"),
                    BinOpKind::Less => emit_sign_dependent_comparison(self, "setl", "setb"),
                    BinOpKind::LessEqual => emit_sign_dependent_comparison(self, "setle", "setbe"),
                    BinOpKind::Greater => emit_sign_dependent_comparison(self, "setg", "seta"),
                    BinOpKind::GreaterEqual =>
                        emit_sign_dependent_comparison(self, "setge", "setae"),
                    BinOpKind::LeftShift => emit_shift(self, "shl", "shl"),
                    BinOpKind::RightShift => emit_shift(self, "sar", "shr"),
                    BinOpKind::BitAnd => emit_arithmetic(self, "and"),
                    BinOpKind::BitXor => emit_arithmetic(self, "xor"),
                    BinOpKind::BitOr => emit_arithmetic(self, "or"),
                };

                self.emit_args("mov", &[expr, &Rax.typed(expr)]);
            }
            Expression::PtrAdd { pointer, integral, pointee_size, order } => {
                assert_eq!(
                    integral.ty.ty,
                    Type::Arithmetic(Arithmetic::Integral(Integral {
                        signedness: Signedness::Unsigned,
                        kind: IntegralKind::LongLong
                    })),
                );
                let (lhs, rhs) = order.select(pointer, integral);
                self.expr(lhs);
                self.expr(rhs);
                self.emit_args("mov", &[&Rax.typed(pointer), pointer]);
                match pointee_size {
                    size @ (1 | 2 | 4 | 8) => {
                        self.emit_args("mov", &[&R10, integral]);
                        self.emit_args(
                            "lea",
                            &[
                                &Rax.typed(pointer),
                                &Memory {
                                    pointer: Rax,
                                    index: Some(Index { register: R10, size }),
                                    offset: Offset::Immediate(0),
                                },
                            ],
                        );
                    }
                    size => self.emit_pointer_offset("add", size, integral),
                }
                self.emit_args("mov", &[expr, &Rax.typed(pointer)]);
            }
            Expression::PtrSub { pointer, integral, pointee_size } => {
                self.expr(pointer);
                self.expr(integral);
                self.emit_args("mov", &[&Rax.typed(pointer), pointer]);
                self.emit_pointer_offset("sub", pointee_size, integral);
                self.emit_args("mov", &[expr, &Rax.typed(pointer)]);
            }
            Expression::PtrCmp { lhs, kind, rhs } => {
                self.expr(lhs);
                self.expr(rhs);
                self.emit_args("mov", &[&Rax.typed(lhs), lhs]);
                self.emit_args("cmp", &[&Rax.typed(lhs), rhs]);
                let operation = match kind {
                    PtrCmpKind::Equal => "sete",
                    PtrCmpKind::NotEqual => "setne",
                    PtrCmpKind::Less => "setb",
                    PtrCmpKind::LessEqual => "setbe",
                    PtrCmpKind::Greater => "seta",
                    PtrCmpKind::GreaterEqual => "setae",
                };
                self.emit_args(operation, &[&Rax.byte()]);
                self.emit_args("movzx", &[&Rax.typed(expr), &Rax.byte()]);
                self.emit_args("mov", &[expr, &Rax.typed(expr)]);
            }
            Expression::Addressof(operand) => {
                match operand.expr {
                    Expression::Name(_) => {
                        let memory_operand: &dyn AsOperand = match operand.slot {
                            Slot::Static(_) => operand,
                            Slot::Automatic(offset) => &Memory {
                                pointer: Rsp,
                                index: None,
                                offset: Offset::Immediate(offset),
                            },
                            Slot::Void => unreachable!(),
                        };
                        self.emit_args("lea", &[&Rax, memory_operand]);
                    }
                    Expression::Deref(operand) => {
                        self.expr(operand);
                        self.emit_args("mov", &[&Rax.typed(operand), operand]);
                    }
                    _ => unreachable!(),
                }
                self.emit_args("mov", &[expr, &Rax]);
            }
            Expression::Deref(operand) => {
                self.expr(operand);
                self.emit_args("mov", &[&R10.typed(operand), operand]);
                self.copy(expr, &Operand::pointer(R10, operand));
            }
            Expression::Call { callee, args, is_varargs } => {
                self.expr(callee);
                for arg in args {
                    self.expr(arg);
                }
                for (i, argument) in args.iter().zip_longest(ARGUMENT_REGISTERS).enumerate() {
                    match argument {
                        EitherOrBoth::Both(arg, register) =>
                            self.emit_args("mov", &[&register.typed(arg), arg]),
                        EitherOrBoth::Left(arg) => {
                            self.copy(
                                &Operand::stack_argument_eightbyte(
                                    arg.ty.ty,
                                    i - ARGUMENT_REGISTERS.len(),
                                ),
                                arg,
                            );
                        }
                        EitherOrBoth::Right(_) => break,
                    }
                }

                if is_varargs {
                    // varargs functions pass an upper bound <= 8 of the number of vector registers
                    // in `al`.
                    self.emit("xor eax, eax");
                }

                self.emit_args("call", &[callee]);
                if expr.ty.ty != Type::Void {
                    self.emit_args("mov", &[expr, &Rax.typed(expr)]);
                }
            }
            Expression::Negate(operand) => {
                self.expr(operand);
                self.emit_args("mov", &[&Rax.typed(operand), operand]);
                self.emit_args("neg", &[&Rax.typed(operand)]);
                self.emit_args("mov", &[expr, &Rax.typed(expr)]);
            }
            Expression::Compl(operand) => {
                self.expr(operand);
                self.emit_args("mov", &[&Rax.typed(operand), operand]);
                self.emit_args("not", &[&Rax.typed(operand)]);
                self.emit_args("mov", &[expr, &Rax.typed(expr)]);
            }
            Expression::Not(operand) => {
                self.expr(operand);
                self.emit_args("cmp", &[operand, &0]);
                self.emit_args("sete", &[&Rax.byte()]);
                self.emit_args("movzx", &[&Rax.typed(expr), &Rax.byte()]);
                self.emit_args("mov", &[expr, &Rax.typed(expr)]);
            }
        }
    }
}

pub fn emit(translation_unit: TranslationUnit) -> String {
    let mut cg = Codegen::default();
    cg.directive("intel_syntax", &[&"noprefix"]);

    for decl in translation_unit.decls {
        match decl {
            ExternalDeclaration::FunctionDefinition(def) => cg.function_definition(def),
            ExternalDeclaration::Declaration(decl) => cg.external_declaration(decl),
        }
    }

    for (name, ty) in std::mem::take(&mut cg.tentative_definitions) {
        assert!(!cg.defined.contains(&name));
        cg.object_definition(name, ty, None);
    }

    cg.code
}
