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
    fn op(&self, operand: &'a impl AsOperand) -> Operand<'a> {
        operand.as_operand()
    }

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

    // TODO: use a bespoke trait that is implemented by `LayoutedExpression` and `TypedRegister`
    // instead of `Display`
    fn emit_args(&mut self, instr: &str, args: &[&dyn Display]) {
        writeln!(
            self.code,
            "    {instr}{}{}",
            if args.is_empty() { "" } else { " " },
            args.iter().format(", "),
        )
        .unwrap();
    }

    fn copy(&mut self, tgt: &Operand, src: &Operand) {
        let ty = tgt.ty();
        assert!(ty == src.ty());
        match ty {
            Type::Arithmetic(_) | Type::Pointer(_) => {
                self.emit_args("mov", &[&Rax.with_ty(ty), &src]);
                self.emit_args("mov", &[tgt, &Rax.with_ty(ty)]);
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
            self.emit_args("imul", &[&"r10", &self.op(integral), &pointee_size]);
        }
        else {
            self.emit_args("movabs", &[&"r10", &pointee_size]);
            self.emit_args("imul", &[&"r10", &self.op(integral)]);
        }
        self.emit_args(operation, &[&"rax", &"r10"]);
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
        self.emit_args("sub", &[&"rsp", &def.stack_size]);
        self.current_function = Some(def);

        for parameter in def.params.0.iter().zip_longest(ARGUMENT_REGISTERS) {
            match parameter {
                EitherOrBoth::Both(param, register) =>
                    self.emit_args("mov", &[&self.op(param), &register.with_ty(&param.ty.ty)]),
                EitherOrBoth::Left(_) => todo!("unimplemented: more than six arguments"),
                EitherOrBoth::Right(_) => break,
            }
        }

        self.compound_statement(def.body);
        assert_eq!(
            self.current_function.take().map(std::ptr::from_ref),
            Some(std::ptr::from_ref(def)),
            "`current_function` is not changed",
        );
        self.emit_args("add", &[&"rsp", &def.stack_size]);
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
                Expression::Integer(token) => self.constant(
                    &token.slice().parse::<u128>().unwrap().to_le_bytes()
                        [..usize::try_from(ty.size()).unwrap()],
                ),
                Expression::NoopTypeConversion(_) => todo!(),
                Expression::Truncate(_) => todo!(),
                Expression::SignExtend(_) => todo!(),
                Expression::ZeroExtend(_) => todo!(),
                Expression::Assign { .. } => todo!(),
                Expression::IntegralBinOp { .. } => todo!(),
                Expression::PtrAdd { .. } => todo!(),
                Expression::PtrSub { .. } => todo!(),
                Expression::PtrCmp { .. } => todo!(),
                Expression::Addressof(_) => todo!(),
                Expression::Deref(_) => todo!(),
                Expression::Call { .. } => todo!(),
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
                        self.copy(&self.op(reference), &self.op(initialiser));
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
                        self.emit_args("mov", &[&Rax.typed(expr), &self.op(expr)]);
                    }
                }
                self.emit_args("add", &[&"rsp", &self.current_function.unwrap().stack_size]);
                self.emit("ret");
            }
        }
    }

    fn expr(&mut self, expr: &LayoutedExpression) {
        match expr.expr {
            Expression::Name(_reference) => (), // already in memory
            Expression::Integer(token) => {
                self.emit_args("mov", &[&self.op(expr), &token.slice()]);
            }
            Expression::NoopTypeConversion(inner) => {
                assert_eq!(expr.ty.ty.size(), inner.ty.ty.size());
                assert_eq!(expr.slot, inner.slot);
                self.expr(inner);
            }
            Expression::Truncate(truncate) => {
                self.expr(truncate);
                self.emit_args("mov", &[&Rax.typed(truncate), &self.op(truncate)]);
                self.emit_args("mov", &[&self.op(expr), &Rax.typed(expr)]);
            }
            Expression::SignExtend(sign_extend) => {
                self.expr(sign_extend);
                self.emit_args("movsx", &[&Rax.typed(expr), &self.op(sign_extend)]);
                self.emit_args("mov", &[&self.op(expr), &Rax.typed(expr)]);
            }
            Expression::ZeroExtend(zero_extend) => {
                self.expr(zero_extend);
                self.emit_args("movzx", &[&Rax.typed(expr), &self.op(zero_extend)]);
                self.emit_args("mov", &[&self.op(expr), &Rax.typed(expr)]);
            }
            Expression::Assign { target, value } => match target.expr {
                Expression::Name(_) => {
                    self.expr(target);
                    self.expr(value);
                    if target.slot != value.slot {
                        self.copy(&self.op(target), &self.op(value));
                    }
                }
                Expression::Deref(operand) => {
                    self.expr(operand);
                    self.expr(value);
                    self.emit_args("mov", &[&R10.typed(operand), &self.op(operand)]);
                    self.copy(&Operand::pointer(R10, operand), &self.op(value));
                }
                _ => unreachable!(),
            },
            Expression::IntegralBinOp { ty, lhs, kind, rhs } => {
                assert_eq!(lhs.ty, rhs.ty);
                assert_eq!(lhs.ty.ty, Type::Arithmetic(Arithmetic::Integral(ty)));

                let emit_arithmetic = |cg: &mut Self, operation| {
                    cg.emit_args(operation, &[&Rax.typed(lhs), &cg.op(rhs)]);
                };
                let emit_comparison = |cg: &mut Self, operation| {
                    assert!(matches!(lhs.ty.ty, Type::Arithmetic(_) | Type::Pointer(_)));
                    emit_arithmetic(cg, "cmp");
                    cg.emit_args(operation, &[&"al"]);
                    cg.emit_args("movzx", &[&Rax.typed(expr), &"al"]);
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

                self.emit_args("mov", &[&Rax.typed(lhs), &self.op(lhs)]);

                match kind {
                    BinOpKind::Add => emit_arithmetic(self, "add"),
                    BinOpKind::Subtract => emit_arithmetic(self, "sub"),
                    BinOpKind::Equal => emit_comparison(self, "sete"),
                    BinOpKind::NotEqual => emit_comparison(self, "setne"),
                    BinOpKind::Less => emit_sign_dependent_comparison(self, "setl", "setb"),
                    BinOpKind::LessEqual => emit_sign_dependent_comparison(self, "setle", "setbe"),
                    BinOpKind::Greater => emit_sign_dependent_comparison(self, "setg", "seta"),
                    BinOpKind::GreaterEqual =>
                        emit_sign_dependent_comparison(self, "setge", "setae"),
                };

                self.emit_args("mov", &[&self.op(expr), &Rax.typed(expr)]);
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
                self.emit_args("mov", &[&Rax.typed(pointer), &self.op(pointer)]);
                match pointee_size {
                    size @ (1 | 2 | 4 | 8) => {
                        self.emit_args("mov", &[&"r10", &self.op(integral)]);
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
                self.emit_args("mov", &[&self.op(expr), &Rax.typed(pointer)]);
            }
            Expression::PtrSub { pointer, integral, pointee_size } => {
                self.expr(pointer);
                self.expr(integral);
                self.emit_args("mov", &[&Rax.typed(pointer), &self.op(pointer)]);
                self.emit_pointer_offset("sub", pointee_size, integral);
                self.emit_args("mov", &[&self.op(expr), &Rax.typed(pointer)]);
            }
            Expression::PtrCmp { lhs, kind, rhs } => {
                self.expr(lhs);
                self.expr(rhs);
                self.emit_args("mov", &[&Rax.typed(lhs), &self.op(lhs)]);
                self.emit_args("cmp", &[&Rax.typed(lhs), &self.op(rhs)]);
                let operation = match kind {
                    PtrCmpKind::Equal => "sete",
                    PtrCmpKind::NotEqual => "setne",
                    PtrCmpKind::Less => "setb",
                    PtrCmpKind::LessEqual => "setbe",
                    PtrCmpKind::Greater => "seta",
                    PtrCmpKind::GreaterEqual => "setae",
                };
                self.emit_args(operation, &[&"al"]);
                self.emit_args("movzx", &[&Rax.typed(expr), &"al"]);
                self.emit_args("mov", &[&self.op(expr), &Rax.typed(expr)]);
            }
            Expression::Addressof(operand) => {
                match operand.expr {
                    Expression::Name(_) => {
                        let memory_operand: &dyn Display = match operand.slot {
                            Slot::Static(_) => &self.op(operand),
                            Slot::Automatic(offset) => &Memory {
                                pointer: Rsp,
                                index: None,
                                offset: Offset::Immediate(offset),
                            },
                            Slot::Pointer { register: _ } => unreachable!("TODO???"),
                            Slot::Void => unreachable!(),
                        };
                        self.emit_args("lea", &[&"rax", memory_operand]);
                    }
                    Expression::Deref(operand) => {
                        self.expr(operand);
                        self.emit_args("mov", &[&Rax.typed(operand), &self.op(operand)]);
                    }
                    _ => unreachable!(),
                }
                self.emit_args("mov", &[&self.op(expr), &"rax"]);
            }
            Expression::Deref(operand) => {
                self.expr(operand);
                self.emit_args("mov", &[&R10.typed(operand), &self.op(operand)]);
                self.copy(&self.op(expr), &Operand::pointer(R10, operand));
            }
            Expression::Call { callee, args, is_varargs } => {
                self.expr(callee);
                for arg in args {
                    self.expr(arg);
                }
                for argument in args.iter().zip_longest(ARGUMENT_REGISTERS) {
                    match argument {
                        EitherOrBoth::Both(arg, register) =>
                            self.emit_args("mov", &[&register.typed(arg), &self.op(arg)]),
                        EitherOrBoth::Left(_) => todo!("unimplemented: more than six arguments"),
                        EitherOrBoth::Right(_) => break,
                    }
                }
                let operation = match callee.ty.ty {
                    Type::Function(_) => "lea",
                    Type::Pointer(_) => "mov",
                    _ => unreachable!(),
                };

                if is_varargs {
                    // varargs functions pass an upper bound <= 8 of the number of vector registers
                    // in `al`.
                    self.emit("xor eax, eax");
                }

                self.emit_args(operation, &[&R10, &self.op(callee)]);
                self.emit("call r10");
                if expr.ty.ty != Type::Void {
                    self.emit_args("mov", &[&self.op(expr), &Rax.typed(expr)]);
                }
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
