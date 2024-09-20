#![feature(try_blocks)]

use std::fmt;
use std::fmt::Display;
use std::fmt::Write as _;

use indexmap::IndexMap;
use indexmap::IndexSet;
use itertools::Itertools as _;
use panko_parser::ast::Type;
use panko_parser::BinOpKind;
use panko_sema::layout::CompoundStatement;
use panko_sema::layout::Declaration;
use panko_sema::layout::Expression;
use panko_sema::layout::ExternalDeclaration;
use panko_sema::layout::FunctionDefinition;
use panko_sema::layout::LayoutedExpression;
use panko_sema::layout::Statement;
use panko_sema::layout::TranslationUnit;
use panko_sema::layout::TypedSlot;
use panko_sema::scope::RefKind;
use Register::*;

#[derive(Debug, Copy, Clone)]
#[repr(u8)]
enum Register {
    Rax,
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

        const REGISTERS: [[&str; 4]; 1] = [["al", "ax", "eax", "rax"]];
        let register_str =
            REGISTERS[self.register as usize][usize::try_from(size.ilog2()).unwrap()];
        write!(f, "{register_str}")
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

    fn copy(&mut self, tgt: &TypedSlot, src: &TypedSlot) {
        let ty = tgt.ty();
        assert!(ty == src.ty());
        match ty {
            Type::Arithmetic(_) | Type::Pointer(_) => {
                self.emit_args("mov", &[&Rax.with_ty(ty), &src]);
                self.emit_args("mov", &[tgt, &Rax.with_ty(ty)]);
            }
            Type::Function(_) | Type::Void => todo!(),
        }
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
                Expression::BinOp { .. } => todo!(),
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
                        self.copy(&reference.typed_slot(), &initialiser.typed_slot());
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
                    self.emit_args("mov", &[&Rax.typed(expr), &expr.typed_slot()]);
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
                self.emit_args("mov", &[&expr.typed_slot(), &token.slice()]);
            }
            Expression::NoopTypeConversion(inner) => {
                assert_eq!(expr.ty.ty.size(), inner.ty.ty.size());
                assert_eq!(expr.slot, inner.slot);
                self.expr(inner);
            }
            Expression::Truncate(truncate) => {
                self.expr(truncate);
                self.emit_args("mov", &[&Rax.typed(truncate), &truncate.typed_slot()]);
                self.emit_args("mov", &[&expr.typed_slot(), &Rax.typed(expr)]);
            }
            Expression::SignExtend(sign_extend) => {
                self.expr(sign_extend);
                self.emit_args("movsx", &[&Rax.typed(expr), &sign_extend.typed_slot()]);
                self.emit_args("mov", &[&expr.typed_slot(), &Rax.typed(expr)]);
            }
            Expression::ZeroExtend(zero_extend) => {
                self.expr(zero_extend);
                self.emit_args("movzx", &[&Rax.typed(expr), &zero_extend.typed_slot()]);
                self.emit_args("mov", &[&expr.typed_slot(), &Rax.typed(expr)]);
            }
            Expression::Assign { target, value } => {
                self.expr(target);
                self.expr(value);
                if target.slot != value.slot {
                    self.copy(&target.typed_slot(), &value.typed_slot());
                }
            }
            Expression::BinOp { lhs, kind, rhs } => {
                let emit_arithmetic = |cg: &mut Self, operation| {
                    cg.emit_args(operation, &[&Rax.typed(lhs), &rhs.typed_slot()]);
                };
                let emit_comparison = |cg: &mut Self, operation| {
                    emit_arithmetic(cg, "cmp");
                    cg.emit_args(operation, &[&"al"]);
                    cg.emit_args("movzx", &[&Rax.typed(expr), &"al"]);
                };

                self.expr(lhs);
                self.expr(rhs);

                self.emit_args("mov", &[&Rax.typed(lhs), &lhs.typed_slot()]);

                match kind {
                    BinOpKind::Add => emit_arithmetic(self, "add"),
                    BinOpKind::Subtract => emit_arithmetic(self, "sub"),
                    BinOpKind::Equal => emit_comparison(self, "sete"),
                    BinOpKind::NotEqual => emit_comparison(self, "setne"),
                };

                self.emit_args("mov", &[&expr.typed_slot(), &Rax.typed(expr)]);
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
