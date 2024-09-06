#![feature(try_blocks)]

use std::fmt::Display;
use std::fmt::Write as _;

use indexmap::IndexMap;
use indexmap::IndexSet;
use itertools::Itertools as _;
use panko_parser::ast::Type;
use panko_sema::layout::CompoundStatement;
use panko_sema::layout::Declaration;
use panko_sema::layout::Expression;
use panko_sema::layout::ExternalDeclaration;
use panko_sema::layout::FunctionDefinition;
use panko_sema::layout::LayoutedExpression;
use panko_sema::layout::Statement;
use panko_sema::layout::TranslationUnit;
use panko_sema::scope::RefKind;

#[derive(Debug, Default)]
struct Codegen<'a> {
    tentative_definitions: IndexMap<&'a str, Type<'a>>,
    defined: IndexSet<&'a str>,
    current_function_stack_size: Option<u64>,
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

    fn emit_args(&mut self, instr: &str, args: &[&dyn Display]) {
        writeln!(
            self.code,
            "    {instr}{}{}",
            if args.is_empty() { "" } else { " " },
            args.iter().format(", "),
        )
        .unwrap();
    }

    fn function_definition(&mut self, def: &FunctionDefinition) {
        self.block(2);
        assert!(
            def.storage_class.is_none(),
            "TODO: only non-static functions are implemented",
        );
        self.directive("globl", &[&def.name()]);
        self.directive("text", &[]);
        self.directive("type", &[&def.name(), &"@function"]);
        self.label(def.name());
        self.emit_args("sub", &[&"sp", &def.stack_size]);
        self.current_function_stack_size = Some(def.stack_size);
        self.compound_statement(def.body);
        assert_eq!(
            self.current_function_stack_size.take(),
            Some(def.stack_size),
            "`current_function_stack_size` is not changed",
        );
        self.emit_args("add", &[&"sp", &def.stack_size]);
        if def.name() == "main" {
            self.emit("xor eax, eax");
        }
        self.emit("ret");
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
                Expression::ImplicitConversion(_) => todo!(),
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
            Statement::Declaration(decl) =>
                if let Some(initialiser) = decl.initialiser.as_ref() {
                    self.expr(initialiser)
                },
            Statement::Expression(expr) =>
                if let Some(expr) = expr.as_ref() {
                    self.expr(expr);
                },
            Statement::Compound(stmts) => self.compound_statement(*stmts),
            Statement::Return(expr) => {
                if let Some(expr) = expr.as_ref() {
                    self.expr(expr);
                    self.emit_args("mov", &[&"eax", &expr.slot]);
                }
                self.emit_args("add", &[&"sp", &self.current_function_stack_size.unwrap()]);
                self.emit("ret");
            }
        }
    }

    fn expr(&mut self, expr: &LayoutedExpression) {
        match expr.expr {
            Expression::Name(_reference) => (), // already in memory
            Expression::Integer(token) => {
                self.emit_args("mov", &[&expr.slot, &token.slice()]);
            }
            Expression::ImplicitConversion(_) => todo!(),
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
