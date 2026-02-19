#![feature(assert_matches)]
#![feature(bstr)]
#![feature(if_let_guard)]
#![feature(impl_trait_in_assoc_type)]
#![feature(try_blocks)]
#![feature(unqualified_local_imports)]

use std::borrow::Cow;
use std::bstr::ByteStr;
use std::bstr::ByteString;
use std::fmt;
use std::fmt::Display;
use std::fmt::Write as _;
use std::mem;

use indexmap::IndexMap;
use indexmap::IndexSet;
use itertools::EitherOrBoth;
use itertools::Itertools as _;
use panko_lex::Loc;
use panko_parser::BinOpKind;
use panko_parser::Comparison;
use panko_parser::LogicalOpKind;
use panko_parser::ast::Arithmetic;
use panko_parser::ast::Signedness;
use panko_report::Report;
use panko_sema::layout::CompoundStatement;
use panko_sema::layout::Declaration;
use panko_sema::layout::Expression;
use panko_sema::layout::ExternalDeclaration;
use panko_sema::layout::FunctionDefinition;
use panko_sema::layout::Initialiser;
use panko_sema::layout::Layout;
use panko_sema::layout::LayoutedExpression;
use panko_sema::layout::Reference;
use panko_sema::layout::Slot;
use panko_sema::layout::Statement;
use panko_sema::layout::Subobject;
use panko_sema::layout::SubobjectInitialiser;
use panko_sema::layout::TranslationUnit;
use panko_sema::layout::Type;
use panko_sema::scope::Linkage;
use panko_sema::scope::RefKind;
use panko_sema::ty::ArrayType;
use panko_sema::ty::Complete;
use panko_sema::ty::Struct;
use panko_sema::typecheck::ArrayLength;
use panko_sema::typecheck::Member;

use crate::Register::*;
use crate::lineno::Linenos;
use crate::operand::AsOperand;
use crate::operand::Index;
use crate::operand::Memory;
use crate::operand::Offset;
use crate::operand::Operand;

mod lineno;
mod operand;

const MAX_IMUL_IMMEDIATE: u64 = 2_u64.pow(31);
#[expect(
    clippy::as_conversions,
    reason = "conversion using `From` not allowed in `const`"
)]
const MAX_ADDRESS_OFFSET: u64 = u32::MAX as _;
const ARGUMENT_REGISTERS: [Register; 6] = [Rdi, Rsi, Rdx, Rcx, R8, R9];

struct ByValue<T>(T);

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
struct TypedRegister<'a, 'b> {
    ty: &'b Type<'a>,
    register: Register,
}

impl Register {
    const fn byte<'a, 'b>(self) -> TypedRegister<'a, 'b> {
        self.with_size(1)
    }

    fn with_ty<'a, 'b>(self, ty: &'b Type<'a>) -> TypedRegister<'a, 'b> {
        TypedRegister { ty, register: self }
    }

    fn typed<'a, 'b>(self, expr: &'b LayoutedExpression<'a>) -> TypedRegister<'a, 'b> {
        TypedRegister { ty: &expr.ty.ty, register: self }
    }

    const fn with_size<'a, 'b>(self, size: u64) -> TypedRegister<'a, 'b> {
        let ty = match size {
            1 => const { &Type::uchar() },
            2 => const { &Type::ushort() },
            4 => const { &Type::uint() },
            8 => const { &Type::size_t() },
            _ => unreachable!(),
        };
        TypedRegister { ty, register: self }
    }
}

impl Display for TypedRegister<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let size = match self.ty {
            Type::Arithmetic(_) | Type::Pointer(_) | Type::Nullptr => self.ty.size(),
            Type::Array(_) | Type::Function(_) | Type::Void => unreachable!(),
            Type::Typeof { expr, unqual: _ } => match *expr {},
            Type::Struct(Struct::Incomplete { name: _, id: _ }) => unreachable!("incomplete"),
            Type::Struct(Struct::Complete(Complete { name: _, id: _, members: _ })) =>
                todo!("register for complete struct (is this unreachable?)"),
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

#[derive(Debug, Clone, Copy)]
struct StaticId(u64);

impl Display for StaticId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "static.{}", self.0)
    }
}

#[derive(Debug, Clone, Copy)]
struct LabelId(u64);

impl Display for LabelId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, ".L.{}", self.0)
    }
}

struct SubobjectAtReference<'a> {
    reference: Reference<'a>,
    subobject: Subobject<'a>,
}

impl<'a> SubobjectAtReference<'a> {
    fn slot(&self) -> Slot<'a> {
        self.reference.slot().offset(self.subobject.offset())
    }
}

#[derive(Debug)]
enum StaticInitialiser<'a> {
    Braced {
        subobject_initialisers: Vec<SubobjectInitialiser<'a, &'a Expression<'a>>>,
    },
    Expression(&'a Expression<'a>),
}

impl<'a> From<&'a Initialiser<'a>> for StaticInitialiser<'a> {
    fn from(initialiser: &'a Initialiser<'a>) -> Self {
        match initialiser {
            Initialiser::Braced { subobject_initialisers } => Self::Braced {
                subobject_initialisers: subobject_initialisers
                    .iter()
                    .map(
                        |SubobjectInitialiser { subobject, initialiser }| SubobjectInitialiser {
                            subobject: *subobject,
                            initialiser: &initialiser.expr,
                        },
                    )
                    .collect(),
            },
            Initialiser::Expression(expr) => Self::Expression(&expr.expr),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum ShouldZero {
    No,
    Yes,
}

#[derive(Debug, Default)]
struct Codegen<'a> {
    deferred_definitions: IndexMap<&'a str, (Linkage, Type<'a>, Option<StaticInitialiser<'a>>)>,
    defined: IndexSet<&'a str>,
    current_function: Option<&'a FunctionDefinition<'a>>,
    code: String,
    // TODO: could use an `IndexSet` here to deduplicate
    strings: Vec<(StaticId, Cow<'a, ByteStr>)>,
    next_label_id: u64,
    debug: bool,
    linenos: Linenos<'a>,
    global_errors: Vec<&'a dyn Report>,
}

impl<'a> Codegen<'a> {
    fn block(&mut self, count: u64) {
        for _ in 0..count {
            self.code.push('\n');
        }
    }

    fn at(&mut self, loc: Loc<'a>) {
        if self.debug {
            let (file, line, column) = self.linenos.lookup(loc);
            self.directive_with_sep("loc", " ", &[&file, &line, &column]);
        }
    }

    fn directive(&mut self, name: &str, args: &[&dyn Display]) {
        self.directive_with_sep(name, ", ", args)
    }

    fn directive_with_sep(&mut self, name: &str, sep: &str, args: &[&dyn Display]) {
        // TODO: this leaves trailing spaces when the directive does not have args
        writeln!(
            self.code,
            ".{name:7}{}{}",
            if args.is_empty() { "" } else { " " },
            args.iter().format(sep),
        )
        .unwrap();
    }

    fn label(&mut self, name: impl Display) {
        writeln!(self.code, "{name}:").unwrap();
    }

    fn zero(&mut self, size: u64) {
        writeln!(self.code, "    .zero {size}").unwrap();
    }

    fn constant(&mut self, constant: &[u8]) {
        writeln!(self.code, "    .byte {}", constant.iter().format(", ")).unwrap();
    }

    fn string(&mut self, s: Cow<'a, ByteStr>) -> StaticId {
        let id = StaticId(self.strings.len().try_into().unwrap());
        self.strings.push((id, s));
        id
    }

    fn new_label(&mut self) -> LabelId {
        let id = self.next_label_id;
        self.next_label_id += 1;
        LabelId(id)
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

    fn copy<'b>(&mut self, tgt: &dyn AsOperand<'b>, src: &dyn AsOperand<'b>) {
        let tgt = tgt.as_operand(try { self.current_function?.argument_area_size });
        let src = src.as_operand(try { self.current_function?.argument_area_size });
        let ty = tgt.ty();
        assert_eq!(ty, src.ty());
        match ty {
            Type::Arithmetic(_) | Type::Pointer(_) | Type::Nullptr => {
                self.emit_args("mov", &[&Rax.with_ty(ty), &src]);
                self.emit_args("mov", &[&tgt, &Rax.with_ty(ty)]);
            }
            Type::Array(_) | Type::Function(_) | Type::Void => unreachable!(),
            Type::Typeof { expr, unqual: _ } => match *expr {},
            Type::Struct(Struct::Incomplete { name: _, id: _ }) => unreachable!("incomplete"),
            Type::Struct(Struct::Complete(Complete { name: _, id: _, members: _ })) =>
                self.memcpy(&tgt, &src, ty.size()),
        }
    }

    fn emit_pointer_offset(
        &mut self,
        operation: &str,
        pointee_size: u64,
        integral: &LayoutedExpression,
    ) {
        assert_eq!(integral.ty.ty, Type::size_t());
        if pointee_size < MAX_IMUL_IMMEDIATE {
            self.emit_args("imul", &[&R10, integral, &pointee_size]);
        }
        else {
            self.emit_args("movabs", &[&R10, &pointee_size]);
            self.emit_args("imul", &[&R10, integral]);
        }
        self.emit_args(operation, &[&Rax, &R10]);
    }

    fn emit_error_output(&mut self, error: &dyn Report) {
        let id = self.string(ByteString(error.to_string().into_bytes()).into());
        self.emit_args("lea", &[&Rdi, &id]);
        self.emit("mov rsi, qword ptr [rip + stderr@gotpcrel]");
        self.emit("mov rsi, [rsi]");
        self.emit("call fputs@plt");
    }

    fn function_definition(&mut self, def: &'a FunctionDefinition<'a>) {
        self.block(2);
        let visibility = match def.reference.linkage() {
            Linkage::External => "globl",
            Linkage::Internal => "local",
            Linkage::None => unreachable!("functions always have linkage"),
            Linkage::Inline => "local",
        };
        self.directive(visibility, &[&def.name()]);
        self.directive("text", &[]);
        self.directive("type", &[&def.name(), &"@function"]);
        self.label(def.name());

        // TODO: should use the `def`’s `loc`
        self.at(def.reference.ty.loc());

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
        match def.noreturn {
            Some(_) => {
                // TODO: include error message like in explicit `return`
                self.emit("ud2");
            }
            None => {
                self.emit_args("add", &[&Rsp, &def.stack_size]);
                if def.is_main() {
                    self.emit("xor eax, eax");
                }
                self.emit("ret");
            }
        }

        self.block(1);
        self.label(format!(".L.{}.entry.sp_unaligned", def.name()));
        self.emit("int3");
    }

    fn object_definition(
        &mut self,
        name: &str,
        linkage: Linkage,
        ty: Type,
        initialiser: Option<StaticInitialiser<'_>>,
    ) {
        let size = match ty {
            // TODO: assert that this only happens in tentative definitions
            // TODO: gcc and clang warn in this case
            Type::Array(ArrayType { ty, length: ArrayLength::Unknown, loc: _ }) => ty.ty.size(),
            _ => ty.size(),
        };

        let visibility = match linkage {
            Linkage::External => "globl",
            Linkage::Internal => "local",
            Linkage::None => "local",
            Linkage::Inline => unreachable!("only functions can be `inline`"),
        };
        self.block(2);
        self.directive(visibility, &[&name]);
        self.directive("data", &[]);
        self.directive("type", &[&name, &"@object"]);
        self.directive("size", &[&name, &size]);
        self.directive("align", &[&ty.align()]);
        self.label(name);
        match initialiser {
            Some(StaticInitialiser::Expression(initialiser)) => match initialiser {
                Expression::Error(_) => todo!("ICE?"),
                Expression::Name(_) => todo!(),
                Expression::Integer(value) =>
                    self.constant(&value.to_le_bytes()[..usize::try_from(size).unwrap()]),
                Expression::String(_string) => unreachable!(
                    "either contained in a `noop-type-conversion` or in an initialiser",
                ),
                Expression::Nullptr => self.zero(ty.size()),
                Expression::NoopTypeConversion(_) => todo!(),
                Expression::Truncate(_) => todo!(),
                Expression::SignExtend(_) => todo!(),
                Expression::ZeroExtend(_) => todo!(),
                Expression::VoidCast(_) => todo!(),
                Expression::BoolCast(_) => todo!(),
                Expression::Assign { .. } => todo!(),
                Expression::IntegralBinOp { .. } => todo!(),
                Expression::PtrAdd { .. } => todo!(),
                Expression::PtrSub { .. } => todo!(),
                Expression::PtrDiff { .. } => todo!(),
                Expression::PtrCmp { .. } => todo!(),
                Expression::Addressof(_) => todo!(),
                Expression::Deref(_) => todo!(),
                Expression::Call { .. } => todo!(),
                Expression::Negate(_) => todo!(),
                Expression::Compl(_) => todo!(),
                Expression::Not(_) => todo!(),
                Expression::Combine { .. } => todo!(),
                Expression::Logical { .. } => todo!(),
                Expression::Conditional { .. } => todo!(),
                Expression::MemberAccess { .. } => todo!(),
            },
            Some(StaticInitialiser::Braced { subobject_initialisers }) => {
                if subobject_initialisers.is_empty() {
                    self.zero(size);
                }
                else {
                    // TODO: this can be made a lot more efficient for sparse initialisers
                    let mut bytes = vec![0; usize::try_from(size).unwrap()];
                    for SubobjectInitialiser { subobject, initialiser } in subobject_initialisers {
                        let subobject_size = usize::try_from(subobject.ty().ty.size()).unwrap();
                        match initialiser {
                            Expression::Integer(value) => bytes
                                [usize::try_from(subobject.offset()).unwrap()..][..subobject_size]
                                .copy_from_slice(&value.to_le_bytes()[..subobject_size]),
                            _ => todo!(),
                        }
                    }
                    self.constant(&bytes);
                }
            }
            None => self.zero(size),
        }
    }

    fn string_literal_definition(&mut self, id: StaticId, value: &ByteStr) {
        // `.asciz` adds a null terminator, so we can optionally remove one here
        let value = value.strip_suffix(b"\0").unwrap_or(value);

        self.block(2);
        let name = format!(".L.{id}");
        self.directive("local", &[&name]);
        self.directive("data", &[]);
        self.directive("type", &[&name, &"@object"]);
        self.directive("size", &[&name, &(value.len() + 1)]);
        self.directive("align", &[&1]);
        self.label(&name);
        self.code.push_str(".asciz \"");
        for &byte in value {
            if byte.is_ascii_graphic() || byte == b' ' {
                write!(&mut self.code, "{}", char::from(byte).escape_default()).unwrap();
            }
            else {
                write!(&mut self.code, "\\{byte:03o}").unwrap();
            }
        }
        self.code.push_str("\"\n");
    }

    fn external_declaration(&mut self, decl: &'a Declaration<'a>) {
        let name = decl.reference.name();
        let linkage = decl.reference.linkage();
        let ty = decl.reference.ty.ty;
        match decl.reference.kind() {
            RefKind::Declaration => (),
            RefKind::TentativeDefinition =>
                if !self.defined.contains(name) {
                    self.deferred_definitions.insert(name, (linkage, ty, None));
                },
            RefKind::Definition => {
                self.deferred_definitions.shift_remove(name);
                self.defined.insert(name);
                let initialiser = try { StaticInitialiser::from(decl.initialiser.as_ref()?) };
                self.object_definition(name, linkage, ty, initialiser)
            }
        }
    }

    fn compound_statement(&mut self, stmts: CompoundStatement<'a>) {
        for stmt in stmts.0 {
            self.stmt(stmt);
        }
    }

    fn stmt(&mut self, stmt: &'a Statement<'a>) {
        match stmt {
            Statement::StructDecl(_) => {
                // struct decls don’t generate code (they don’t contain VMT refs)
            }
            Statement::Declaration(Declaration { reference, initialiser }) =>
                match reference.slot() {
                    Slot::Automatic(_) => self.initialise(reference, initialiser.as_ref()),
                    Slot::Static(name) if reference.ty.ty.is_object() => {
                        self.deferred_definitions.insert(
                            name,
                            (
                                reference.linkage(),
                                reference.ty.ty,
                                try { StaticInitialiser::from(initialiser.as_ref()?) },
                            ),
                        );
                    }
                    Slot::Static(_) => (),
                    Slot::Void | Slot::StaticWithOffset { .. } => unreachable!(),
                },
            Statement::Typedef(_) => {
                // TODO: emit the type in case of a VMT
            }
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

    fn memset_zero(&mut self, target: &dyn AsOperand) {
        self.emit("xor eax, eax");
        match target.size() {
            size @ (1 | 2 | 4 | 8) => {
                self.emit_args("mov", &[&ByValue(target), &Rax.with_size(size)]);
            }
            size => {
                self.emit_args("lea", &[&Rdi, target]);
                self.emit_args("movabs", &[&Rcx, &size]);
                self.emit("rep stosb");
            }
        }
    }

    fn memcpy<'b>(&mut self, target: &dyn AsOperand<'b>, src: &dyn AsOperand<'b>, byte_count: u64) {
        self.emit_args("lea", &[&Rdi, target]);
        self.emit_args("lea", &[&Rsi, src]);
        self.emit_args("movabs", &[&Rcx, &byte_count]);
        self.emit("rep movsb");
    }

    fn initialise(&mut self, reference: &Reference<'a>, initialiser: Option<&Initialiser<'a>>) {
        match initialiser {
            Some(Initialiser::Expression(LayoutedExpression {
                expr: Expression::String(string),
                ..
            })) if reference.ty.ty.is_array() => {
                self.initialise_with_string(reference, ShouldZero::Yes, string);
            }
            Some(Initialiser::Expression(initialiser)) => {
                self.expr(initialiser);
                if reference.slot() != initialiser.slot {
                    self.copy(reference, initialiser);
                }
            }
            Some(Initialiser::Braced { subobject_initialisers }) => {
                self.memset_zero(reference);

                subobject_initialisers.iter().for_each(
                    |SubobjectInitialiser { subobject, initialiser }| {
                        let target = SubobjectAtReference {
                            reference: *reference,
                            subobject: *subobject,
                        };

                        if let Expression::String(string) = initialiser.expr {
                            self.initialise_with_string(&target, ShouldZero::No, string);
                        }
                        else {
                            self.expr(initialiser);

                            if target.slot() != initialiser.slot {
                                self.copy(&target, initialiser);
                            }
                        }
                    },
                );
            }
            None => (),
        }
    }

    fn initialise_with_string(
        &mut self,
        target: &dyn AsOperand,
        should_zero: ShouldZero,
        string: &'a ByteStr,
    ) {
        let target_size = target.size();
        let string_len = u64::try_from(string.len()).unwrap();
        let id = self.string(Cow::Borrowed(string));
        if let ShouldZero::Yes = should_zero
            && string_len < target_size
        {
            self.memset_zero(target);
        }
        self.memcpy(target, &id, target_size.min(string_len));
    }

    fn expr(&mut self, expr: &LayoutedExpression<'a>) {
        self.at(expr.loc);
        match expr.expr {
            Expression::Error(error) => {
                self.emit_error_output(error);
                self.emit("ud2");
            }
            Expression::Name(_reference) => (), // already in memory
            Expression::Integer(value) => {
                self.emit_args("movabs", &[&Rax, &value]);
                self.emit_args("mov", &[expr, &Rax.typed(expr)]);
            }
            Expression::String(_string) => unreachable!(
                "strings are either used as array initialisers or as operands to addressof operators, but never separately",
            ),
            Expression::Nullptr => {
                self.emit("xor eax, eax");
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
            Expression::BoolCast(inner) => {
                self.expr(inner);
                self.emit_args("cmp", &[inner, &0]);
                self.emit("setne al");
                self.emit_args("mov", &[expr, &Rax.typed(expr)]);
            }
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
                    if expr.slot != value.slot {
                        self.copy(expr, value);
                    }
                }
                Expression::MemberAccess { lhs, member } => {
                    let member = self.member_access_lvalue(lhs, member);
                    self.expr(value);
                    let member = self.member_pointer(member);
                    self.copy(&*member, value);
                    if expr.slot != value.slot {
                        self.copy(expr, value);
                    }
                }
                _ => unreachable!(),
            },
            Expression::IntegralBinOp { ty, lhs, op, rhs } => {
                if !matches!(op.kind, BinOpKind::LeftShift | BinOpKind::RightShift) {
                    // TODO: at this point, `lhs.ty` and `rhs.ty` should both be unqualified due to
                    // lvalue conversion. However, we don’t represent lvalue conversions in the
                    // Ast. This should probably be fixed at some point.
                    assert_eq!(lhs.ty.ty, rhs.ty.ty);
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

                self.at(lhs.loc);
                self.emit_args("mov", &[&Rax.typed(lhs), lhs]);

                self.at(op.token.loc());
                match op.kind {
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
                        if matches!(op.kind, BinOpKind::Modulo) {
                            match ty.size() {
                                1 => self.emit("mov al, ah"),
                                2 | 4 | 8 => self.emit_args("mov", &[&Rax, &Rdx]),
                                _ => unreachable!(),
                            }
                        }
                    }
                    BinOpKind::Add => emit_arithmetic(self, "add"),
                    BinOpKind::Subtract => emit_arithmetic(self, "sub"),
                    BinOpKind::Comparison(cmp) => match cmp {
                        Comparison::Equal => emit_comparison(self, "sete"),
                        Comparison::NotEqual => emit_comparison(self, "setne"),
                        Comparison::Less => emit_sign_dependent_comparison(self, "setl", "setb"),
                        Comparison::LessEqual =>
                            emit_sign_dependent_comparison(self, "setle", "setbe"),
                        Comparison::Greater => emit_sign_dependent_comparison(self, "setg", "seta"),
                        Comparison::GreaterEqual =>
                            emit_sign_dependent_comparison(self, "setge", "setae"),
                    },
                    BinOpKind::LeftShift => emit_shift(self, "shl", "shl"),
                    BinOpKind::RightShift => emit_shift(self, "sar", "shr"),
                    BinOpKind::BitAnd => emit_arithmetic(self, "and"),
                    BinOpKind::BitXor => emit_arithmetic(self, "xor"),
                    BinOpKind::BitOr => emit_arithmetic(self, "or"),
                };

                self.emit_args("mov", &[expr, &Rax.typed(expr)]);
            }
            Expression::PtrAdd { pointer, integral, pointee_size, order } => {
                assert_eq!(integral.ty.ty, Type::size_t());
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
            Expression::PtrDiff { lhs, rhs, pointee_size } => {
                self.expr(lhs);
                self.expr(rhs);
                self.emit_args("mov", &[&Rax.typed(lhs), lhs]);
                self.emit_args("sub", &[&Rax.typed(lhs), rhs]);
                self.emit("cqo");
                self.emit_args("movabs", &[&Rcx, &pointee_size]);
                self.emit_args("idiv", &[&Rax, &Rcx]);
                self.emit_args("mov", &[expr, &Rax.typed(expr)]);
            }
            Expression::PtrCmp { lhs, kind, rhs } => {
                self.expr(lhs);
                self.expr(rhs);
                self.emit_args("mov", &[&Rax.typed(lhs), lhs]);
                self.emit_args("cmp", &[&Rax.typed(lhs), rhs]);
                let operation = match kind {
                    Comparison::Equal => "sete",
                    Comparison::NotEqual => "setne",
                    Comparison::Less => "setb",
                    Comparison::LessEqual => "setbe",
                    Comparison::Greater => "seta",
                    Comparison::GreaterEqual => "setae",
                };
                self.emit_args(operation, &[&Rax.byte()]);
                self.emit_args("movzx", &[&Rax.typed(expr), &Rax.byte()]);
                self.emit_args("mov", &[expr, &Rax.typed(expr)]);
            }
            Expression::Addressof(operand) => {
                match operand.expr {
                    Expression::Name(_reference) => {
                        // `_reference` is already in memory, just load the address
                        self.emit_args("lea", &[&Rax, operand]);
                    }
                    Expression::Deref(operand) => {
                        self.expr(operand);
                        self.emit_args("mov", &[&Rax.typed(operand), operand]);
                    }
                    Expression::String(string) => {
                        let id = self.string(Cow::Borrowed(string));
                        self.emit_args("lea", &[&Rax, &id]);
                    }
                    Expression::MemberAccess { lhs, member } => {
                        let member = self.member_access_lvalue(lhs, member);
                        let member = self.member_pointer(member);
                        self.emit_args("lea", &[&Rax, &*member]);
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
            Expression::Combine { first, second } => {
                self.expr(first);
                self.expr(second);
                assert_eq!(expr.slot, second.slot);
            }
            Expression::Logical { lhs, op, rhs } => {
                self.expr(lhs);
                self.emit_args("cmp", &[lhs, &0]);
                let label = self.new_label();
                let operation = match op.kind() {
                    LogicalOpKind::And => "je",
                    LogicalOpKind::Or => "jne",
                };
                self.emit_args(operation, &[&label]);
                self.expr(rhs);
                self.emit_args("cmp", &[rhs, &0]);
                self.label(label);
                self.emit("setne al");
                self.emit("movzx eax, al");
                self.emit_args("mov", &[expr, &Rax.typed(expr)]);
            }
            Expression::Conditional { condition, then, or_else } => {
                self.expr(condition);
                self.emit_args("cmp", &[condition, &0]);
                let condition_false = self.new_label();
                self.emit_args("je", &[&condition_false]);
                self.expr(then);
                if then.slot != expr.slot {
                    self.copy(expr, then);
                }
                let merge = self.new_label();
                self.emit_args("jmp", &[&merge]);
                self.label(condition_false);
                self.expr(or_else);
                if or_else.slot != expr.slot {
                    self.copy(expr, or_else);
                }
                self.label(merge);
            }
            Expression::MemberAccess { lhs, member: _ } => {
                self.expr(lhs);
            }
        }
    }

    fn member_access_lvalue(
        &mut self,
        lhs: &'a LayoutedExpression<'a>,
        member: Member<'a, Layout>,
    ) -> MemberPointer<'a> {
        let ty = lhs.ty.ty;
        let pointer = match lhs.expr {
            Expression::MemberAccess { lhs, member } => self.member_access_lvalue(lhs, member),
            Expression::Deref(operand) => {
                self.expr(operand);
                MemberPointer::Pointer { pointer: operand, offset: 0, ty }
            }
            _ => {
                self.expr(lhs);
                MemberPointer::Stack(PointerIntoStackWithOffset { object: lhs, offset: 0, ty })
            }
        };
        pointer.member(member)
    }

    fn member_pointer(&mut self, member: MemberPointer<'a>) -> Box<dyn AsOperand<'a> + 'a> {
        match member {
            MemberPointer::Stack(pointer) => Box::new(pointer),
            MemberPointer::Pointer { pointer, offset, ty } => {
                self.emit_args("mov", &[&R10, pointer]);
                self.emit_args("add", &[&R10, &offset]);
                Box::new(Operand::lvalue(R10, ty))
            }
        }
    }
}

struct PointerIntoStackWithOffset<'a> {
    object: &'a LayoutedExpression<'a>,
    offset: u64,
    ty: Type<'a>,
}

enum MemberPointer<'a> {
    Stack(PointerIntoStackWithOffset<'a>),
    Pointer {
        pointer: &'a LayoutedExpression<'a>,
        offset: u64,
        ty: Type<'a>,
    },
}

impl<'a> MemberPointer<'a> {
    fn member(&self, member: Member<'a, Layout>) -> Self {
        let Member { ty, offset: additional_offset, .. } = member;
        let ty = ty.ty;
        match *self {
            Self::Stack(PointerIntoStackWithOffset { object, offset, ty: _ }) =>
                Self::Stack(PointerIntoStackWithOffset {
                    object,
                    offset: offset.strict_add(additional_offset),
                    ty,
                }),
            Self::Pointer { pointer, offset, ty: _ } => Self::Pointer {
                pointer,
                offset: offset.strict_add(additional_offset),
                ty,
            },
        }
    }
}

pub fn emit(translation_unit: TranslationUnit, with_debug_info: bool) -> (String, String) {
    let mut cg = Codegen {
        debug: with_debug_info,
        ..Codegen::default()
    };

    for decl in translation_unit.decls {
        match decl {
            ExternalDeclaration::StructDecl(_) => {
                // struct decls don’t generate code (they don’t contain VMT refs)
            }
            ExternalDeclaration::FunctionDefinition(def) => cg.function_definition(def),
            ExternalDeclaration::Declaration(decl) => cg.external_declaration(decl),
            ExternalDeclaration::Typedef(_) => {
                // TODO: check that the type is not a VMT
            }
            ExternalDeclaration::ProvideExternalDefinitionForInlineFunction(name) => {
                cg.block(2);
                cg.directive("globl", &[name]);
            }
            ExternalDeclaration::Error(error) => cg.global_errors.push(*error),
        }
    }

    if !cg.global_errors.is_empty() {
        let name = "__panko_emit_global_errors";

        cg.block(2);
        cg.directive("globl", &[&name]);
        cg.directive("text", &[]);
        cg.directive("type", &[&name, &"@function"]);
        cg.label(name);
        for error in mem::take(&mut cg.global_errors) {
            cg.emit_error_output(error);
        }
        cg.emit("int3");

        cg.block(2);
        cg.directive("section", &[&".init_array", &r#""aw""#]);
        cg.directive("align", &[&8]);
        cg.directive("quad", &[&name]);
    }

    for (name, (linkage, ty, initialiser)) in mem::take(&mut cg.deferred_definitions) {
        assert!(!cg.defined.contains(&name));
        cg.object_definition(name, linkage, ty, initialiser);
    }

    for (id, value) in mem::take(&mut cg.strings) {
        cg.string_literal_definition(id, &value);
    }

    let code = mem::take(&mut cg.code);

    cg.directive("intel_syntax", &[&"noprefix"]);
    for (fileno, path) in &mem::take(&mut cg.linenos) {
        // TODO: properly escape `path`
        cg.directive_with_sep("file", " ", &[&fileno, &format!("\"{}\"", path.display())]);
    }

    (cg.code, code)
}
