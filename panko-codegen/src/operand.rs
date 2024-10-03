use std::fmt;

use panko_sema::layout::LayoutedExpression;
use panko_sema::layout::Reference;
use panko_sema::layout::Slot;
use panko_sema::ty::Type;

use crate::Register;
use crate::Register::*;
use crate::TypedRegister;
use crate::MAX_ADDRESS_OFFSET;

#[derive(Debug, Clone, Copy)]
pub(super) enum Offset<'a> {
    Immediate(u64),
    Plt(&'a str),
}

impl fmt::Display for Offset<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Offset::Immediate(immediate) => write!(f, "{immediate}"),
            Offset::Plt(name) => write!(f, "{name}@plt"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) struct Index {
    pub(super) register: Register,
    pub(super) size: u64,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct Memory<'a> {
    pub(super) pointer: Register,
    pub(super) index: Option<Index>,
    pub(super) offset: Offset<'a>,
}

impl fmt::Display for Memory<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { pointer, index, offset } = self;
        assert!(matches!(
            index,
            None | Some(Index { register: _, size: 1 | 2 | 4 | 8 }),
        ));
        assert!(matches!(
            offset,
            Offset::Immediate(0..=MAX_ADDRESS_OFFSET) | Offset::Plt(_),
        ));
        write!(f, "[{pointer} + ")?;
        if let Some(Index { register: reg, size }) = index {
            write!(f, "{size} * {reg} + ")?;
        }
        write!(f, "{offset}]")
    }
}

#[derive(Debug, Clone, Copy)]
enum OperandKind<'a> {
    Register(Register),
    Pointer(Memory<'a>),
    Immediate(u64),
}

#[derive(Debug, Clone, Copy)]
pub(super) struct Operand<'a> {
    kind: OperandKind<'a>,
    ty: Type<'a>,
}

impl<'a> Operand<'a> {
    pub(super) fn pointer(register: Register, expr: &LayoutedExpression<'a>) -> Self {
        let Type::Pointer(ty) = expr.ty.ty
        else {
            unreachable!()
        };
        Self {
            kind: OperandKind::Pointer(Memory {
                pointer: register,
                index: None,
                offset: Offset::Immediate(0),
            }),
            ty: ty.ty,
        }
    }

    pub(super) fn ty(&self) -> &Type<'a> {
        &self.ty
    }
}

impl fmt::Display for Operand<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            OperandKind::Register(register) => write!(f, "{}", register.with_ty(self.ty())),
            OperandKind::Pointer(memory) => {
                let size = match self.ty {
                    Type::Arithmetic(_) | Type::Pointer(_) => self.ty.size(),
                    // Using a function results in a pointer to that function, so we need 8 bytes.
                    Type::Function(_) => 8,
                    Type::Void => unreachable!(),
                };
                let ptr_type = match size {
                    1 => "byte",
                    2 => "word",
                    4 => "dword",
                    8 => "qword",
                    _ => unreachable!(),
                };
                write!(f, "{ptr_type} ptr {memory}")
            }
            OperandKind::Immediate(imm) => write!(f, "{imm}"),
        }
    }
}

fn slot_as_operand<'a>(slot: Slot<'a>, ty: Type<'a>) -> Operand<'a> {
    let (pointer, offset) = match slot {
        Slot::Static(name) => (Rip, Offset::Plt(name)),
        Slot::Automatic(offset) => {
            // TODO: For types with alignment > 8, we also need to take the stack pointer into
            // account.
            assert!(offset.is_multiple_of(ty.align()));
            (Rsp, Offset::Immediate(offset))
        }
        Slot::Pointer { register: _ } => todo!(),
        Slot::Void => unreachable!(),
    };
    Operand {
        kind: OperandKind::Pointer(Memory { pointer, index: None, offset }),
        ty,
    }
}

pub(super) trait AsOperand {
    fn as_operand(&self) -> Operand;
}

impl<'a> AsOperand for Operand<'a> {
    fn as_operand(&self) -> Operand {
        *self
    }
}

impl<'a> AsOperand for &Operand<'a> {
    fn as_operand(&self) -> Operand {
        **self
    }
}

impl<'a> AsOperand for LayoutedExpression<'a> {
    fn as_operand(&self) -> Operand {
        slot_as_operand(self.slot, self.ty.ty)
    }
}

impl<'a> AsOperand for Reference<'a> {
    fn as_operand(&self) -> Operand {
        slot_as_operand(self.slot(), self.ty.ty)
    }
}

impl<'a> AsOperand for TypedRegister<'a> {
    fn as_operand(&self) -> Operand {
        let Self { register, ty } = *self;
        Operand {
            kind: OperandKind::Register(register),
            ty: *ty,
        }
    }
}

impl AsOperand for Register {
    fn as_operand(&self) -> Operand {
        Operand {
            kind: OperandKind::Register(*self),
            ty: Type::ullong(),
        }
    }
}

impl AsOperand for u64 {
    fn as_operand(&self) -> Operand {
        Operand {
            kind: OperandKind::Immediate(*self),
            ty: Type::ullong(),
        }
    }
}

impl<'a> AsOperand for Memory<'a> {
    fn as_operand(&self) -> Operand {
        Operand {
            kind: OperandKind::Pointer(*self),
            ty: Type::ullong(),
        }
    }
}
