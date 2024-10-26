use std::fmt;

use panko_sema::layout::LayoutedExpression;
use panko_sema::layout::Reference;
use panko_sema::layout::Slot;
use panko_sema::ty::Type;

use crate::Register;
use crate::StaticId;
use crate::TypedRegister;
use crate::MAX_ADDRESS_OFFSET;

#[derive(Debug, Clone, Copy)]
pub(super) enum Offset<'a> {
    Immediate(u64),
    Plt(&'a str),
    Static(StaticId),
}

impl fmt::Display for Offset<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Offset::Immediate(immediate) => write!(f, "{immediate}"),
            Offset::Plt(name) => write!(f, "{name}@plt"),
            Offset::Static(id) => write!(f, ".L.{id}"),
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
            Offset::Immediate(0..=MAX_ADDRESS_OFFSET) | Offset::Plt(_) | Offset::Static(_),
        ));
        write!(f, "[{pointer}")?;
        if let Some(Index { register: reg, size }) = index {
            write!(f, " + {size} * {reg}")?;
        }
        match offset {
            Offset::Immediate(0) => (),
            offset => write!(f, " + {offset}")?,
        }
        write!(f, "]")
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
    fn pointer_into_stack(ty: Type<'a>, offset: u64) -> Self {
        Self {
            kind: OperandKind::Pointer(Memory {
                pointer: Register::Rsp,
                index: None,
                offset: Offset::Immediate(offset),
            }),
            ty,
        }
    }

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

    pub(super) fn stack_parameter_eightbyte(ty: Type<'a>, index: usize, stack_size: u64) -> Self {
        assert!(
            ty.size() <= 8,
            "TODO: handle parameters that are not class INTEGER",
        );
        let offset =
            // skip the callee’s stack
            stack_size
            // skip return address
            + 8
            // each eightbyte is eight bytes large
            + u64::try_from(index).unwrap() * 8;
        Self::pointer_into_stack(ty, offset)
    }

    pub(super) fn stack_argument_eightbyte(ty: Type<'a>, index: usize) -> Self {
        assert!(
            ty.size() <= 8,
            "TODO: handle arguments that are not class INTEGER",
        );
        Self::pointer_into_stack(ty, u64::try_from(index).unwrap() * 8)
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

fn slot_as_operand<'a>(slot: Slot<'a>, ty: Type<'a>, argument_area_size: u64) -> Operand<'a> {
    let (pointer, offset) = match slot {
        Slot::Static(name) => (Register::Rip, Offset::Plt(name)),
        Slot::Automatic(offset) => {
            // TODO: For types with alignment > 8, we also need to take the stack pointer into
            // account.
            assert!(offset.is_multiple_of(ty.align()));
            (
                Register::Rsp,
                Offset::Immediate(argument_area_size + offset),
            )
        }
        Slot::Void => unreachable!(),
    };
    Operand {
        kind: OperandKind::Pointer(Memory { pointer, index: None, offset }),
        ty,
    }
}

pub(super) trait AsOperand {
    fn as_operand(&self, argument_area_size: Option<u64>) -> Operand;
}

impl<'a> AsOperand for Operand<'a> {
    fn as_operand(&self, _argument_area_size: Option<u64>) -> Operand {
        *self
    }
}

impl<'a> AsOperand for &Operand<'a> {
    fn as_operand(&self, _argument_area_size: Option<u64>) -> Operand {
        **self
    }
}

impl<'a> AsOperand for LayoutedExpression<'a> {
    fn as_operand(&self, argument_area_size: Option<u64>) -> Operand {
        slot_as_operand(self.slot, self.ty.ty, argument_area_size.unwrap())
    }
}

impl<'a> AsOperand for Reference<'a> {
    fn as_operand(&self, argument_area_size: Option<u64>) -> Operand {
        slot_as_operand(self.slot(), self.ty.ty, argument_area_size.unwrap())
    }
}

impl<'a> AsOperand for TypedRegister<'a> {
    fn as_operand(&self, _argument_area_size: Option<u64>) -> Operand {
        let Self { register, ty } = *self;
        Operand {
            kind: OperandKind::Register(register),
            ty: *ty,
        }
    }
}

impl AsOperand for Register {
    fn as_operand(&self, _argument_area_size: Option<u64>) -> Operand {
        Operand {
            kind: OperandKind::Register(*self),
            ty: Type::ullong(),
        }
    }
}

impl AsOperand for u64 {
    fn as_operand(&self, _argument_area_size: Option<u64>) -> Operand {
        Operand {
            kind: OperandKind::Immediate(*self),
            ty: Type::ullong(),
        }
    }
}

impl<'a> AsOperand for Memory<'a> {
    fn as_operand(&self, _argument_area_size: Option<u64>) -> Operand {
        Operand {
            kind: OperandKind::Pointer(*self),
            ty: Type::ullong(),
        }
    }
}

impl AsOperand for StaticId {
    fn as_operand(&self, _argument_area_size: Option<u64>) -> Operand {
        Operand {
            kind: OperandKind::Pointer(Memory {
                pointer: Register::Rip,
                index: None,
                offset: Offset::Static(*self),
            }),
            ty: Type::ulong(),
        }
    }
}
