use std::assert_matches::assert_matches;
use std::fmt;

use panko_sema::layout::LayoutedExpression;
use panko_sema::layout::Reference;
use panko_sema::layout::Slot;
use panko_sema::layout::Type;

use crate::ByValue;
use crate::LabelId;
use crate::MAX_ADDRESS_OFFSET;
use crate::Register;
use crate::StaticId;
use crate::SubobjectAtReference;
use crate::TypedRegister;

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
        assert_matches!(
            index,
            None | Some(Index { register: _, size: 1 | 2 | 4 | 8 }),
        );
        assert_matches!(
            offset,
            Offset::Immediate(0..=MAX_ADDRESS_OFFSET) | Offset::Plt(_) | Offset::Static(_),
        );
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
    Pointer {
        address: Memory<'a>,
        // TODO: this feels weird, find a better representation. Implementing `AsOperand` for
        // `ByValue<&dyn AsOperand>` makes it even more iffy.
        is_dereferenced: bool,
    },
    Immediate(u64),
    Label(LabelId),
}

#[derive(Debug, Clone, Copy)]
pub(super) struct Operand<'a> {
    kind: OperandKind<'a>,
    ty: Type<'a>,
}

impl<'a> Operand<'a> {
    fn pointer_into_stack(ty: Type<'a>, offset: u64) -> Self {
        Self {
            kind: OperandKind::Pointer {
                address: Memory {
                    pointer: Register::Rsp,
                    index: None,
                    offset: Offset::Immediate(offset),
                },
                is_dereferenced: false,
            },
            ty,
        }
    }

    pub(super) fn pointer(register: Register, expr: &LayoutedExpression<'a>) -> Self {
        let Type::Pointer(ty) = expr.ty.ty
        else {
            unreachable!()
        };
        Self {
            kind: OperandKind::Pointer {
                address: Memory {
                    pointer: register,
                    index: None,
                    offset: Offset::Immediate(0),
                },
                is_dereferenced: false,
            },
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
            OperandKind::Pointer { address: memory, is_dereferenced } => {
                let size = match self.ty {
                    // scalars are always dereferenced
                    Type::Arithmetic(_) | Type::Pointer(_) | Type::Nullptr => self.ty.size(),
                    // Using a function results in a pointer to that function, so we need 8 bytes.
                    Type::Function(_) =>
                        if is_dereferenced {
                            unreachable!()
                        }
                        else {
                            8
                        },
                    // Same for arrays.
                    Type::Array(_) =>
                        if is_dereferenced {
                            self.ty.size()
                        }
                        else {
                            8
                        },
                    Type::Void => unreachable!(),
                    Type::Typeof { expr, unqual: _ } => match expr {},
                };
                let ptr_type = match size {
                    1 => "byte",
                    2 => "word",
                    4 => "dword",
                    8 => "qword",
                    _ => unreachable!("invalid pointee size {size}"),
                };
                write!(f, "{ptr_type} ptr {memory}")
            }
            OperandKind::Immediate(imm) => write!(f, "{imm}"),
            OperandKind::Label(id) => write!(f, "{id}"),
        }
    }
}

fn slot_as_operand<'a>(
    slot: Slot<'a>,
    ty: Type<'a>,
    argument_area_size: u64,
    is_dereferenced: bool,
) -> Operand<'a> {
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
        Slot::StaticWithOffset { name: _, offset: _ } => todo!(),
    };
    Operand {
        kind: OperandKind::Pointer {
            address: Memory { pointer, index: None, offset },
            is_dereferenced,
        },
        ty,
    }
}

pub(super) trait AsOperand {
    fn as_operand(&self, argument_area_size: Option<u64>) -> Operand;

    fn size(&self) -> u64 {
        self.as_operand(None).ty.size()
    }
}

impl AsOperand for Operand<'_> {
    fn as_operand(&self, _argument_area_size: Option<u64>) -> Operand {
        *self
    }
}

impl AsOperand for &Operand<'_> {
    fn as_operand(&self, _argument_area_size: Option<u64>) -> Operand {
        **self
    }
}

impl AsOperand for LayoutedExpression<'_> {
    fn as_operand(&self, argument_area_size: Option<u64>) -> Operand {
        slot_as_operand(self.slot, self.ty.ty, argument_area_size.unwrap(), false)
    }

    fn size(&self) -> u64 {
        self.ty.ty.size()
    }
}

impl AsOperand for Reference<'_> {
    fn as_operand(&self, argument_area_size: Option<u64>) -> Operand {
        slot_as_operand(self.slot(), self.ty.ty, argument_area_size.unwrap(), false)
    }

    fn size(&self) -> u64 {
        self.ty.ty.size()
    }
}

impl AsOperand for ByValue<&Reference<'_>> {
    fn as_operand(&self, argument_area_size: Option<u64>) -> Operand {
        slot_as_operand(
            self.0.slot(),
            self.0.ty.ty,
            argument_area_size.unwrap(),
            true,
        )
    }

    fn size(&self) -> u64 {
        self.0.ty.ty.size()
    }
}

impl AsOperand for ByValue<&dyn AsOperand> {
    fn as_operand(&self, argument_area_size: Option<u64>) -> Operand {
        let Operand { kind, ty } = self.0.as_operand(argument_area_size);

        let kind = match kind {
            OperandKind::Pointer { address, is_dereferenced: _ } =>
                OperandKind::Pointer { address, is_dereferenced: true },
            kind => kind,
        };

        Operand { kind, ty }
    }

    fn size(&self) -> u64 {
        self.0.size()
    }
}

impl AsOperand for SubobjectAtReference<'_> {
    fn as_operand(&self, argument_area_size: Option<u64>) -> Operand {
        slot_as_operand(
            self.slot(),
            self.subobject.ty().ty,
            argument_area_size.unwrap(),
            false,
        )
    }

    fn size(&self) -> u64 {
        self.subobject.ty().ty.size()
    }
}

impl AsOperand for TypedRegister<'_> {
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
            ty: Type::size_t(),
        }
    }
}

impl AsOperand for u64 {
    fn as_operand(&self, _argument_area_size: Option<u64>) -> Operand {
        Operand {
            kind: OperandKind::Immediate(*self),
            ty: Type::size_t(),
        }
    }
}

impl AsOperand for Memory<'_> {
    fn as_operand(&self, _argument_area_size: Option<u64>) -> Operand {
        assert!(
            !matches!(self.pointer, Register::Rsp),
            "this would need an offset adjustment of `argument_area_size`",
        );
        Operand {
            kind: OperandKind::Pointer { address: *self, is_dereferenced: false },
            ty: Type::size_t(),
        }
    }
}

impl AsOperand for StaticId {
    fn as_operand(&self, _argument_area_size: Option<u64>) -> Operand {
        Operand {
            kind: OperandKind::Pointer {
                address: Memory {
                    pointer: Register::Rip,
                    index: None,
                    offset: Offset::Static(*self),
                },
                is_dereferenced: false,
            },
            ty: Type::size_t(),
        }
    }
}

impl AsOperand for LabelId {
    fn as_operand(&self, _argument_area_size: Option<u64>) -> Operand {
        Operand {
            kind: OperandKind::Label(*self),
            ty: Type::Void,
        }
    }
}
