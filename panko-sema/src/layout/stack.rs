use std::collections::hash_map::Entry;
use std::collections::HashMap;

use super::LayoutedExpression;
use super::Reference;
use super::Slot;
use crate::nonempty;
use crate::scope;
use crate::scope::Id;
use crate::scope::StorageDuration;
use crate::ty::Type;

#[derive(Debug, Default)]
struct Slots {
    stack: nonempty::Vec<u64>,
    offset: u64,
    size: u64,
}

impl Slots {
    fn add_slot<'a>(&mut self, ty: Type<'a>) -> Slot<'a> {
        if matches!(ty, Type::Void) {
            return Slot::Void;
        }
        self.offset = self.offset.next_multiple_of(ty.align());
        let slot = Slot::Automatic(self.offset);
        self.offset += ty.size();
        self.size = self.size.max(self.offset);
        slot
    }

    fn push(&mut self) {
        self.stack.push(self.offset);
    }

    fn pop(&mut self) {
        self.offset = self.stack.pop().unwrap();
    }
}

#[derive(Debug, Default)]
pub(super) struct Stack<'a> {
    ids: HashMap<Id, Reference<'a>>,
    slots: Slots,
    argument_area_size: u64,
}

impl<'a> Stack<'a> {
    pub(super) fn with_block<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.push();
        let result = f(self);
        self.pop();
        result
    }

    pub(super) fn size(self) -> u64 {
        self.slots.size
    }

    pub(super) fn push(&mut self) {
        self.slots.push()
    }

    pub(super) fn pop(&mut self) {
        self.slots.pop()
    }

    pub(super) fn add(&mut self, reference: scope::Reference<'a>) -> Reference<'a> {
        self.add_at(reference, None)
    }

    pub(super) fn add_at(
        &mut self,
        reference: scope::Reference<'a>,
        maybe_slot: Option<Slot<'a>>,
    ) -> Reference<'a> {
        match self.ids.entry(reference.id) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let scope::Reference {
                    name,
                    loc: _,
                    ty,
                    id,
                    usage_location: _,
                    kind,
                    storage_duration,
                } = reference;
                let slot = match storage_duration {
                    StorageDuration::Static => Slot::Static(reference.name()),
                    StorageDuration::Automatic =>
                        maybe_slot.unwrap_or_else(|| self.slots.add_slot(ty.ty)),
                };
                *entry.insert(Reference { name, ty, id, kind, slot })
            }
        }
    }

    pub(super) fn temporary(&mut self, ty: Type<'a>) -> Slot<'a> {
        self.slots.add_slot(ty)
    }

    pub(super) fn function_arguments(&mut self, args: &[LayoutedExpression<'a>]) {
        let argument_area_size = args
            .iter()
            // TODO: this is `panko_codegen::ARGUMENT_REGISTERS.len()`
            .skip(6)
            .map(|arg| arg.ty.ty.size().next_multiple_of(8))
            .sum();
        self.argument_area_size = self.argument_area_size.max(argument_area_size);
    }

    pub(super) fn argument_area_size(&self) -> u64 {
        self.argument_area_size
    }
}
