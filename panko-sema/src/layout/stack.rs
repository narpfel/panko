use std::collections::hash_map::Entry;
use std::collections::HashMap;

use im_rc::HashSet;
use panko_parser::ast::Type;

use super::Reference;
use super::Slot;
use crate::nonempty;
use crate::scope;
use crate::scope::Id;
use crate::scope::StorageDuration;

#[derive(Debug, Default)]
struct Slots {
    stack: nonempty::Vec<(u64, HashSet<u64>)>,
    offset: u64,
    size: u64,
}

impl Slots {
    fn add_slot<'a>(&mut self, ty: Type<'a>) -> Slot<'a> {
        self.offset = self.offset.next_multiple_of(ty.align());
        let slot = Slot::Automatic(self.offset);
        self.offset += ty.size();
        self.size = self.size.max(self.offset);
        slot
    }

    fn push(&mut self) {
        self.stack.push((self.offset, self.stack.last().1.clone()));
    }

    fn pop(&mut self) {
        self.offset = self.stack.pop().unwrap().0;
    }

    fn make_permanent(&mut self, offset: u64) {
        self.stack.last_mut().1.insert(offset);
    }

    fn is_permanent(&self, offset: u64) -> bool {
        self.stack.last().1.contains(&offset)
    }
}

#[derive(Debug, Default)]
pub(super) struct Stack<'a> {
    ids: HashMap<Id, Reference<'a>>,
    slots: Slots,
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
                    ty,
                    id,
                    usage_location: _,
                    kind,
                    storage_duration,
                } = reference;
                let slot = match storage_duration {
                    StorageDuration::Static => Slot::Static(reference.name()),
                    // TODO: This can be expressed more concisely using guard patterns
                    StorageDuration::Automatic => match maybe_slot {
                        Some(Slot::Automatic(offset)) if self.slots.is_permanent(offset) =>
                            self.slots.add_slot(ty.ty),
                        maybe_slot => maybe_slot.unwrap_or_else(|| self.slots.add_slot(ty.ty)),
                    },
                };
                if let Slot::Automatic(offset) = slot {
                    self.slots.make_permanent(offset);
                }
                *entry.insert(Reference { name, ty, id, kind, slot })
            }
        }
    }

    pub(super) fn temporary(&mut self, ty: Type<'a>) -> Slot<'a> {
        self.slots.add_slot(ty)
    }
}
