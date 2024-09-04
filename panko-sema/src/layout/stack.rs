use std::collections::hash_map::Entry;
use std::collections::HashMap;

use super::Reference;
use super::Slot;
use crate::nonempty;
use crate::scope;
use crate::scope::StorageDuration;

#[derive(Debug, Default)]
pub(super) struct Stack<'a> {
    names: nonempty::Vec<HashMap<&'a str, Reference<'a>>>,
}

impl<'a> Stack<'a> {
    #[expect(unused)]
    fn lookup(&self, name: &'a str) -> Option<Reference<'a>> {
        self.names
            .iter()
            .rev()
            .find_map(|names| names.get(name))
            .copied()
    }

    #[expect(unused)]
    fn lookup_innermost(&mut self, name: &'a str) -> Entry<&'a str, Reference<'a>> {
        self.names.last_mut().entry(name)
    }

    pub(super) fn push(&mut self) {
        self.names.push(HashMap::default());
    }

    pub(super) fn pop(&mut self) {
        self.names.pop();
    }

    pub(super) fn add(&mut self, reference: scope::Reference<'a>) -> Reference<'a> {
        // TODO: lookup reference
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
            // TODO: do actual stack layout
            StorageDuration::Automatic => Slot::Automatic(0),
        };
        Reference { name, ty, id, kind, slot }
    }
}
