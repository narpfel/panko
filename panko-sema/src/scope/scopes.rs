use std::collections::HashMap;
use std::collections::hash_map::Entry;

use panko_lex::Loc;
use panko_parser::ast::Session;
use panko_parser::nonempty;

use super::Id;
use super::IsInGlobalScope;
use super::IsParameter;
use super::Linkage;
use super::QualifiedType;
use super::RefInitialiser;
use super::Reference;
use super::StorageDuration;

#[derive(Debug, Default)]
struct Scope<'a> {
    names: nonempty::Vec<HashMap<&'a str, Reference<'a>>>,
    type_names: nonempty::Vec<HashMap<&'a str, QualifiedType<'a>>>,
}

impl<'a> Scope<'a> {
    fn lookup(&self, name: &'a str) -> Option<Reference<'a>> {
        self.names
            .iter()
            .rev()
            .find_map(|names| names.get(name))
            .copied()
    }

    fn lookup_innermost(&mut self, name: &'a str) -> Entry<&'a str, Reference<'a>> {
        self.names.last_mut().entry(name)
    }

    fn lookup_ty(&self, name: &'a str) -> Option<QualifiedType<'a>> {
        self.type_names
            .iter()
            .rev()
            .find_map(|names| names.get(name))
            .copied()
    }

    fn lookup_ty_innermost(&mut self, name: &'a str) -> Entry<&'a str, QualifiedType<'a>> {
        self.type_names.last_mut().entry(name)
    }

    fn push(&mut self) {
        self.names.push(HashMap::default());
        self.type_names.push(HashMap::default());
    }

    fn pop(&mut self) {
        self.names.pop();
        self.type_names.pop();
    }
}

#[derive(Debug)]
pub(super) struct Scopes<'a> {
    pub(super) sess: &'a Session<'a>,
    /// at most two elements: the global scope and a function scope
    scopes: nonempty::Vec<Scope<'a>>,
    next_id: u64,
}

impl<'a> Scopes<'a> {
    pub(super) fn new(sess: &'a Session<'a>) -> Self {
        Self {
            sess,
            scopes: nonempty::Vec::default(),
            next_id: 0,
        }
    }

    pub(super) fn add(
        &mut self,
        name: &'a str,
        loc: Loc<'a>,
        ty: QualifiedType<'a>,
        storage_duration: StorageDuration<Option<Linkage>>,
        is_parameter: IsParameter,
        is_in_global_scope: IsInGlobalScope,
    ) -> Result<Reference<'a>, QualifiedType<'a>> {
        if let Entry::Occupied(entry) = self.lookup_ty_innermost(name) {
            return Err(*entry.get());
        }

        let sess = self.sess;
        let id = self.id();
        let reference = Reference {
            name,
            decl_loc: loc,
            ty,
            id,
            usage_loc: loc,
            storage_duration,
            previous_definition: None,
            is_parameter,
            is_in_global_scope,
            initialiser: None,
        };
        match self.lookup_innermost(name) {
            Entry::Occupied(mut entry) => {
                let previous_definition = entry.get_mut();
                let reference = Reference {
                    id: previous_definition.id,
                    previous_definition: Some(
                        sess.alloc(previous_definition.at(previous_definition.usage_loc)),
                    ),
                    ..reference
                };
                *previous_definition = reference;
                Ok(reference)
            }
            Entry::Vacant(entry) => {
                entry.insert(reference);
                Ok(reference)
            }
        }
    }

    #[expect(clippy::result_large_err)]
    pub(super) fn add_ty(
        &mut self,
        name: &'a str,
        ty: QualifiedType<'a>,
    ) -> Result<Option<QualifiedType<'a>>, Reference<'a>> {
        if let Entry::Occupied(entry) = self.lookup_innermost(name) {
            return Err(*entry.get());
        }

        match self.lookup_ty_innermost(name) {
            Entry::Occupied(entry) => Ok(Some(*entry.get())),
            Entry::Vacant(entry) => {
                entry.insert(ty);
                Ok(None)
            }
        }
    }

    pub(super) fn temporary(&mut self, loc: Loc<'a>, ty: QualifiedType<'a>) -> Reference<'a> {
        let id = self.id();
        Reference {
            name: "unnamed-temporary",
            decl_loc: loc,
            ty,
            id,
            usage_loc: loc,
            storage_duration: StorageDuration::Automatic,
            previous_definition: None,
            is_parameter: IsParameter::No,
            is_in_global_scope: IsInGlobalScope::No,
            initialiser: None,
        }
    }

    pub(super) fn lookup(&self, name: &'a str, loc: Loc<'a>) -> Option<Reference<'a>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.lookup(name))
            .map(|reference| reference.at(loc))
    }

    fn lookup_innermost(&mut self, name: &'a str) -> Entry<&'a str, Reference<'a>> {
        self.scopes.last_mut().lookup_innermost(name)
    }

    pub(super) fn lookup_ty(&self, name: &'a str) -> Option<QualifiedType<'a>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.lookup_ty(name))
    }

    fn lookup_ty_innermost(&mut self, name: &'a str) -> Entry<&'a str, QualifiedType<'a>> {
        self.scopes.last_mut().lookup_ty_innermost(name)
    }

    pub(super) fn push(&mut self) {
        self.scopes.push(Scope::default());
        assert!(self.scopes.len() <= 2);
    }

    pub(super) fn pop(&mut self) {
        self.scopes.pop().unwrap();
    }

    pub(super) fn open_new_scope(&mut self) {
        self.scopes.last_mut().push()
    }

    pub(super) fn exit_scope(&mut self) {
        self.scopes.last_mut().pop();
    }

    fn id(&mut self) -> Id {
        let id = Id(self.next_id);
        self.next_id += 1;
        id
    }

    pub(super) fn is_in_global_scope(&self) -> IsInGlobalScope {
        if self.scopes.len() == 1 {
            IsInGlobalScope::Yes
        }
        else {
            IsInGlobalScope::No
        }
    }

    pub(super) fn add_initialiser(
        &mut self,
        reference: &Reference<'a>,
        initialiser: Option<RefInitialiser<'a>>,
    ) {
        match self.lookup_innermost(reference.name) {
            Entry::Occupied(mut entry) => entry.get_mut().initialiser = initialiser,
            Entry::Vacant(_) => unreachable!(),
        }
    }
}
