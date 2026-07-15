use std::bstr::ByteStr;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::collections::hash_map::OccupiedEntry;
use std::fmt::Display;
use std::vec::Drain;

use itertools::Either;
use panko_lex::Loc;
use panko_parser::StructKind;
use panko_parser::ast;
use panko_parser::ast::Session;
use panko_parser::ast::TypeDeclaration;
use panko_parser::nonempty;

use super::BuiltinNameKind;
use super::Id;
use super::IsInGlobalScope;
use super::IsParameter;
use super::Linkage;
use super::QualifiedType;
use super::RefInitialiser;
use super::Reference;
use super::StorageDuration;
use super::Type;
use crate::scope::BuiltinName;
use crate::ty::Complete;
use crate::ty::Struct;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Tag {
    Struct,
    Union,
}

impl Display for Tag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Struct => "struct",
            Self::Union => "union",
        };
        write!(f, "{s}")
    }
}

impl From<StructKind> for Tag {
    fn from(kind: StructKind) -> Self {
        match kind {
            StructKind::Struct => Self::Struct,
            StructKind::Union => Self::Union,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) struct Tagged<'a> {
    pub(super) ty: Type<'a>,
    pub(super) tag: Tag,
}

#[derive(Debug, Default)]
struct Scope<'a> {
    names: nonempty::Vec<HashMap<&'a str, Reference<'a>>>,
    type_names: nonempty::Vec<HashMap<&'a str, QualifiedType<'a>>>,
    tagged: nonempty::Vec<HashMap<&'a str, Tagged<'a>>>,
    function_name: Option<&'a str>,
}

impl<'a> Scope<'a> {
    fn function(function_name: &'a str) -> Scope<'a> {
        Self {
            function_name: Some(function_name),
            ..Self::default()
        }
    }

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

    fn tagged_entry(&mut self, name: &'a str) -> Option<OccupiedEntry<&'a str, Tagged<'a>>> {
        self.tagged
            .iter_mut()
            .rev()
            .find_map(|scope| match scope.entry(name) {
                Entry::Occupied(entry) => Some(entry),
                Entry::Vacant(_) => None,
            })
    }

    fn lookup_tagged_innermost(&mut self, name: &'a str) -> Entry<&'a str, Tagged<'a>> {
        self.tagged.last_mut().entry(name)
    }

    fn push(&mut self) {
        let Self {
            names,
            type_names,
            tagged,
            function_name: _,
        } = self;
        names.push(HashMap::default());
        type_names.push(HashMap::default());
        tagged.push(HashMap::default());
    }

    fn pop(&mut self) {
        let Self {
            names,
            type_names,
            tagged,
            function_name: _,
        } = self;
        names.pop();
        type_names.pop();
        tagged.pop();
    }
}

#[derive(Debug)]
pub(super) struct Scopes<'a> {
    pub(super) sess: &'a Session<'a>,
    /// at most two elements: the global scope and a function scope
    scopes: nonempty::Vec<Scope<'a>>,
    next_id: u64,
    hoisted_compound_literal_decls: Vec<Reference<'a>>,
}

impl<'a> Scopes<'a> {
    pub(super) fn new(sess: &'a Session<'a>) -> Self {
        Self {
            sess,
            scopes: nonempty::Vec::default(),
            next_id: 0,
            hoisted_compound_literal_decls: vec![],
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

    pub(super) fn add_function(
        &mut self,
        name: &'a str,
        loc: Loc<'a>,
        ty: QualifiedType<'a>,
        linkage: Linkage,
    ) -> Result<Reference<'a>, QualifiedType<'a>> {
        self.add(
            name,
            loc,
            ty,
            StorageDuration::Static(Some(linkage)),
            IsParameter::No,
            IsInGlobalScope::Yes,
        )
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

    pub(super) fn lookup(
        &self,
        name: &'a str,
        loc: Loc<'a>,
    ) -> Option<Either<Reference<'a>, BuiltinName<'a>>> {
        match name {
            "__panko_gp_offset" if let IsInGlobalScope::No = self.is_in_global_scope() =>
                Some(Either::Right(BuiltinName {
                    kind: BuiltinNameKind::GpOffset,
                    loc,
                })),
            "__panko_overflow_arg_area" if let IsInGlobalScope::No = self.is_in_global_scope() =>
                Some(Either::Right(BuiltinName {
                    kind: BuiltinNameKind::OverflowArgArea,
                    loc,
                })),
            "__func__" if let IsInGlobalScope::No = self.is_in_global_scope() =>
                Some(Either::Right(BuiltinName {
                    kind: BuiltinNameKind::Func(ByteStr::new(
                        self.scopes.last().function_name.unwrap(),
                    )),
                    loc,
                })),
            _ => self
                .scopes
                .iter()
                .rev()
                .find_map(|scope| scope.lookup(name))
                .map(|reference| Either::Left(reference.at(loc))),
        }
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

    fn lookup_tagged_innermost(&mut self, name: &'a str) -> Entry<&'a str, Tagged<'a>> {
        self.scopes.last_mut().lookup_tagged_innermost(name)
    }

    fn tagged_entry(&mut self, name: &'a str) -> Option<OccupiedEntry<&'a str, Tagged<'a>>> {
        self.scopes
            .iter_mut()
            .rev()
            .find_map(|scope| scope.tagged_entry(name))
    }

    fn lookup_tagged(&mut self, name: &'a str) -> Option<Tagged<'a>> {
        self.tagged_entry(name).map(|entry| *entry.get())
    }

    pub(super) fn lookup_or_add_struct(&mut self, name: &'a str, kind: StructKind) -> Tagged<'a> {
        self.lookup_tagged(name).unwrap_or_else(|| {
            let id = self.id();
            let r#struct = Type::Struct(Struct::Incomplete { name, id, kind });
            let tagged = Tagged { ty: r#struct, tag: kind.into() };
            *self
                .lookup_tagged_innermost(name)
                .insert_entry(tagged)
                .get()
        })
    }

    pub(super) fn lookup_or_add_complete_struct(
        &mut self,
        name: Option<&'a str>,
        kind: StructKind,
        members: &'a [Either<TypeDeclaration<'a>, ast::Member<'a>>],
    ) -> (Tagged<'a>, Option<Tagged<'a>>) {
        let previous_definition = try { self.lookup_tagged(name?)? };

        // forward declare so that `name` is available in the body
        let forward_decl = try { self.lookup_or_add_struct(name?, kind).ty };

        let members = super::resolve_struct_members(self, members);

        let id = match forward_decl {
            Some(Type::Struct(r#struct)) => r#struct.id(),
            Some(_) => unreachable!(),
            None => self.id(),
        };
        let ty = Type::Struct(Struct::Complete(Complete { name, id, kind, members }));
        let tagged = Tagged { ty, tag: kind.into() };

        if let Some(name) = name {
            // complete the forward declaration
            self.lookup_tagged_innermost(name).insert_entry(tagged);
        }

        (tagged, previous_definition)
    }

    pub(super) fn push(&mut self, function_name: &'a str) {
        self.scopes.push(Scope::function(function_name));
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

    pub(super) fn id(&mut self) -> Id {
        let id = Id(self.next_id);
        self.next_id += 1;
        id
    }

    pub(super) fn is_in_global_scope(&self) -> IsInGlobalScope {
        match self.scopes.len() {
            0 => unreachable!("self.scopes is non-empty"),
            1 => IsInGlobalScope::Yes,
            2.. => IsInGlobalScope::No,
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

    pub(crate) fn hoist_compound_literal(&mut self, reference: Reference<'a>) {
        self.hoisted_compound_literal_decls.push(reference)
    }

    pub(crate) fn take_hoisted_compound_literals(&mut self) -> Drain<Reference<'a>> {
        self.hoisted_compound_literal_decls.drain(..)
    }
}
