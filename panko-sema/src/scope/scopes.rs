use std::assert_matches;
use std::bstr::ByteStr;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::collections::hash_map::OccupiedEntry;
use std::fmt::Display;
use std::vec::Drain;

use itertools::Either;
use panko_lex::Loc;
use panko_lex::Token;
use panko_parser::StructKind;
use panko_parser::ast;
use panko_parser::ast::Session;
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
use crate::fake_trait_impls::NoHashEq;
use crate::scope::BuiltinName;
use crate::scope::Enumerator;
use crate::scope::Expression;
use crate::scope::Redeclared;
use crate::ty::Complete;
use crate::ty::CompleteEnum;
use crate::ty::Enum;
use crate::ty::Struct;

#[derive(Debug, Clone, Copy)]
struct Unfixupped<'a>(Enumerator<'a>);

#[derive(Debug, Clone, Copy)]
pub(crate) enum Name<'a, E = Enumerator<'a>> {
    Reference(Reference<'a>),
    Enumerator(E),
}

impl<'a> Name<'a> {
    pub(super) fn loc(&self) -> Loc<'a> {
        match self {
            Self::Reference(reference) => reference.loc(),
            Self::Enumerator(enumerator) => enumerator.loc(),
        }
    }

    pub(super) fn name(&self) -> &'a str {
        match self {
            Self::Reference(reference) => reference.name,
            Self::Enumerator(enumerator) => enumerator.name,
        }
    }

    pub(super) fn ty(&self) -> Option<&QualifiedType<'a>> {
        match self {
            Self::Reference(reference) => Some(&reference.ty),
            Self::Enumerator(_) => None,
        }
    }

    fn at(&self, loc: Loc<'a>) -> Name<'a> {
        match self {
            Self::Reference(reference) => Self::Reference(reference.at(loc)),
            Self::Enumerator(enumerator) => Self::Enumerator(Enumerator { loc, ..*enumerator }),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Tag {
    Struct,
    Union,
    Enum,
}

impl Display for Tag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Struct => "struct",
            Self::Union => "union",
            Self::Enum => "enum",
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
    pub(super) loc: Option<Token<'a>>,
}

#[derive(Debug, Default)]
struct Scope<'a> {
    names: nonempty::Vec<HashMap<&'a str, Name<'a, Unfixupped<'a>>>>,
    type_names: nonempty::Vec<HashMap<&'a str, QualifiedType<'a>>>,
    tagged: nonempty::Vec<HashMap<&'a str, Id>>,
    function_name: Option<&'a str>,
}

impl<'a> Scope<'a> {
    fn function(function_name: &'a str) -> Scope<'a> {
        Self {
            function_name: Some(function_name),
            ..Self::default()
        }
    }

    fn lookup(&self, name: &'a str) -> Option<Name<'a, Unfixupped<'a>>> {
        self.names
            .iter()
            .rev()
            .find_map(|names| names.get(name))
            .copied()
    }

    fn lookup_innermost(&mut self, name: &'a str) -> Entry<&'a str, Name<'a, Unfixupped<'a>>> {
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

    fn tagged_entry<'e>(
        &mut self,
        env: &'e mut Env<'a>,
        name: &'a str,
    ) -> Option<OccupiedEntry<'e, Id, Tagged<'a>>> {
        let mut scopes = self.tagged.iter_mut().rev();
        let id = scopes.find_map(|scope| scope.get(name))?;
        match env.tagged.entry(*id) {
            Entry::Occupied(entry) => Some(entry),
            Entry::Vacant(_) => None,
        }
    }

    fn lookup_tagged_innermost<'e>(
        &mut self,
        env: &'e mut Env<'a>,
        name: &'a str,
        id: Id,
    ) -> Entry<'e, Id, Tagged<'a>> {
        let id = self.tagged.last_mut().entry(name).or_insert(id);
        env.tagged.entry(*id)
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

// This is used to lookup types by `id`.
//
// TODO: Check out if the same could be achieved by using a data structure like `iddqd::BiHashMap`.
#[derive(Debug, Default)]
struct Env<'a> {
    tagged: HashMap<Id, Tagged<'a>>,
}

impl<'a> Env<'a> {
    fn fixup_enumerator_ty(&self, name: Name<'a, Unfixupped<'a>>) -> Name<'a> {
        match name {
            Name::Reference(reference) => Name::Reference(reference),
            Name::Enumerator(Unfixupped(enumerator)) => {
                let ty = match self.tagged.get(&enumerator.ty.id()) {
                    Some(Tagged { ty: Type::Enum(r#enum), tag: _, loc: _ }) => *r#enum,
                    _ => unreachable!(),
                };
                Name::Enumerator(Enumerator { ty, ..enumerator })
            }
        }
    }
}

#[derive(Debug)]
pub(super) struct Scopes<'a> {
    pub(super) sess: &'a Session<'a>,
    /// at most two elements: the global scope and a function scope
    scopes: nonempty::Vec<Scope<'a>>,
    env: Env<'a>,
    next_id: u64,
    hoisted_compound_literal_decls: Vec<Reference<'a>>,
}

impl<'a> Scopes<'a> {
    pub(super) fn new(sess: &'a Session<'a>) -> Self {
        Self {
            sess,
            scopes: nonempty::Vec::default(),
            env: Env::default(),
            next_id: 0,
            hoisted_compound_literal_decls: vec![],
        }
    }

    #[expect(clippy::result_large_err)]
    pub(super) fn add(
        &mut self,
        name: &'a str,
        loc: Loc<'a>,
        ty: QualifiedType<'a>,
        storage_duration: StorageDuration<Option<Linkage>>,
        is_parameter: IsParameter,
        is_in_global_scope: IsInGlobalScope,
    ) -> Result<Reference<'a>, Redeclared<'a>> {
        if let Entry::Occupied(entry) = self.lookup_ty_innermost(name) {
            return Err(Redeclared::TypedefAsValue {
                at: loc,
                typedef_ty: *entry.get(),
                value_ty: ty,
            });
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
        match self.scopes.last_mut().lookup_innermost(name) {
            Entry::Occupied(mut entry) => {
                let stored = entry.get_mut();
                let previous_definition = match self.env.fixup_enumerator_ty(*stored) {
                    Name::Reference(reference) => reference,
                    Name::Enumerator(enumerator) =>
                        return Err(Redeclared::EnumeratorAsVariable {
                            enumerator,
                            at: loc,
                            value_ty: ty,
                        }),
                };
                let reference = Reference {
                    id: previous_definition.id,
                    previous_definition: Some(
                        sess.alloc(previous_definition.at(previous_definition.loc())),
                    ),
                    ..reference
                };
                *stored = Name::Reference(reference);
                Ok(reference)
            }
            Entry::Vacant(entry) => {
                entry.insert(Name::Reference(reference));
                Ok(reference)
            }
        }
    }

    #[expect(clippy::result_large_err)]
    pub(super) fn add_enumerator(
        &mut self,
        name: Token<'a>,
        ty: Enum<'a, super::Scope>,
        index: usize,
        value: Option<&'a Expression<'a>>,
    ) -> Result<Enumerator<'a>, Redeclared<'a>> {
        if let Entry::Occupied(entry) = self.lookup_ty_innermost(name.slice()) {
            return Err(Redeclared::TypedefAsValue {
                at: name.loc(),
                typedef_ty: *entry.get(),
                value_ty: Type::Enum(ty).unqualified(),
            });
        }

        let loc = name.loc();
        let name = name.slice();
        let id = self.id();
        let enumerator = Enumerator { name, loc, id, ty, index, value };
        match self.scopes.last_mut().lookup_innermost(name) {
            Entry::Occupied(mut entry) => {
                let stored = entry.get_mut();
                let previous_definition = match self.env.fixup_enumerator_ty(*stored) {
                    Name::Reference(reference) =>
                        return Err(Redeclared::VariableAsEnumerator {
                            at: loc,
                            variable: reference,
                        }),
                    Name::Enumerator(enumerator) => enumerator,
                };
                // TODO: check that `enumerator` is a valid redeclaration of `previous_definition`
                let enumerator = Enumerator { id: previous_definition.id, ..enumerator };
                *stored = Name::Enumerator(Unfixupped(enumerator));
                Ok(enumerator)
            }
            Entry::Vacant(entry) => {
                entry.insert(Name::Enumerator(Unfixupped(enumerator)));
                Ok(enumerator)
            }
        }
    }

    #[expect(clippy::result_large_err)]
    pub(super) fn add_ty(
        &mut self,
        name: &'a str,
        ty: QualifiedType<'a>,
    ) -> Result<Option<QualifiedType<'a>>, Name<'a>> {
        if let Entry::Occupied(entry) = self.scopes.last_mut().lookup_innermost(name) {
            return Err(self.env.fixup_enumerator_ty(*entry.get()));
        }

        match self.lookup_ty_innermost(name) {
            Entry::Occupied(entry) => Ok(Some(*entry.get())),
            Entry::Vacant(entry) => {
                entry.insert(ty);
                Ok(None)
            }
        }
    }

    #[expect(clippy::result_large_err)]
    pub(super) fn add_function(
        &mut self,
        name: &'a str,
        loc: Loc<'a>,
        ty: QualifiedType<'a>,
        linkage: Linkage,
    ) -> Result<Reference<'a>, Redeclared<'a>> {
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
    ) -> Option<Either<Name<'a>, BuiltinName<'a>>> {
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
                .map(|name| Either::Left(self.env.fixup_enumerator_ty(name).at(loc))),
        }
    }

    fn lookup_innermost(&mut self, name: &'a str) -> Entry<&'a str, Name<'a, Unfixupped<'a>>> {
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

    fn lookup_tagged_innermost(&mut self, name: &'a str, id: Id) -> Entry<Id, Tagged<'a>> {
        self.scopes
            .last_mut()
            .lookup_tagged_innermost(&mut self.env, name, id)
    }

    fn get_tagged_innermost(&mut self, name: &'a str) -> Option<Tagged<'a>> {
        let id = self.scopes.last_mut().tagged.last_mut().get(name)?;
        self.env.tagged.get(id).copied()
    }

    fn tagged_entry(&mut self, name: &'a str) -> Option<OccupiedEntry<Id, Tagged<'a>>> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(entry) = scope.tagged_entry(&mut self.env, name) {
                return Some(entry);
            }
        }
        None
    }

    fn lookup_tagged(&mut self, name: &'a str) -> Option<Tagged<'a>> {
        self.tagged_entry(name).map(|entry| *entry.get())
    }

    pub(super) fn lookup_or_add_struct(&mut self, loc: Token<'a>, kind: StructKind) -> Tagged<'a> {
        let name = loc.slice();
        self.lookup_tagged(name).unwrap_or_else(|| {
            let id = self.id();
            let r#struct = Type::Struct(Struct::Incomplete { name, id, kind });
            let tagged = Tagged {
                ty: r#struct,
                tag: kind.into(),
                loc: Some(loc),
            };
            *self
                .lookup_tagged_innermost(name, id)
                .insert_entry(tagged)
                .get()
        })
    }

    fn lookup_or_add_struct_innermost(&mut self, loc: Token<'a>, kind: StructKind) -> Tagged<'a> {
        let name = loc.slice();
        let scope = self.scopes.last_mut().tagged.last_mut();
        let id = scope.get(name).copied().unwrap_or_else(|| self.id());
        let entry = self
            .scopes
            .last_mut()
            .lookup_tagged_innermost(&mut self.env, name, id);
        *entry.or_insert_with(|| Tagged {
            ty: Type::Struct(Struct::Incomplete { name, id, kind }),
            tag: kind.into(),
            loc: Some(loc),
        })
    }

    pub(super) fn lookup_or_add_enum(&mut self, loc: Option<Token<'a>>) -> Tagged<'a> {
        let name = try { loc?.slice() };
        try { self.lookup_tagged(name?)? }.unwrap_or_else(|| {
            let id = self.id();
            let r#enum = Type::Enum(Enum::Incomplete { name, id });
            let tagged = Tagged { ty: r#enum, tag: Tag::Enum, loc };
            match name {
                Some(name) => *self
                    .lookup_tagged_innermost(name, id)
                    .insert_entry(tagged)
                    .get(),
                None => {
                    let was_present = self.env.tagged.insert(id, tagged);
                    assert_matches!(was_present, None);
                    tagged
                }
            }
        })
    }

    pub(super) fn lookup_or_add_complete_struct(
        &mut self,
        loc: Option<Token<'a>>,
        kind: StructKind,
        members: &'a [ast::Declaration<'a, ast::Member<'a>>],
    ) -> (Tagged<'a>, Option<Tagged<'a>>) {
        let name = try { loc?.slice() };
        let previous_definition = try { self.get_tagged_innermost(name?)? };

        // forward declare so that `name` is available in the body
        let forward_decl = try { self.lookup_or_add_struct_innermost(loc?, kind).ty };

        let members = super::resolve_struct_members(self, members);

        let id = match forward_decl {
            Some(Type::Struct(r#struct)) => r#struct.id(),
            Some(_) => unreachable!(),
            None => self.id(),
        };
        let ty = Type::Struct(Struct::Complete(Complete { name, id, kind, members }));
        let tagged = Tagged { ty, tag: kind.into(), loc };

        if let Some(name) = name {
            // complete the forward declaration
            self.lookup_tagged_innermost(name, id).insert_entry(tagged);
        }

        (tagged, previous_definition)
    }

    pub(super) fn lookup_or_add_complete_enum(
        &mut self,
        loc: Option<Token<'a>>,
        enumerators: &'a [panko_parser::Enumerator<'a>],
    ) -> (Tagged<'a>, Option<Tagged<'a>>) {
        let name = try { loc?.slice() };
        let previous_definition = try { self.get_tagged_innermost(name?)? };

        // forward declare so that `name` is available in the body
        let forward_decl = match self.lookup_or_add_enum(loc).ty {
            Type::Enum(r#enum) => r#enum,
            Type::Struct(r#struct) => Enum::Incomplete { name, id: r#struct.id() },
            _ => unreachable!(),
        };

        let enumerators = NoHashEq(super::resolve_enumerators(self, forward_decl, enumerators));
        let id = forward_decl.id();
        let ty = Type::Enum(Enum::Complete(CompleteEnum { name, id, enumerators }));
        let tagged = Tagged { ty, tag: Tag::Enum, loc };

        // complete the forward declaration
        match name {
            Some(name) => {
                self.lookup_tagged_innermost(name, id).insert_entry(tagged);
            }
            None =>
                if let Some(previous_decl) = self.env.tagged.insert(id, tagged) {
                    assert_matches!(
                        previous_decl,
                        Tagged {
                            ty: Type::Enum(Enum::Incomplete { name: None, id: old_id }),
                            tag: Tag::Enum,
                            loc: None,
                        }
                        if id == old_id,
                    )
                },
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
            Entry::Occupied(mut entry) => match entry.get_mut() {
                Name::Reference(reference) => reference.initialiser = initialiser,
                Name::Enumerator(_) => unreachable!(),
            },
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
