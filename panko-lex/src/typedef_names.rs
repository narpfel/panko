use std::collections::HashMap;

use crate::TokenKind;

#[derive(Debug)]
pub struct TypedefNames<'a> {
    // TODO: should be a `nonempty::Vec`
    scopes: Vec<HashMap<&'a str, TokenKind>>,
}

impl<'a> TypedefNames<'a> {
    pub fn insert(&mut self, name: &'a str, kind: TokenKind) {
        self.scopes.last_mut().unwrap().insert(name, kind);
    }

    pub fn is_type_identifier(&self, name: &str) -> bool {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| Some(*scope.get(name)? == TokenKind::TypeIdentifier))
            .unwrap_or(false)
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::default())
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
        assert!(!self.scopes.is_empty());
    }
}

impl Default for TypedefNames<'_> {
    fn default() -> Self {
        Self { scopes: vec![HashMap::default()] }
    }
}
