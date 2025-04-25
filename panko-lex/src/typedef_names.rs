use std::collections::HashSet;

pub struct TypedefNames<'a> {
    // TODO: should be a `nonempty::Vec`
    scopes: Vec<HashSet<&'a str>>,
}

impl<'a> TypedefNames<'a> {
    pub fn insert(&mut self, name: &'a str) {
        self.scopes.last_mut().unwrap().insert(name);
    }

    pub fn contains(&self, name: &str) -> bool {
        self.scopes.iter().any(|scope| scope.contains(name))
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashSet::default())
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
        assert!(!self.scopes.is_empty());
    }
}

impl Default for TypedefNames<'_> {
    fn default() -> Self {
        Self { scopes: vec![HashSet::default()] }
    }
}
