use std::fmt;
use std::hash::Hash;

use panko_parser::sexpr_builder::AsSExpr;
use panko_parser::sexpr_builder::SExpr;

#[derive(Clone, Copy)]
pub struct HashEqIgnored<T>(pub(crate) T);

impl<T> PartialEq for HashEqIgnored<T> {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl<T> Eq for HashEqIgnored<T> {}

impl<T> Hash for HashEqIgnored<T> {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {}
}

impl<T> fmt::Debug for HashEqIgnored<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl<T> AsSExpr for HashEqIgnored<T>
where
    T: AsSExpr,
{
    fn as_sexpr(&self) -> SExpr {
        AsSExpr::as_sexpr(&self.0)
    }
}
