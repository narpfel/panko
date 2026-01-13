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

/// Pretend to implement [`PartialEq`], [`Eq`] and [`Hash`] so that unhashable and un`eq`able types
/// can be used as [`Step`] associated types.
///
/// [`PartialEq::eq`] and [`Hash::hash`] contain post-mono errors, so actually using these fake
/// instances will produce a compile-time error.
///
/// Also, it is encouraged not to implement the faked traits for [`Step`]s containing a
/// [`NoHashEq`]. This will produce a pre-mono error when deriving any of these traits.
///
/// [`Step`]: crate::ty::Step
#[derive(Clone, Copy)]
pub(crate) struct NoHashEq<T>(pub(crate) T);

impl<T> PartialEq for NoHashEq<T> {
    fn eq(&self, _other: &Self) -> bool {
        const { panic!("this impl can’t be used") }
    }
}

impl<T> Eq for NoHashEq<T> {}

impl<T> Hash for NoHashEq<T> {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {
        const { panic!("this impl can’t be used") }
    }
}

impl<T> fmt::Debug for NoHashEq<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl<T> AsSExpr for NoHashEq<T>
where
    T: AsSExpr,
{
    fn as_sexpr(&self) -> SExpr {
        AsSExpr::as_sexpr(&self.0)
    }
}
