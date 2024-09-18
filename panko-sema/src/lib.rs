#![feature(cmp_minmax)]
#![feature(impl_trait_in_fn_trait_return)]
#![feature(try_blocks)]
#![feature(type_alias_impl_trait)]

pub use crate::layout::layout;
pub use crate::scope::resolve_names;
pub use crate::typecheck::resolve_types;

pub mod layout;
mod nonempty;
pub mod scope;
pub mod typecheck;
