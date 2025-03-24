#![feature(cmp_minmax)]
#![feature(gen_blocks)]
#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(impl_trait_in_fn_trait_return)]
#![feature(never_type)]
#![feature(try_blocks)]

pub use crate::layout::layout;
pub use crate::scope::resolve_names;
pub use crate::typecheck::resolve_types;

pub mod layout;
mod nonempty;
pub mod scope;
pub mod ty;
pub mod typecheck;
