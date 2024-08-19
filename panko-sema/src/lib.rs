#![feature(try_blocks)]

pub use crate::scope::resolve_names;
pub use crate::typecheck::resolve_types;

mod nonempty;
mod scope;
mod typecheck;
