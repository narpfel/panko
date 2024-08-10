#![feature(try_blocks)]

use panko_parser::Report;

pub use crate::scope::resolve_names;

mod nonempty;
mod scope;
