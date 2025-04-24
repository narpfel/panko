#![feature(assert_matches)]
#![feature(bstr)]
#![feature(char_max_len)]
#![feature(cmp_minmax)]
#![feature(gen_blocks)]
#![feature(if_let_guard)]
#![feature(impl_trait_in_fn_trait_return)]
#![feature(never_type)]
#![feature(try_blocks)]
#![feature(unqualified_local_imports)]

use std::fmt::Display;

use itertools::Itertools;

pub use crate::layout::layout;
pub use crate::scope::resolve_names;
pub use crate::typecheck::resolve_types;

pub mod layout;
mod nonempty;
pub mod scope;
pub mod ty;
pub mod typecheck;

trait ItertoolsExt: Itertools {
    fn join_end(self, sep: &str, last_sep: &str) -> String
    where
        Self: Sized,
        Self::Item: Display,
    {
        let values = self.collect_vec();
        match &values[..] {
            [] => String::new(),
            [value] => value.to_string(),
            [init @ .., last] => format!("{}{last_sep}{last}", init.iter().join(sep)),
        }
    }
}

impl<T> ItertoolsExt for T where T: Itertools {}
