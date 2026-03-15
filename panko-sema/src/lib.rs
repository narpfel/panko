#![feature(bstr)]
#![feature(cmp_minmax)]
#![feature(gen_blocks)]
#![feature(never_type)]
#![feature(stmt_expr_attributes)]
#![feature(try_blocks)]
#![feature(unqualified_local_imports)]

use std::fmt::Display;

use itertools::Itertools;

pub use crate::layout::layout;
pub use crate::scope::resolve_names;
pub use crate::typecheck::resolve_types;

mod fake_trait_impls;
pub mod layout;
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
