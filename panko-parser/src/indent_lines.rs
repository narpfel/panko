use std::fmt::Write as _;

use crate::SEXPR_INDENT;

pub(crate) trait IndentLines {
    fn indent_lines(&self) -> String;
}

impl IndentLines for str {
    fn indent_lines(&self) -> String {
        self.lines().fold(String::new(), |mut s, line| {
            writeln!(s, "{:SEXPR_INDENT$}{line}", "").unwrap();
            s
        })
    }
}
