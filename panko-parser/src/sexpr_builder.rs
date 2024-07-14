use std::fmt;

use crate::NO_VALUE;
use crate::SEXPR_INDENT;

const EMPTY: &str = "";

pub struct SExpr<'a> {
    pub name: String,
    pub params: Vec<Param<'a>>,
}

impl SExpr<'_> {
    pub fn string(s: &str) -> Self {
        Self { name: s.to_owned(), params: vec![] }
    }

    fn fmt(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        write!(f, "{EMPTY:indent$}({}", self.name)?;
        let mut has_line_break = false;
        for param in &self.params {
            match param {
                Param::Inherit(param) if !param.is_multiline() && !has_line_break => {
                    write!(f, " ")?;
                    param.as_sexpr().fmt(f, 0)?;
                }
                Param::Inherit(param) | Param::Line(param) => {
                    writeln!(f)?;
                    param.as_sexpr().fmt(f, indent + SEXPR_INDENT)?;
                    has_line_break = true;
                }
                Param::InlineString(s) if !has_line_break => {
                    write!(f, " {s}")?;
                }
                Param::InlineString(s) | Param::String(s) => {
                    write!(f, "\n{EMPTY:indent$}{s}", indent = indent + SEXPR_INDENT)?;
                    has_line_break = true;
                }
            }
        }
        write!(f, ")")
    }
}

impl fmt::Display for SExpr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExpr::fmt(self, f, 0)
    }
}

pub enum Param<'a> {
    Line(Box<dyn AsSExpr + 'a>),
    Inherit(Box<dyn AsSExpr + 'a>),
    InlineString(String),
    String(String),
}

impl Param<'_> {
    fn is_multiline(&self) -> bool {
        match self {
            Param::Line(_) => true,
            Param::Inherit(param) => param.is_multiline(),
            Param::InlineString(_) => false,
            Param::String(_) => true,
        }
    }
}

pub trait AsSExpr {
    fn as_sexpr(&self) -> SExpr;

    fn is_multiline(&self) -> bool {
        self.as_sexpr().params.iter().any(Param::is_multiline)
    }
}

impl<T> AsSExpr for Option<T>
where
    T: AsSExpr,
{
    fn as_sexpr(&self) -> SExpr {
        match self {
            None => SExpr {
                name: NO_VALUE.to_owned(),
                params: vec![],
            },
            Some(value) => value.as_sexpr(),
        }
    }
}
