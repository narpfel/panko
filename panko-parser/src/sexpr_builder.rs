use std::fmt;

use crate::NO_VALUE;
use crate::SEXPR_INDENT;

const EMPTY: &str = "";

pub struct SExpr<'a> {
    name: String,
    params: Vec<Param<'a>>,
    parenthesise_if_empty: bool,
}

impl<'a> SExpr<'a> {
    pub(crate) fn new(s: &str) -> Self {
        Self {
            name: s.to_owned(),
            params: vec![],
            parenthesise_if_empty: true,
        }
    }

    pub(crate) fn string(s: &str) -> Self {
        Self {
            name: s.to_owned(),
            params: vec![],
            parenthesise_if_empty: false,
        }
    }

    pub(crate) fn lines<T>(mut self, params: impl IntoIterator<Item = &'a T>) -> Self
    where
        T: AsSExpr + 'a,
    {
        self.params
            .extend(params.into_iter().map(|param| Param::Line(param)));
        self
    }

    pub(crate) fn inherit(mut self, param: &'a dyn AsSExpr) -> Self {
        self.params.push(Param::Inherit(param));
        self
    }

    pub(crate) fn debug<T>(mut self, params: impl IntoIterator<Item = T>) -> Self
    where
        T: std::fmt::Debug,
    {
        self.params.extend(
            params
                .into_iter()
                .map(|param| Param::String(format!("{param:?}"))),
        );
        self
    }

    pub(crate) fn debug_explicit_empty<T, IntoIter>(mut self, params: IntoIter) -> Self
    where
        IntoIter: IntoIterator<Item = T>,
        IntoIter::IntoIter: ExactSizeIterator,
        T: std::fmt::Debug,
    {
        let params = params.into_iter();
        if params.len() == 0 {
            self.params.push(Param::Inherit(&()));
        }
        self.debug(params)
    }

    pub(crate) fn short_inline_explicit_empty<T, IntoIter>(mut self, params: IntoIter) -> Self
    where
        IntoIter: IntoIterator<Item = &'a T>,
        IntoIter::IntoIter: ExactSizeIterator,
        T: AsSExpr + 'a,
    {
        let params = params.into_iter();
        if params.len() == 0 {
            self.params.push(Param::Inherit(&()));
        }
        else {
            let f = if params.len() == 1 {
                Param::Inherit
            }
            else {
                Param::Line
            };
            self.params.extend(params.map(|param| f(param)));
        }
        self
    }

    pub(crate) fn inline_string(mut self, string: String) -> Self {
        self.params.push(Param::InlineString(string));
        self
    }

    fn fmt(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let (open_paren, close_paren) = if self.params.is_empty() && !self.parenthesise_if_empty {
            ("", "")
        }
        else {
            ("(", ")")
        };

        write!(f, "{EMPTY:indent$}{open_paren}{}", self.name)?;
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
        write!(f, "{}", close_paren)
    }
}

impl fmt::Display for SExpr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExpr::fmt(self, f, 0)
    }
}

enum Param<'a> {
    Line(&'a dyn AsSExpr),
    Inherit(&'a dyn AsSExpr),
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
            None => ().as_sexpr(),
            Some(value) => value.as_sexpr(),
        }
    }
}

impl AsSExpr for () {
    fn as_sexpr(&self) -> SExpr {
        SExpr::string(NO_VALUE)
    }
}
