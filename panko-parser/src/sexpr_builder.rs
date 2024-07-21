use std::fmt;
use std::iter::once;
use std::iter::repeat;

use panko_lex::Token;

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

    fn list() -> Self {
        Self {
            name: "".to_owned(),
            params: vec![],
            parenthesise_if_empty: true,
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

    pub(crate) fn inherit_many<T>(mut self, params: impl IntoIterator<Item = &'a T>) -> Self
    where
        T: AsSExpr + 'a,
    {
        self.params
            .extend(params.into_iter().map(|param| Param::Inherit(param)));
        self
    }

    pub(crate) fn inherit_many_explicit_empty<T, IntoIter>(mut self, params: IntoIter) -> Self
    where
        IntoIter: IntoIterator<Item = &'a T>,
        IntoIter::IntoIter: ExactSizeIterator,
        T: AsSExpr + 'a,
    {
        let params = params.into_iter();
        if params.len() == 0 {
            self.params.push(Param::Inherit(&()));
        }
        self.inherit_many(params)
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
        let first_was_preceded = !self.name.is_empty();
        for (param, was_preceded) in self
            .params
            .iter()
            .zip(once(first_was_preceded).chain(repeat(true)))
        {
            match param {
                Param::Inherit(param) if !param.is_multiline() && !has_line_break => {
                    if was_preceded {
                        write!(f, " ")?;
                    }
                    param.as_sexpr().fmt(f, 0)?;
                }
                Param::Inherit(param) | Param::Line(param) => {
                    if was_preceded {
                        writeln!(f)?;
                    }
                    param.as_sexpr().fmt(f, indent + SEXPR_INDENT)?;
                    has_line_break = true;
                }
                Param::InlineString(s) if !has_line_break => {
                    if was_preceded {
                        write!(f, " ")?;
                    }
                    write!(f, "{s}")?;
                }
                Param::InlineString(s) | Param::String(s) => {
                    if was_preceded {
                        writeln!(f)?;
                    }
                    write!(f, "{EMPTY:indent$}{s}", indent = indent + SEXPR_INDENT)?;
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
    #[expect(unused)]
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

impl<T> AsSExpr for &T
where
    T: AsSExpr,
{
    fn as_sexpr(&self) -> SExpr {
        (*self).as_sexpr()
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

impl<T> AsSExpr for &[T]
where
    T: AsSExpr,
{
    fn as_sexpr(&self) -> SExpr {
        SExpr::list().inherit_many_explicit_empty(*self)
    }
}

impl AsSExpr for Token<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::string(self.slice())
    }
}

impl AsSExpr for &str {
    fn as_sexpr(&self) -> SExpr {
        SExpr::string(self)
    }
}
