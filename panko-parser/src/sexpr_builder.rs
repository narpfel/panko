use std::borrow::Cow;
use std::fmt;
use std::iter::once;
use std::iter::repeat;

use itertools::Either;
use panko_lex::Token;
use yansi::Paint as _;

use crate::NO_VALUE;
use crate::SEXPR_INDENT;

const EMPTY: &str = "";

pub struct SExpr<'a> {
    name: Cow<'a, str>,
    params: Vec<Param<'a>>,
    parenthesise_if_empty: bool,
}

impl<'a> SExpr<'a> {
    pub fn new(s: impl Into<Cow<'a, str>>) -> Self {
        Self {
            name: s.into(),
            params: vec![],
            parenthesise_if_empty: true,
        }
    }

    pub fn string(s: impl Into<Cow<'a, str>>) -> Self {
        Self {
            name: s.into(),
            params: vec![],
            parenthesise_if_empty: false,
        }
    }

    fn list() -> Self {
        Self {
            name: "".into(),
            params: vec![],
            parenthesise_if_empty: true,
        }
    }

    pub fn display(value: &dyn fmt::Display) -> Self {
        Self {
            name: format!("`{value}`").into(),
            params: vec![],
            parenthesise_if_empty: false,
        }
    }

    pub fn lines<T>(mut self, params: impl IntoIterator<Item = &'a T>) -> Self
    where
        T: AsSExpr + 'a,
    {
        self.params
            .extend(params.into_iter().map(|param| Param::Line(param)));
        self
    }

    pub fn lines_explicit_empty<T, IntoIter>(mut self, params: IntoIter) -> Self
    where
        IntoIter: IntoIterator<Item = &'a T>,
        IntoIter::IntoIter: ExactSizeIterator,
        T: AsSExpr + 'a,
    {
        let params = params.into_iter();
        if params.len() == 0 {
            self.params.push(Param::Inherit(&()));
        }
        self.lines(params)
    }

    pub fn inherit(mut self, param: &'a dyn AsSExpr) -> Self {
        self.params.push(Param::Inherit(param));
        self
    }

    pub fn inherit_front(mut self, param: &'a dyn AsSExpr) -> Self {
        self.params.insert(0, Param::Inherit(param));
        self
    }

    pub fn inherit_many<T>(mut self, params: impl IntoIterator<Item = &'a T>) -> Self
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

    pub fn inline_string(mut self, string: String) -> Self {
        self.params.push(Param::InlineString(string));
        self
    }

    fn fmt(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let should_parenthesise = self.params.is_empty() && !self.parenthesise_if_empty;
        let (open_paren, close_paren) = if should_parenthesise {
            ("", "")
        }
        else {
            ("(", ")")
        };

        write!(
            f,
            "{EMPTY:indent$}{open_paren}{}",
            self.name
                .bold()
                .whenever(yansi::Condition::cached(!should_parenthesise)),
        )?;
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
        write!(f, "{close_paren}")
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
        SExpr::string(*self)
    }
}

impl<T, U> AsSExpr for Either<T, U>
where
    T: AsSExpr,
    U: AsSExpr,
{
    fn as_sexpr(&self) -> SExpr {
        match self {
            Either::Left(left) => left.as_sexpr(),
            Either::Right(right) => right.as_sexpr(),
        }
    }
}

impl AsSExpr for ! {
    fn as_sexpr(&self) -> SExpr {
        match *self {}
    }
}
