use std::fmt;

use itertools::Itertools as _;
use panko_lex::Loc;
use panko_lex::Token;
use panko_parser::ast::Arithmetic;
use panko_parser::ast::Integral;
use panko_parser::ast::IntegralKind;
use panko_parser::ast::Signedness;
use panko_parser::sexpr_builder::AsSExpr;
use panko_parser::sexpr_builder::SExpr;
use panko_parser::NO_VALUE;

#[derive(Debug, Clone, Copy)]
pub struct ParameterDeclaration<'a> {
    pub loc: Loc<'a>,
    pub ty: QualifiedType<'a>,
    pub name: Option<Token<'a>>,
}

impl PartialEq for ParameterDeclaration<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

impl Eq for ParameterDeclaration<'_> {}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionType<'a> {
    pub params: &'a [ParameterDeclaration<'a>],
    pub return_type: &'a QualifiedType<'a>,
    pub is_varargs: bool,
}

impl fmt::Display for FunctionType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { params, return_type, is_varargs } = *self;
        let maybe_ellipsis = match (is_varargs, params.is_empty()) {
            (true, true) => "...",
            (true, false) => ", ...",
            _ => "",
        };
        write!(
            f,
            "fn({}{}) -> {}",
            params.iter().format_with(", ", |param, f| f(&format_args!(
                "{}: {}",
                param.name.map_or(NO_VALUE, |name| name.slice()),
                param.ty,
            ))),
            maybe_ellipsis,
            return_type,
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type<'a> {
    Arithmetic(Arithmetic),
    Pointer(&'a QualifiedType<'a>),
    Function(FunctionType<'a>),
    Void,
    // TODO
}

impl<'a> Type<'a> {
    pub fn int() -> Self {
        Self::Arithmetic(Arithmetic::Integral(Integral {
            signedness: Signedness::Signed,
            kind: IntegralKind::Int,
        }))
    }

    pub fn ullong() -> Self {
        Self::Arithmetic(Arithmetic::Integral(Integral {
            signedness: Signedness::Unsigned,
            kind: IntegralKind::LongLong,
        }))
    }

    pub fn unqualified(&self) -> QualifiedType<'a> {
        QualifiedType {
            is_const: false,
            is_volatile: false,
            ty: *self,
        }
    }

    // TODO: this is a temporary hack until the `Report` derive macro handles stringification
    // better
    pub fn slice(&self) -> String {
        self.to_string()
    }

    pub fn size(&self) -> u64 {
        match self {
            Type::Arithmetic(arithmetic) => arithmetic.size(),
            Type::Pointer(_) => 8,
            Type::Function(_) => unreachable!("functions are not objects and don’t have a size"),
            Type::Void => 0,
        }
    }

    pub fn align(&self) -> u64 {
        // TODO: correct align
        self.size()
    }
}

impl fmt::Display for Type<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Arithmetic(Arithmetic::Integral(integral)) => write!(f, "{integral}"),
            Type::Pointer(pointee) => write!(f, "ptr<{pointee}>"),
            Type::Function(function) => write!(f, "{function}"),
            Type::Void => write!(f, "void"),
        }
    }
}

impl AsSExpr for Type<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::display(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct QualifiedType<'a> {
    pub(crate) is_const: bool,
    pub(crate) is_volatile: bool,
    pub ty: Type<'a>,
}

impl<'a> QualifiedType<'a> {
    // TODO: this is a temporary hack until the `Report` derive macro handles stringification
    // better
    pub(crate) fn slice(&self) -> String {
        self.to_string()
    }
}

impl fmt::Display for QualifiedType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.ty.fmt(f)?;
        if self.is_const {
            write!(f, " const")?;
        }
        if self.is_volatile {
            write!(f, " volatile")?;
        }
        Ok(())
    }
}

impl AsSExpr for QualifiedType<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::display(self)
    }
}
