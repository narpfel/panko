use std::borrow::Cow;
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

impl<'a> ParameterDeclaration<'a> {
    pub(crate) fn loc(&self) -> Loc<'a> {
        self.loc
    }

    pub(crate) fn slice(&self) -> Cow<'a, str> {
        self.name
            .map(|name| Cow::Borrowed(name.slice()))
            .unwrap_or_else(|| Cow::Owned(format!("<unnamed parameter of type `{}`>", self.ty)))
    }
}

impl PartialEq for ParameterDeclaration<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.ty.ty == other.ty.ty
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
                param.ty.ty,
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
    pub const fn uchar() -> Self {
        Self::Arithmetic(Arithmetic::Integral(Integral {
            signedness: Signedness::Unsigned,
            kind: IntegralKind::Char,
        }))
    }

    pub fn int() -> Self {
        Self::Arithmetic(Arithmetic::Integral(Integral {
            signedness: Signedness::Signed,
            kind: IntegralKind::Int,
        }))
    }

    pub fn ulong() -> Self {
        Self::Arithmetic(Arithmetic::Integral(Integral {
            signedness: Signedness::Unsigned,
            kind: IntegralKind::Long,
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
            loc: Loc::synthesised(),
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
            Type::Void => unreachable!("void is not an object and doesn’t have a size"),
        }
    }

    pub fn align(&self) -> u64 {
        // TODO: correct align
        self.size()
    }

    pub(crate) fn is_slot_compatible(&self, ty: &Type) -> bool {
        // TODO: This is more restrictive than necessary.
        self.size() == ty.size() && self.align() == ty.align()
    }

    pub(crate) fn is_object(&self) -> bool {
        !self.is_function()
    }

    pub(crate) fn is_complete(&self) -> bool {
        match self {
            Type::Arithmetic(_) | Type::Pointer(_) | Type::Function(_) => true,
            Type::Void => false,
        }
    }

    pub(crate) fn is_function(&self) -> bool {
        matches!(self, Type::Function(_))
    }

    pub(crate) fn is_scalar(&self) -> bool {
        matches!(
            self,
            Type::Arithmetic(_) | Type::Pointer(_), // TODO: | Type::Nullptr
        )
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

#[derive(Debug, Clone, Copy)]
pub struct QualifiedType<'a> {
    pub(crate) is_const: bool,
    pub(crate) is_volatile: bool,
    pub ty: Type<'a>,
    pub(crate) loc: Loc<'a>,
}

impl<'a> QualifiedType<'a> {
    // TODO: this is a temporary hack until the `Report` derive macro handles stringification
    // better
    pub(crate) fn slice(&self) -> String {
        self.to_string()
    }
}

impl PartialEq for QualifiedType<'_> {
    fn eq(&self, other: &Self) -> bool {
        let Self { is_const, is_volatile, ty, loc: _ } = self;
        let Self {
            is_const: other_is_const,
            is_volatile: other_is_volatile,
            ty: other_ty,
            loc: _,
        } = other;
        (is_const, is_volatile, ty) == (other_is_const, other_is_volatile, other_ty)
    }
}

impl Eq for QualifiedType<'_> {}

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
