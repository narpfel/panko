use std::borrow::Cow;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;

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
pub struct ParameterDeclaration<'a, TypeofExpr> {
    pub loc: Loc<'a>,
    pub ty: QualifiedType<'a, TypeofExpr>,
    pub name: Option<Token<'a>>,
}

impl<'a, TypeofExpr> ParameterDeclaration<'a, TypeofExpr> {
    pub(crate) fn loc(&self) -> Loc<'a> {
        self.loc
    }

    pub(crate) fn slice(&self) -> Cow<'a, str>
    where
        TypeofExpr: AsSExpr,
    {
        self.name
            .map(|name| Cow::Borrowed(name.slice()))
            .unwrap_or_else(|| Cow::Owned(format!("<unnamed parameter of type `{}`>", self.ty)))
    }
}

impl<TypeofExpr> PartialEq for ParameterDeclaration<'_, TypeofExpr>
where
    TypeofExpr: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.ty.ty == other.ty.ty
    }
}

impl<TypeofExpr> Eq for ParameterDeclaration<'_, TypeofExpr> where TypeofExpr: Eq {}

impl<TypeofExpr> Hash for ParameterDeclaration<'_, TypeofExpr>
where
    TypeofExpr: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ty.ty.hash(state)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionType<'a, TypeofExpr> {
    pub params: &'a [ParameterDeclaration<'a, TypeofExpr>],
    pub return_type: &'a QualifiedType<'a, TypeofExpr>,
    pub is_varargs: bool,
}

impl<TypeofExpr> fmt::Display for FunctionType<'_, TypeofExpr>
where
    TypeofExpr: AsSExpr,
{
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type<'a, TypeofExpr> {
    Arithmetic(Arithmetic),
    Pointer(&'a QualifiedType<'a, TypeofExpr>),
    Function(FunctionType<'a, TypeofExpr>),
    Void,
    Typeof { expr: TypeofExpr, unqual: bool },
    // TODO
}

impl<'a, TypeofExpr> Type<'a, TypeofExpr> {
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

    pub(crate) fn ptrdiff_t() -> Self {
        // TODO: this is platform dependent
        Self::long()
    }

    pub fn size_t() -> Self {
        // TODO: this is platform dependent
        Self::ulong()
    }

    fn long() -> Self {
        Self::Arithmetic(Arithmetic::Integral(Integral {
            signedness: Signedness::Signed,
            kind: IntegralKind::Long,
        }))
    }

    fn ulong() -> Self {
        Self::Arithmetic(Arithmetic::Integral(Integral {
            signedness: Signedness::Unsigned,
            kind: IntegralKind::Long,
        }))
    }

    pub fn unqualified(&self) -> QualifiedType<'a, TypeofExpr>
    where
        TypeofExpr: Copy,
    {
        QualifiedType {
            is_const: false,
            is_volatile: false,
            ty: *self,
            loc: Loc::synthesised(),
        }
    }

    // TODO: this is a temporary hack until the `Report` derive macro handles stringification
    // better
    pub fn slice(&self) -> String
    where
        TypeofExpr: AsSExpr,
    {
        self.to_string()
    }

    pub(crate) fn is_object(&self) -> bool {
        !self.is_function()
    }

    pub(crate) fn is_complete(&self) -> bool {
        match self {
            Type::Arithmetic(_) | Type::Pointer(_) | Type::Function(_) => true,
            Type::Void => false,
            Type::Typeof { .. } => todo!(),
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

impl Type<'_, !> {
    pub fn size(&self) -> u64 {
        match self {
            Type::Arithmetic(arithmetic) => arithmetic.size(),
            Type::Pointer(_) => 8,
            Type::Function(_) => unreachable!("functions are not objects and don’t have a size"),
            Type::Void => unreachable!("void is not an object and doesn’t have a size"),
            Type::Typeof { expr, unqual: _ } => match *expr {},
        }
    }

    pub fn align(&self) -> u64 {
        match self {
            Type::Arithmetic(arithmetic) => arithmetic.size(),
            Type::Pointer(_) => 8,
            Type::Function(_) =>
                unreachable!("functions are not objects and don’t have an alignment"),
            Type::Void => unreachable!("void is not an object and doesn’t have an alignment"),
            Type::Typeof { expr, unqual: _ } => match *expr {},
        }
    }

    pub(crate) fn is_slot_compatible(&self, ty: &Type<'_, !>) -> bool {
        // TODO: This is more restrictive than necessary.
        self.size() == ty.size() && self.align() == ty.align()
    }
}

impl<TypeofExpr> fmt::Display for Type<'_, TypeofExpr>
where
    TypeofExpr: AsSExpr,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Arithmetic(Arithmetic::Integral(integral)) => write!(f, "{integral}"),
            Type::Pointer(pointee) => write!(f, "ptr<{pointee}>"),
            Type::Function(function) => write!(f, "{function}"),
            Type::Void => write!(f, "void"),
            Type::Typeof { .. } => todo!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct QualifiedType<'a, TypeofExpr> {
    pub(crate) is_const: bool,
    pub(crate) is_volatile: bool,
    pub ty: Type<'a, TypeofExpr>,
    pub(crate) loc: Loc<'a>,
}

impl<TypeofExpr> QualifiedType<'_, TypeofExpr> {
    // TODO: this is a temporary hack until the `Report` derive macro handles stringification
    // better
    pub(crate) fn slice(&self) -> String
    where
        TypeofExpr: AsSExpr,
    {
        self.to_string()
    }

    pub fn loc(&self) -> Loc {
        self.loc
    }
}

impl<TypeofExpr> PartialEq for QualifiedType<'_, TypeofExpr>
where
    TypeofExpr: PartialEq,
{
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

impl<TypeofExpr> Eq for QualifiedType<'_, TypeofExpr> where TypeofExpr: Eq {}

impl<TypeofExpr> Hash for QualifiedType<'_, TypeofExpr>
where
    TypeofExpr: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        let Self { is_const, is_volatile, ty, loc: _ } = self;
        (is_const, is_volatile, ty).hash(state)
    }
}

impl<TypeofExpr> fmt::Display for QualifiedType<'_, TypeofExpr>
where
    TypeofExpr: AsSExpr,
{
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

impl<TypeofExpr> AsSExpr for QualifiedType<'_, TypeofExpr>
where
    TypeofExpr: AsSExpr,
{
    fn as_sexpr(&self) -> SExpr {
        SExpr::display(self)
    }
}
