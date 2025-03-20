use std::borrow::Cow;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;

use bumpalo::Bump;
use itertools::Itertools as _;
use panko_lex::Loc;
use panko_lex::Token;
use panko_parser::NO_VALUE;
use panko_parser::ast::Arithmetic;
use panko_parser::ast::Integral;
use panko_parser::ast::IntegralKind;
use panko_parser::ast::Signedness;
use panko_parser::sexpr_builder::AsSExpr;
use panko_parser::sexpr_builder::SExpr;
use yansi::Paint as _;

use crate::typecheck::ArrayLength;

pub(crate) mod subobjects;

#[derive(Debug, Clone, Copy)]
pub struct ParameterDeclaration<'a, TypeofExpr, LengthExpr> {
    pub loc: Loc<'a>,
    pub ty: QualifiedType<'a, TypeofExpr, LengthExpr>,
    pub name: Option<Token<'a>>,
}

impl<'a, TypeofExpr, LengthExpr> ParameterDeclaration<'a, TypeofExpr, LengthExpr> {
    pub(crate) fn loc(&self) -> Loc<'a> {
        self.loc
    }

    pub(crate) fn slice(&self) -> Cow<'a, str>
    where
        TypeofExpr: AsSExpr,
        LengthExpr: AsSExpr,
    {
        self.name
            .map(|name| Cow::Borrowed(name.slice()))
            .unwrap_or_else(|| Cow::Owned(format!("<unnamed parameter of type `{}`>", self.ty)))
    }
}

impl<TypeofExpr, LengthExpr> PartialEq for ParameterDeclaration<'_, TypeofExpr, LengthExpr>
where
    TypeofExpr: PartialEq,
    LengthExpr: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.ty.ty == other.ty.ty
    }
}

impl<TypeofExpr, LengthExpr> Eq for ParameterDeclaration<'_, TypeofExpr, LengthExpr>
where
    TypeofExpr: Eq,
    LengthExpr: Eq,
{
}

impl<TypeofExpr, LengthExpr> Hash for ParameterDeclaration<'_, TypeofExpr, LengthExpr>
where
    TypeofExpr: Hash,
    LengthExpr: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ty.ty.hash(state)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ArrayType<'a, TypeofExpr, LengthExpr> {
    pub ty: &'a QualifiedType<'a, TypeofExpr, LengthExpr>,
    pub length: LengthExpr,
}

impl<TypeofExpr, LengthExpr> fmt::Display for ArrayType<'_, TypeofExpr, LengthExpr>
where
    TypeofExpr: AsSExpr,
    LengthExpr: AsSExpr,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { ty, length } = self;
        write!(f, "array<{ty}; {length}>", length = length.as_sexpr())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionType<'a, TypeofExpr, LengthExpr> {
    pub params: &'a [ParameterDeclaration<'a, TypeofExpr, LengthExpr>],
    pub return_type: &'a QualifiedType<'a, TypeofExpr, LengthExpr>,
    pub is_varargs: bool,
}

impl<TypeofExpr, LengthExpr> fmt::Display for FunctionType<'_, TypeofExpr, LengthExpr>
where
    TypeofExpr: AsSExpr,
    LengthExpr: AsSExpr,
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
pub enum Type<'a, TypeofExpr, LengthExpr> {
    Arithmetic(Arithmetic),
    Pointer(&'a QualifiedType<'a, TypeofExpr, LengthExpr>),
    Array(ArrayType<'a, TypeofExpr, LengthExpr>),
    Function(FunctionType<'a, TypeofExpr, LengthExpr>),
    Void,
    Typeof { expr: TypeofExpr, unqual: bool },
    // TODO
}

impl<'a, TypeofExpr, LengthExpr> Type<'a, TypeofExpr, LengthExpr> {
    pub(crate) const fn char() -> Self {
        Self::Arithmetic(Arithmetic::Integral(Integral {
            signedness: Signedness::Signed,
            kind: IntegralKind::PlainChar,
        }))
    }

    pub const fn uchar() -> Self {
        Self::Arithmetic(Arithmetic::Integral(Integral {
            signedness: Signedness::Unsigned,
            kind: IntegralKind::Char,
        }))
    }

    pub const fn ushort() -> Self {
        Self::Arithmetic(Arithmetic::Integral(Integral {
            signedness: Signedness::Unsigned,
            kind: IntegralKind::Short,
        }))
    }

    pub const fn uint() -> Self {
        Self::Arithmetic(Arithmetic::Integral(Integral {
            signedness: Signedness::Unsigned,
            kind: IntegralKind::Int,
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

    pub const fn size_t() -> Self {
        // TODO: this is platform dependent
        Self::ulong()
    }

    fn long() -> Self {
        Self::Arithmetic(Arithmetic::Integral(Integral {
            signedness: Signedness::Signed,
            kind: IntegralKind::Long,
        }))
    }

    const fn ulong() -> Self {
        Self::Arithmetic(Arithmetic::Integral(Integral {
            signedness: Signedness::Unsigned,
            kind: IntegralKind::Long,
        }))
    }

    pub fn unqualified(&self) -> QualifiedType<'a, TypeofExpr, LengthExpr>
    where
        TypeofExpr: Copy,
        LengthExpr: Copy,
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
        LengthExpr: AsSExpr,
    {
        self.to_string()
    }

    pub(crate) fn is_object(&self) -> bool {
        !self.is_function()
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

    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array(_))
    }
}

impl<LengthExpr> Type<'_, !, ArrayLength<LengthExpr>> {
    pub fn size(&self) -> u64 {
        match self {
            Type::Arithmetic(arithmetic) => arithmetic.size(),
            Type::Pointer(_) => 8,
            Type::Array(ArrayType { ty, length }) => {
                let elem_size = ty.ty.size();
                let length = match length {
                    ArrayLength::Constant(length) => *length,
                    ArrayLength::Variable(_) => todo!("`sizeof` of variable length array"),
                    ArrayLength::Unknown => unreachable!(
                        "arrays without a length are incomplete types and don’t have a size"
                    ),
                };
                elem_size
                    .checked_mul(length)
                    .unwrap_or_else(|| todo!("type is too large"))
            }
            Type::Function(_) => unreachable!("functions are not objects and don’t have a size"),
            Type::Void => unreachable!("void is not an object and doesn’t have a size"),
            Type::Typeof { expr, unqual: _ } => match *expr {},
        }
    }

    pub fn align(&self) -> u64 {
        match self {
            Type::Arithmetic(arithmetic) => arithmetic.size(),
            Type::Pointer(_) => 8,
            Type::Array(array_type) => array_type.ty.ty.align(),
            Type::Function(_) =>
                unreachable!("functions are not objects and don’t have an alignment"),
            Type::Void => unreachable!("void is not an object and doesn’t have an alignment"),
            Type::Typeof { expr, unqual: _ } => match *expr {},
        }
    }

    pub(crate) fn is_slot_compatible<E>(&self, ty: &Type<'_, !, ArrayLength<E>>) -> bool {
        // TODO: This is more restrictive than necessary.
        self.size() == ty.size() && self.align() == ty.align()
    }

    pub(crate) fn is_complete(&self) -> bool {
        match self {
            Type::Arithmetic(_) | Type::Pointer(_) | Type::Function(_) => true,
            Type::Array(ArrayType { ty: _, length }) => length.is_known(),
            Type::Void => false,
            Type::Typeof { expr, unqual: _ } => match *expr {},
        }
    }
}

impl<TypeofExpr, LengthExpr> fmt::Display for Type<'_, TypeofExpr, LengthExpr>
where
    TypeofExpr: AsSExpr,
    LengthExpr: AsSExpr,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Arithmetic(Arithmetic::Integral(integral)) => write!(f, "{integral}"),
            Type::Pointer(pointee) => write!(f, "ptr<{pointee}>"),
            Type::Array(array) => write!(f, "{array}"),
            Type::Function(function) => write!(f, "{function}"),
            Type::Void => write!(f, "void"),
            Type::Typeof { .. } => todo!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct QualifiedType<'a, TypeofExpr, LengthExpr> {
    pub(crate) is_const: bool,
    pub(crate) is_volatile: bool,
    pub ty: Type<'a, TypeofExpr, LengthExpr>,
    pub(crate) loc: Loc<'a>,
}

impl<TypeofExpr, LengthExpr> QualifiedType<'_, TypeofExpr, LengthExpr> {
    // TODO: this is a temporary hack until the `Report` derive macro handles stringification
    // better
    pub(crate) fn slice(&self) -> String
    where
        TypeofExpr: AsSExpr,
        LengthExpr: AsSExpr,
    {
        self.to_string()
    }

    pub fn loc(&self) -> Loc {
        self.loc
    }
}

impl<'a, LengthExpr> QualifiedType<'a, !, ArrayLength<LengthExpr>>
where
    LengthExpr: Copy,
{
    pub(crate) fn composite_ty(&self, bump: &'a Bump, other: &Self) -> Option<Self> {
        let Self { is_const, is_volatile, ty, loc } = self;
        let Self {
            is_const: other_is_const,
            is_volatile: other_is_volatile,
            ty: other_ty,
            loc: _,
        } = other;

        if (is_const, is_volatile) != (other_is_const, other_is_volatile) {
            return None;
        }

        let ty = match (ty, other_ty) {
            (
                Type::Array(ArrayType { ty, length }),
                Type::Array(ArrayType { ty: other_ty, length: other_length }),
            ) => {
                let ty = bump.alloc(ty.composite_ty(bump, *other_ty)?);
                let length = match (length, other_length) {
                    (ArrayLength::Constant(length), ArrayLength::Constant(other_length))
                        if length != other_length =>
                        return None,
                    (ArrayLength::Constant(length), _) | (_, ArrayLength::Constant(length)) =>
                        ArrayLength::Constant(*length),
                    (ArrayLength::Variable(length), _) | (_, ArrayLength::Variable(length)) =>
                        ArrayLength::Variable(*length),
                    (ArrayLength::Unknown, ArrayLength::Unknown) => ArrayLength::Unknown,
                };
                Type::Array(ArrayType { ty, length })
            }
            _ if ty == other_ty => *ty,
            _ => return None,
        };

        Some(Self {
            is_const: *is_const,
            is_volatile: *is_volatile,
            ty,
            loc: *loc,
        })
    }
}

impl<TypeofExpr, LengthExpr> PartialEq for QualifiedType<'_, TypeofExpr, LengthExpr>
where
    TypeofExpr: PartialEq,
    LengthExpr: PartialEq,
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

impl<TypeofExpr, LengthExpr> Eq for QualifiedType<'_, TypeofExpr, LengthExpr>
where
    TypeofExpr: Eq,
    LengthExpr: Eq,
{
}

impl<TypeofExpr, LengthExpr> Hash for QualifiedType<'_, TypeofExpr, LengthExpr>
where
    TypeofExpr: Hash,
    LengthExpr: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        let Self { is_const, is_volatile, ty, loc: _ } = self;
        (is_const, is_volatile, ty).hash(state)
    }
}

impl<TypeofExpr, LengthExpr> fmt::Display for QualifiedType<'_, TypeofExpr, LengthExpr>
where
    TypeofExpr: AsSExpr,
    LengthExpr: AsSExpr,
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

impl<TypeofExpr, LengthExpr> AsSExpr for QualifiedType<'_, TypeofExpr, LengthExpr>
where
    TypeofExpr: AsSExpr,
    LengthExpr: AsSExpr,
{
    fn as_sexpr(&self) -> SExpr {
        SExpr::display(&self.italic())
    }
}
