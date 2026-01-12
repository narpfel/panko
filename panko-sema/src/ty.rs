use std::borrow::Cow;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;

use itertools::Itertools as _;
use panko_lex::Bump;
use panko_lex::EncodingPrefix;
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

use crate::fake_trait_impls::HashEqIgnored;
use crate::scope::Id;
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
    pub loc: HashEqIgnored<Loc<'a>>,
}

impl<'a, TypeofExpr, LengthExpr> ArrayType<'a, TypeofExpr, LengthExpr> {
    pub(crate) fn loc(&self) -> Loc<'a> {
        self.loc.0
    }
}

impl<TypeofExpr, LengthExpr> fmt::Display for ArrayType<'_, TypeofExpr, LengthExpr>
where
    TypeofExpr: AsSExpr,
    LengthExpr: AsSExpr,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { ty, length, loc: _ } = self;
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
pub enum Struct<'a, TypeofExpr, LengthExpr> {
    Incomplete {
        name: &'a str,
        id: Id,
    },
    Complete {
        name: Option<&'a str>,
        id: Id,
        members: &'a [Member<'a, TypeofExpr, LengthExpr>],
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Member<'a, TypeofExpr, LengthExpr> {
    pub(crate) name: &'a str,
    pub(crate) ty: QualifiedType<'a, TypeofExpr, LengthExpr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type<'a, TypeofExpr, LengthExpr> {
    Arithmetic(Arithmetic),
    Pointer(&'a QualifiedType<'a, TypeofExpr, LengthExpr>),
    Array(ArrayType<'a, TypeofExpr, LengthExpr>),
    Function(FunctionType<'a, TypeofExpr, LengthExpr>),
    Void,
    Typeof { expr: TypeofExpr, unqual: bool },
    Nullptr,
    Struct(Struct<'a, TypeofExpr, LengthExpr>),
    // TODO
}

impl<'a, TypeofExpr, LengthExpr> Type<'a, TypeofExpr, LengthExpr> {
    pub(crate) const BOOL: Self = Self::bool();

    pub(crate) const fn bool() -> Self {
        Self::Arithmetic(Arithmetic::Integral(Integral {
            signedness: Signedness::Unsigned,
            kind: IntegralKind::Bool,
        }))
    }

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

    pub const fn schar() -> Self {
        Self::Arithmetic(Arithmetic::Integral(Integral {
            signedness: Signedness::Signed,
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

    pub const fn int() -> Self {
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

    pub(crate) const fn char_constant_ty(encoding_prefix: EncodingPrefix) -> Self {
        match encoding_prefix {
            EncodingPrefix::None => Self::int(),
            EncodingPrefix::Utf8 => Self::char8_t(),
            EncodingPrefix::Utf16 => Self::char16_t(),
            EncodingPrefix::Utf32 => Self::char32_t(),
            EncodingPrefix::Wchar => Self::wchar_t(),
        }
    }

    pub(crate) fn escape_sequence_ty(encoding_prefix: EncodingPrefix) -> Self {
        match encoding_prefix {
            EncodingPrefix::None => Type::uchar(),
            EncodingPrefix::Utf8 => Type::char8_t(),
            EncodingPrefix::Utf16 => Type::char16_t(),
            EncodingPrefix::Utf32 => Type::char32_t(),
            EncodingPrefix::Wchar => Type::wchar_t().make_unsigned(),
        }
    }

    pub(crate) const fn char8_t() -> Self {
        Self::uchar()
    }

    pub(crate) const fn char16_t() -> Self {
        Self::ushort()
    }

    pub(crate) const fn char32_t() -> Self {
        Self::uint()
    }

    pub(crate) const fn wchar_t() -> Self {
        Self::int()
    }

    // TODO: this ICE’s for `typeof`, but is only called in `Self::escape_sequence_ty`
    pub(crate) const fn make_unsigned(&self) -> Self {
        match self {
            Self::Arithmetic(Arithmetic::Integral(Integral { signedness: _, kind })) =>
                Self::Arithmetic(Arithmetic::Integral(Integral {
                    signedness: Signedness::Unsigned,
                    kind: *kind,
                })),
            _ => panic!("cannot make self unsigned (not an integral type)"),
        }
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
            loc: HashEqIgnored(Loc::synthesised()),
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
}

impl<LengthExpr> Type<'_, !, ArrayLength<LengthExpr>> {
    pub fn is_object(&self) -> bool {
        !self.is_function()
    }

    pub(crate) fn is_function(&self) -> bool {
        matches!(self, Type::Function(_))
    }

    pub(crate) fn is_scalar(&self) -> bool {
        matches!(self, Type::Arithmetic(_) | Type::Pointer(_) | Type::Nullptr)
    }

    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array(_))
    }

    pub fn size(&self) -> u64 {
        match self {
            Type::Arithmetic(arithmetic) => arithmetic.size(),
            Type::Pointer(_) => Self::size_t().size(),
            Type::Array(ArrayType { ty, length, loc: _ }) => {
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
            Type::Nullptr => Self::size_t().size(),
            Type::Struct(Struct::Incomplete { name: _, id: _ }) => unreachable!("incomplete"),
            Type::Struct(Struct::Complete { name: _, id: _, members: _ }) =>
                todo!("sizeof complete struct"),
        }
    }

    pub fn align(&self) -> u64 {
        match self {
            Type::Arithmetic(arithmetic) => arithmetic.size(),
            Type::Pointer(_) => Self::size_t().align(),
            Type::Array(array_type) => array_type.ty.ty.align(),
            Type::Function(_) =>
                unreachable!("functions are not objects and don’t have an alignment"),
            Type::Void => unreachable!("void is not an object and doesn’t have an alignment"),
            Type::Typeof { expr, unqual: _ } => match *expr {},
            Type::Nullptr => Self::size_t().align(),
            Type::Struct(Struct::Incomplete { name: _, id: _ }) => unreachable!("incomplete"),
            Type::Struct(Struct::Complete { name: _, id: _, members: _ }) =>
                todo!("alignof complete struct"),
        }
    }

    pub(crate) fn is_slot_compatible<E>(&self, ty: &Type<'_, !, ArrayLength<E>>) -> bool {
        // TODO: This is more restrictive than necessary.
        self.size() == ty.size() && self.align() == ty.align()
    }

    pub(crate) fn is_complete(&self) -> bool {
        match self {
            Type::Arithmetic(_) | Type::Pointer(_) | Type::Function(_) => true,
            Type::Array(ArrayType { ty, length, loc: _ }) =>
                length.is_known() && ty.ty.is_complete(),
            Type::Void => false,
            Type::Typeof { expr, unqual: _ } => match *expr {},
            Type::Nullptr => true,
            Type::Struct(Struct::Incomplete { name: _, id: _ }) => false,
            Type::Struct(Struct::Complete { name: _, id: _, members: _ }) =>
                todo!("is_complete for complete struct"),
        }
    }

    pub(crate) fn can_represent<T>(&self, value: T) -> bool
    where
        i8: TryFrom<T>,
        i16: TryFrom<T>,
        i32: TryFrom<T>,
        i64: TryFrom<T>,
        u8: TryFrom<T>,
        u16: TryFrom<T>,
        u32: TryFrom<T>,
        u64: TryFrom<T>,
    {
        match self {
            Self::Arithmetic(Arithmetic::Integral(integral)) => integral.can_represent(value),
            _ => false,
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
            Type::Typeof { expr, unqual } => write!(
                f,
                "typeof{}({})",
                if *unqual { "_unqual" } else { "" },
                expr.as_sexpr(),
            ),
            Type::Nullptr => write!(f, "nullptr_t"),
            Type::Struct(Struct::Incomplete { name, id }) =>
                write!(f, "struct {name}~{id}", id = id.0),
            Type::Struct(Struct::Complete { name, id, members: _ }) =>
                write!(f, "struct {}~{} complete", name.as_sexpr(), id.0),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct QualifiedType<'a, TypeofExpr, LengthExpr> {
    pub(crate) is_const: bool,
    pub(crate) is_volatile: bool,
    pub ty: Type<'a, TypeofExpr, LengthExpr>,
    pub(crate) loc: HashEqIgnored<Loc<'a>>,
}

impl<'a, TypeofExpr, LengthExpr> QualifiedType<'a, TypeofExpr, LengthExpr> {
    // TODO: this is a temporary hack until the `Report` derive macro handles stringification
    // better
    pub(crate) fn slice(&self) -> String
    where
        TypeofExpr: AsSExpr,
        LengthExpr: AsSExpr,
    {
        self.to_string()
    }

    pub fn loc(&self) -> Loc<'a> {
        self.loc.0
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
                Type::Array(ArrayType { ty, length, loc }),
                Type::Array(ArrayType {
                    ty: other_ty,
                    length: other_length,
                    loc: _,
                }),
            ) => {
                let ty = bump.alloc(ty.composite_ty(bump, other_ty)?);
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
                Type::Array(ArrayType { ty, length, loc: *loc })
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
