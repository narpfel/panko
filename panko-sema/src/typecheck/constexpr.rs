use std::assert_matches;
use std::bstr::ByteStr;
use std::collections::LinkedList;
use std::iter::once;
use std::ops::Range;

use itertools::Either;
use itertools::Itertools as _;
use itertools::zip_eq;
use panko_parser::BinOpKind;
use panko_parser::Comparison;
use panko_parser::LogicalOpKind;
use panko_parser::ast::Arithmetic;
use panko_parser::ast::Session;
use panko_parser::ast::Signedness;
use panko_report::Report;

use crate::scope::BuiltinName;
use crate::scope::BuiltinNameKind;
use crate::scope::StorageDuration;
use crate::ty::subobjects::Subobject;
use crate::typecheck::Bitfield;
use crate::typecheck::Declaration;
use crate::typecheck::Expression;
use crate::typecheck::Initialiser;
use crate::typecheck::InitialiserRef;
use crate::typecheck::MemberKind;
use crate::typecheck::Reference;
use crate::typecheck::SubobjectInitialiser;
use crate::typecheck::Type;
use crate::typecheck::TypedExpression;
use crate::typecheck::constexpr::arithmetic::C;
use crate::typecheck::constexpr::diagnostics::Diagnostic;
use crate::typecheck::constexpr::diagnostics::Kind::*;
use crate::typecheck::literal::StringLiteral;

mod arithmetic;
mod diagnostics;

enum ConversionKind {
    Truncate,
    ZeroExtend,
    SignExtend,
    Noop,
    Bool,
}

#[derive(Debug, Clone, Default)]
pub(super) struct Errors<'a>(LinkedList<Either<Diagnostic<'a>, &'a dyn Report>>);

impl<'a> Errors<'a> {
    fn new(diagnostic: Diagnostic<'a>) -> Self {
        Self(LinkedList::from_iter([Either::Left(diagnostic)]))
    }

    fn chain(self, Self(mut other_errors): Self) -> Self {
        let Self(mut errors) = self;
        errors.append(&mut other_errors);
        Self(errors)
    }
}

impl<'a> IntoIterator for Errors<'a> {
    type IntoIter = std::collections::linked_list::IntoIter<Self::Item>;
    type Item = Either<Diagnostic<'a>, &'a dyn Report>;

    fn into_iter(self) -> Self::IntoIter {
        let Self(errors) = self;
        errors.into_iter()
    }
}

enum Integral {
    Signed(i64),
    Unsigned(u64),
}

#[derive(Debug)]
pub(super) struct Value<'a, 'b> {
    expr: &'b TypedExpression<'a>,
    repr: Repr<'a>,
}

#[derive(Debug)]
enum Repr<'a> {
    Bytes(Box<[Byte<'a>]>),
    Error(Errors<'a>),
}

#[derive(Debug, Clone, Copy)]
enum Byte<'a> {
    Literal(u8),
    Address {
        index: u64,
        target: Target<'a>,
        // TODO: this should be bounded by the `reference`’s type’s size
        offset: u64,
    },
}

#[derive(Debug, Clone, Copy)]
enum Target<'a> {
    Reference(&'a Reference<'a>),
    String(&'a ByteStr),
}

impl<'a> Target<'a> {
    fn persist(self) -> PersistedTarget<'a, Reference<'a>> {
        match self {
            Self::Reference(reference) => PersistedTarget::Reference(*reference),
            Self::String(string) => PersistedTarget::String(string),
        }
    }
}

impl PartialEq for Target<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Reference(reference), Self::Reference(other_reference)) =>
                reference.id == other_reference.id,
            (Self::Reference(_), Self::String(_)) | (Self::String(_), Self::Reference(_)) => false,
            (Self::String(byte_str), Self::String(other_byte_str)) => byte_str == other_byte_str,
        }
    }
}

impl Eq for Target<'_> {}

fn iter_literal_bytes(bytes: &[Byte]) -> impl Iterator<Item = Option<u8>> {
    bytes.iter().map(|b| match b {
        Byte::Literal(b) => Some(*b),
        Byte::Address { .. } => None,
    })
}

fn read_literal_bytes<const N: usize>(bytes: &[Byte]) -> Option<[u8; N]> {
    iter_literal_bytes(bytes).collect_array()?.transpose()
}

fn read_u64(bytes: &[Byte]) -> Option<u64> {
    Some(u64::from_le_bytes(read_literal_bytes(bytes)?))
}

fn write_u64<'a>(value: u64) -> Repr<'a> {
    Repr::Bytes(Box::new(value.to_le_bytes().map(Byte::Literal)))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct Address<'a> {
    target: Target<'a>,
    offset: u64,
}

impl<'a> Address<'a> {
    fn into_bytes(self) -> impl Iterator<Item = Byte<'a>> {
        let Self { target, offset } = self;
        (0..8).map(move |index| Byte::Address { index, target, offset })
    }
}

fn read_address<'a>(bytes: &[Byte<'a>]) -> Option<Address<'a>> {
    let address = match *bytes.first()? {
        Byte::Address { index: 0, target, offset } => Address { target, offset },
        _ => return None,
    };

    let (index, address) = bytes
        .iter()
        .try_fold(
            (0, address),
            |(prev_index, expected_address), byte| match *byte {
                Byte::Literal(_) => None,
                Byte::Address { index, target, offset } => {
                    let address = Address { target, offset };
                    match index == prev_index && address == expected_address {
                        true => Some((index.strict_add(1), address)),
                        false => None,
                    }
                }
            },
        )
        .unzip();

    match index {
        Some(8) => address,
        _ => None,
    }
}

fn write_address<'a>(address: Address<'a>) -> Repr<'a> {
    Repr::Bytes(address.into_bytes().collect())
}

pub(super) enum Pointer<'a> {
    FromInt(Result<u64, Errors<'a>>),
    Address(Address<'a>),
}

impl<'a> Pointer<'a> {
    fn with_value(self, value: u64) -> Result<u64, Errors<'a>> {
        match self {
            Self::FromInt(Ok(_)) | Self::Address(Address { .. }) => Ok(value),
            Self::FromInt(Err(errors)) => Err(errors),
        }
    }
}

impl<'a, 'b> Value<'a, 'b> {
    fn into_integral(self) -> Result<Integral, Errors<'a>> {
        let Self { expr, repr: _ } = &self;
        let signedness = match expr.ty.ty {
            Type::Arithmetic(Arithmetic::Integral(integral)) => integral.signedness,
            _ => todo!("type error"),
        };
        let ty = match signedness {
            Signedness::Unsigned => Type::ULONG,
            Signedness::Signed => Type::LONG,
        };
        let expr = TypedExpression { ty: ty.unqualified(), ..**expr };
        let Value { expr: _, repr } = self.convert(&expr, ConversionKind::SignExtend);
        let bytes = read_literal_bytes(&repr.into_bytes()?)
            .expect("`convert` only generates `Repr`s that only contain `Literal`s");
        match signedness {
            Signedness::Signed => Ok(Integral::Signed(i64::from_le_bytes(bytes))),
            Signedness::Unsigned => Ok(Integral::Unsigned(u64::from_le_bytes(bytes))),
        }
    }

    pub(super) fn into_unsigned(self) -> Result<Result<u64, i64>, Errors<'a>> {
        match self.into_integral()? {
            Integral::Unsigned(value) => Ok(Ok(value)),
            Integral::Signed(value) => Ok(u64::try_from(value).map_err(|_| value)),
        }
    }

    pub(super) fn into_pointer(self) -> Pointer<'a> {
        let Self { expr, repr } = self;
        assert_matches!(expr.ty.ty, Type::Pointer(_) | Type::Nullptr);
        match repr {
            Repr::Bytes(bytes) if let Some(value) = read_u64(&bytes) => Pointer::FromInt(Ok(value)),
            Repr::Bytes(bytes) if let Some(addr) = read_address(&bytes) => Pointer::Address(addr),
            Repr::Bytes(_) => unreachable!("mixing `Literal` and `Address` bytes is invalid"),
            Repr::Error(errors) => Pointer::FromInt(Err(errors)),
        }
    }

    fn error(expr: &'b TypedExpression<'a>) -> Self {
        Self {
            expr,
            repr: Repr::Error(Errors::default()),
        }
    }

    fn with_errors(expr: &'b TypedExpression<'a>, errors: Errors<'a>) -> Self {
        Self { expr, repr: Repr::Error(errors) }
    }

    fn int(value: u64, expr: &'b TypedExpression<'a>) -> Self {
        let ty = expr.ty.ty;
        assert!(ty.can_represent(value), "`{ty}`.can_represent({value})");
        match ty {
            Type::Arithmetic(Arithmetic::Integral(integral)) => {
                let size = usize::try_from(integral.size()).unwrap();
                let bytes = value.to_le_bytes().map(Byte::Literal)[..size].into();
                let repr = Repr::Bytes(bytes);
                Self { expr, repr }
            }
            _ => todo!("error: invalid type for integral constexpr value"),
        }
    }

    fn convert<'c>(self, expr: &'c TypedExpression<'a>, kind: ConversionKind) -> Value<'a, 'c> {
        let Self { expr: old_expr, repr } = self;
        let ty = old_expr.ty.ty;
        type Kind = ConversionKind;
        match kind {
            _ if let Repr::Error(_) = repr => Value { expr, repr },
            // TODO: check for illegal pointer conversions
            Kind::Noop => Value { expr, repr },
            Kind::Bool => {
                let is_nonzero = match repr {
                    Repr::Bytes(bytes) => bytes.iter().any(|&b| match b {
                        Byte::Literal(b) => b != 0,
                        Byte::Address { .. } => true,
                    }),
                    Repr::Error(_) => unreachable!(),
                };
                Value::int(is_nonzero.into(), expr)
            }
            Kind::Truncate | Kind::ZeroExtend | Kind::SignExtend => match ty {
                Type::Arithmetic(Arithmetic::Integral(integral)) => {
                    let mut bytes =
                        iter_literal_bytes(&repr.into_bytes().expect("repr is not `Repr::Error`"))
                            .collect::<Option<Vec<_>>>()
                            .expect("TODO: error message: invalid ptr2int cast, e. g. `(int)(size_t)&static_var`");
                    let fill_value = match integral.signedness {
                        Signedness::Signed if let ConversionKind::SignExtend = kind =>
                            match bytes.last().unwrap().cast_signed() < 0 {
                                true => 0xff,
                                false => 0,
                            },
                        _ => 0,
                    };
                    bytes.resize(usize::try_from(expr.ty.ty.size()).unwrap(), fill_value);
                    let repr = Repr::Bytes(bytes.into_iter().map(Byte::Literal).collect());
                    Value { expr, repr }
                }
                _ => todo!("error message, e. g. `static int x = (int)&static_var;`"),
            },
        }
    }

    fn persist(self, sess: &Session<'a>) -> PersistedValue<'a, Reference<'a>> {
        match self.repr {
            Repr::Bytes(bytes) => persist(sess, bytes),
            Repr::Error(errors) => {
                let errors =
                    sess.alloc_slice_fill_iter(errors.into_iter().map(|error| match error {
                        Either::Left(diagnostic) => sess.emit(diagnostic),
                        Either::Right(diagnostic) => diagnostic,
                    }));
                PersistedValue::Error(errors)
            }
        }
    }
}

macro_rules! impl_as_ty {
    ($pattern:pat => $t:ident + $l:lifetime) => {
        fn ${concat(as_, $t)}(&self) -> Option<Result<$t, Errors<$l>>> {
            let Self { expr, repr } = self;
            match expr.ty.ty {
                $pattern => Some(match repr {
                    Repr::Bytes(bytes) => Ok($t::from_le_bytes(read_literal_bytes(bytes)?)),
                    Repr::Error(errors) => Err(errors.clone()),
                }),
                _ => None,
            }
        }
    };
}

impl<'a> Value<'a, '_> {
    impl_as_ty!(Type::ULLONG | Type::ULONG => u64 + 'a);

    impl_as_ty!(Type::LLONG | Type::LONG => i64 + 'a);

    impl_as_ty!(Type::UINT => u32 + 'a);

    impl_as_ty!(Type::INT => i32 + 'a);
}

impl<'a> Repr<'a> {
    fn empty(ty: &Type<'a>) -> Self {
        let bytes = vec![Byte::Literal(0); usize::try_from(ty.size()).unwrap()].into_boxed_slice();
        Repr::Bytes(bytes)
    }

    fn into_bytes(self) -> Result<Box<[Byte<'a>]>, Errors<'a>> {
        match self {
            Self::Bytes(bytes) => Ok(bytes),
            Self::Error(errors) => Err(errors),
        }
    }

    fn get(self, indices: Range<u64>) -> Self {
        let Range { start, end } = indices;
        let indices = usize::try_from(start).unwrap()..usize::try_from(end).unwrap();
        match self {
            Self::Bytes(bytes) => Self::Bytes(bytes[indices].into()),
            Self::Error(errors) => Self::Error(errors),
        }
    }
}

trait IntoErrors<'a> {
    fn into_errors(self) -> Option<Errors<'a>>;
}

impl<'a> IntoErrors<'a> for Value<'a, '_> {
    fn into_errors(self) -> Option<Errors<'a>> {
        match self.repr {
            Repr::Error(errors) => Some(errors),
            _ => None,
        }
    }
}

impl<'a> IntoErrors<'a> for Pointer<'a> {
    fn into_errors(self) -> Option<Errors<'a>> {
        match self {
            Pointer::FromInt(Err(errors)) => Some(errors),
            Pointer::FromInt(Ok(_)) | Pointer::Address(Address { .. }) => None,
        }
    }
}

impl<'a> IntoErrors<'a> for &TypedExpression<'a> {
    fn into_errors(self) -> Option<Errors<'a>> {
        eval(self).into_errors()
    }
}

fn gather_errors<'a>(values: impl IntoIterator<Item = impl IntoErrors<'a>>) -> Errors<'a> {
    values
        .into_iter()
        .filter_map(IntoErrors::into_errors)
        .fold(Errors::default(), Errors::chain)
}

fn no_errors<'a, 'b>() -> [Value<'a, 'b>; 0] {
    []
}

fn not_constexpr<'a, 'b>(
    expr: &'b TypedExpression<'a>,
    values: impl IntoIterator<Item = impl IntoErrors<'a>>,
) -> Value<'a, 'b> {
    let errors = gather_errors(values).chain(Errors::new(Diagnostic::NotConstexpr { at: *expr }));
    Value::with_errors(expr, errors)
}

fn as_bool<'a>(expr: &TypedExpression<'a>) -> Result<bool, Errors<'a>> {
    let bool_expr = TypedExpression { ty: Type::BOOL.unqualified(), ..*expr };
    let value = eval(expr).convert(&bool_expr, ConversionKind::Bool);
    Ok(value.into_unsigned()?.expect("`bool` is unsigned") != 0)
}

fn eval_pointer_arithmetic<'a, 'b>(
    expr: &'b TypedExpression<'a>,
    pointer: &TypedExpression<'a>,
    integral: Value<'a, '_>,
    pointee_size: u64,
    op: impl FnOnce(u64, i64) -> Option<u64>,
) -> Value<'a, 'b> {
    let integral = integral.as_i64().expect("not a type error");
    let pointee_size = pointee_size
        .checked_cast_signed()
        .ok_or_else(|| SizeOverflow.at(pointer));
    let integral = arithmetic::binop(integral, pointee_size, i64::checked_mul, || {
        SizeOverflow.at(expr)
    });
    let add = |base| arithmetic::binop(base, integral, op, || PointerAdditionOverflow.at(expr));

    let repr = match eval(pointer).into_pointer() {
        Pointer::FromInt(pointer) => add(pointer).map(write_u64),
        Pointer::Address(Address { target, offset }) =>
            add(Ok(offset)).map(|offset| write_address(Address { target, offset })),
    }
    .unwrap_or_else(Repr::Error);

    Value { expr, repr }
}

enum PointerBinopKind {
    Equality,
    Other,
}

fn eval_pointer_binop<'a, 'b>(
    typed_expr: &'b TypedExpression<'a>,
    kind: PointerBinopKind,
    lhs: &TypedExpression<'a>,
    rhs: &TypedExpression<'a>,
    compute: impl FnOnce(Result<u64, Errors<'a>>, Result<u64, Errors<'a>>) -> Value<'a, 'b>,
) -> Value<'a, 'b> {
    let lhs = eval(lhs);
    let rhs = eval(rhs);

    match (lhs.into_pointer(), rhs.into_pointer()) {
        (Pointer::FromInt(lhs), Pointer::FromInt(rhs)) => compute(lhs, rhs),

        (
            Pointer::Address(Address {
                target: Target::Reference(reference),
                offset,
            }),
            Pointer::Address(Address {
                target: Target::Reference(rhs_ref),
                offset: rhs_offset,
            }),
        ) if reference.id == rhs_ref.id => compute(Ok(offset), Ok(rhs_offset)),

        (
            Pointer::Address(Address { target: Target::String(string), offset }),
            Pointer::Address(Address {
                target: Target::String(rhs_string),
                offset: rhs_offset,
            }),
        ) if string == rhs_string => compute(Ok(offset), Ok(rhs_offset)),

        (lhs, rhs) => match kind {
            PointerBinopKind::Equality => compute(lhs.with_value(0), rhs.with_value(1)),
            PointerBinopKind::Other => not_constexpr(typed_expr, [lhs, rhs]),
        },
    }
}

fn eval_static_initialiser<'a, 'b>(
    expr: &'b TypedExpression<'a>,
    initialiser: &'b Initialiser<'a>,
) -> Value<'a, 'b> {
    let ty = expr.ty.ty;
    match initialiser {
        Initialiser::Braced { subobject_initialisers } => {
            let mut repr = Repr::empty(&ty);
            for SubobjectInitialiser { subobject, initialiser } in *subobject_initialisers {
                let Subobject { ty, offset, kind } = *subobject;
                let subobject_size = usize::try_from(ty.size()).unwrap();
                let offset = usize::try_from(offset).unwrap();
                let value = eval(initialiser);
                match &mut repr {
                    Repr::Bytes(bytes) => match kind {
                        MemberKind::Normal => match value.repr {
                            Repr::Bytes(subobject) =>
                                bytes[offset..][..subobject_size].copy_from_slice(&subobject),
                            Repr::Error(errors) => repr = Repr::Error(errors),
                        },
                        MemberKind::Bitfield(Bitfield { offset: bitfield_offset, width: _ }) => {
                            let value = match value.into_integral() {
                                Ok(Integral::Signed(value)) => value.cast_unsigned(),
                                Ok(Integral::Unsigned(value)) => value,
                                Err(errors) => {
                                    repr = Repr::Error(errors);
                                    continue;
                                }
                            };
                            for (tgt_byte, src_byte) in zip_eq(
                                &mut bytes[offset..][..subobject_size],
                                &(value << bitfield_offset).to_le_bytes()[..subobject_size],
                            ) {
                                match tgt_byte {
                                    Byte::Literal(tgt_byte) => *tgt_byte |= src_byte,
                                    Byte::Address { .. } => unreachable!(
                                        "bitfields can only overlap with other bitfields, which never have `Address` repr"
                                    ),
                                }
                            }
                        }
                    },
                    Repr::Error(errors) =>
                        repr = Repr::Error(std::mem::take(errors).chain(gather_errors([value]))),
                }
            }
            Value { expr, repr }
        }
        Initialiser::Expression(expr) => eval(expr),
        Initialiser::Static { initialiser: _, value } => {
            let repr = match value {
                PersistedValue::Chunks { chunks } => {
                    let bytes = chunks.iter().flat_map(|chunk| match chunk {
                        Chunk::Literal(bytes) =>
                            Either::Left(bytes.iter().copied().map(Byte::Literal)),
                        Chunk::Address { target, offset } => {
                            let target = match target {
                                PersistedTarget::Reference(reference) =>
                                    Target::Reference(reference),
                                PersistedTarget::String(string) => Target::String(string),
                            };
                            Either::Right(Address { target, offset: *offset }.into_bytes())
                        }
                    });
                    Repr::Bytes(bytes.collect())
                }
                PersistedValue::Error(reports) => Repr::Error(Errors(LinkedList::from_iter(
                    reports.iter().copied().map(Either::Right),
                ))),
            };
            Value { expr, repr }
        }
    }
}

fn eval_string_literal<'a, 'b>(
    expr: &'b TypedExpression<'a>,
    string_literal: &StringLiteral<'a>,
) -> Value<'a, 'b> {
    Value {
        expr,
        repr: Repr::Bytes(
            string_literal
                .value()
                .iter()
                .copied()
                .map(Byte::Literal)
                .collect(),
        ),
    }
}

fn pointer_to_string_literal<'a, 'b>(
    expr: &'b TypedExpression<'a>,
    string: &'a ByteStr,
) -> Value<'a, 'b> {
    let repr = write_address(Address {
        target: Target::String(string),
        offset: 0,
    });
    Value { expr, repr }
}

pub(super) fn eval<'a, 'b>(typed_expr: &'b TypedExpression<'a>) -> Value<'a, 'b> {
    let TypedExpression { ty, expr } = typed_expr;
    let ty = ty.ty;

    macro_rules! unary {
        ($operand:expr, $meth:ident, $($t:ident),*) => {{
            let operand = $operand;
            match () {
                $(
                    () if let Some(operand) = operand.${concat(as_, $t)}() =>
                        operand.$meth(typed_expr).into_value(typed_expr),
                )*
                () => todo!(),
            }
        }};
    }

    match expr {
        Expression::Error(_) => Value::error(typed_expr),

        Expression::Parenthesised { open_paren: _, expr, close_paren: _ } => eval(expr),

        Expression::Integer { value, token: _ }
        | Expression::Sizeof { sizeof: _, operand: _, size: value }
        | Expression::Lengthof { lengthof: _, operand: _, length: value }
        | Expression::SizeofTy {
            sizeof: _,
            ty: _,
            size: value,
            close_paren: _,
        }
        | Expression::LengthofTy {
            lengthof: _,
            ty: _,
            length: value,
            close_paren: _,
        }
        | Expression::Alignof {
            alignof: _,
            ty: _,
            align: value,
            close_paren: _,
        } => Value::int(*value, typed_expr),

        Expression::String(string_literal) => eval_string_literal(typed_expr, string_literal),

        Expression::Nullptr(_) => Value {
            expr: typed_expr,
            repr: Repr::Bytes([Byte::Literal(0); 8].into()),
        },

        Expression::NoopTypeConversion(expr) =>
            eval(expr).convert(typed_expr, ConversionKind::Noop),
        Expression::Truncate(expr) => eval(expr).convert(typed_expr, ConversionKind::Truncate),
        Expression::SignExtend(expr) => eval(expr).convert(typed_expr, ConversionKind::SignExtend),
        Expression::ZeroExtend(expr) => eval(expr).convert(typed_expr, ConversionKind::ZeroExtend),
        Expression::BoolCast(expr) => eval(expr).convert(typed_expr, ConversionKind::Bool),

        Expression::VoidCast(expr) => {
            let errors = gather_errors([eval(expr)]);
            match errors.0.is_empty() {
                true => Value {
                    expr: typed_expr,
                    repr: Repr::Bytes(Box::new([])),
                },
                false => Value::with_errors(typed_expr, errors),
            }
        }

        Expression::IntegralBinOp { ty: _, lhs, op, rhs: rhs_expr } => {
            let lhs = eval(lhs);
            let rhs = eval(rhs_expr);
            macro_rules! binary {
                ($($t:ident),*) => {
                    match () {
                        $(
                            ()
                                if let BinOpKind::LeftShift | BinOpKind::RightShift = op.kind
                                && let Some(lhs) = lhs.${concat(as_, $t)}() => {
                                    let rhs = try {
                                        let rhs = rhs.into_unsigned()?;
                                        let rhs = rhs.map_err(|_| NegativeShiftRhs.at(rhs_expr))?;
                                        match rhs.try_into() {
                                            Ok(rhs @ 0..$t::BITS) => rhs,
                                            _ => Err(ShiftRhsOutOfRange.at(rhs_expr))?,
                                        }
                                    };
                                    let result = match op.kind {
                                        BinOpKind::LeftShift => lhs.shl(rhs, typed_expr),
                                        BinOpKind::RightShift => lhs.shr(rhs),
                                        _ => unreachable!(),
                                    };
                                    result.into_value(typed_expr)
                                }
                        )*
                        $(
                            ()
                                if let Some(lhs) = lhs.${concat(as_, $t)}()
                                && let Some(rhs) = rhs.${concat(as_, $t)}() => {
                                    let result = match op.kind {
                                        BinOpKind::Multiply => lhs.mul(rhs, typed_expr),
                                        BinOpKind::Divide => lhs.div(rhs.nonzero(rhs_expr), typed_expr),
                                        BinOpKind::Modulo => lhs.rem(rhs.nonzero(rhs_expr), typed_expr),
                                        BinOpKind::Add => lhs.add(rhs, typed_expr),
                                        BinOpKind::Subtract => lhs.sub(rhs, typed_expr),
                                        BinOpKind::LeftShift | BinOpKind::RightShift =>
                                            unreachable!(),
                                        BinOpKind::BitAnd => lhs.bitand(rhs),
                                        BinOpKind::BitXor => lhs.bitxor(rhs),
                                        BinOpKind::BitOr => lhs.bitor(rhs),
                                        BinOpKind::Comparison(comparison) =>
                                            return lhs.compare(comparison, rhs).into_value(typed_expr),
                                    };
                                    result.into_value(typed_expr)
                                }
                        )*
                        () => todo!(),
                    }
                };
            }
            binary!(u64, i64, u32, i32)
        }

        Expression::Negate { minus: _, operand } => unary!(eval(operand), neg, u32, i32, u64, i64),
        Expression::Compl { compl: _, operand } => unary!(eval(operand), compl, u32, i32, u64, i64),
        Expression::Not { not: _, operand } => {
            assert_eq!(ty, Type::int());
            let operand = eval(operand).convert(typed_expr, ConversionKind::Bool);
            unary!(operand, not, i32)
        }

        Expression::Addressof { ampersand, operand } => match &operand.expr {
            Expression::Parenthesised { open_paren: _, expr, close_paren: _ } => {
                let Value { expr: _, repr } = eval(&TypedExpression {
                    expr: Expression::Addressof { ampersand: *ampersand, operand: expr },
                    ..*typed_expr
                });
                Value { expr: typed_expr, repr }
            }
            Expression::Name(reference)
            | Expression::CompoundLiteral {
                open_paren: _,
                decl: Declaration { reference, initialiser: _ },
            } => match reference.storage_duration {
                StorageDuration::Static(_) => Value {
                    expr: typed_expr,
                    repr: write_address(Address {
                        target: Target::Reference(reference),
                        offset: 0,
                    }),
                },
                StorageDuration::Automatic => not_constexpr(typed_expr, no_errors()),
            },
            Expression::Deref { star: _, operand } => eval(operand),
            Expression::MemberAccess { lhs, member, member_loc: _ } => {
                let pointer = &TypedExpression {
                    ty: Type::Pointer(&lhs.ty).unqualified(),
                    expr: Expression::Addressof { ampersand: *ampersand, operand: lhs },
                };
                let member_expr = TypedExpression {
                    ty: Type::ptrdiff_t().unqualified(),
                    ..*typed_expr
                };
                let offset = Value::int(member.offset, &member_expr);
                eval_pointer_arithmetic(typed_expr, pointer, offset, 1, u64::checked_add_signed)
            }
            Expression::String(string) => pointer_to_string_literal(typed_expr, string.value()),
            Expression::BuiltinName(BuiltinName {
                kind: BuiltinNameKind::Func(name),
                loc: _,
            }) => pointer_to_string_literal(typed_expr, name),
            _ => unreachable!("there are no other expression kinds that are valid as lvalues"),
        },

        Expression::PtrAdd {
            pointer,
            integral,
            pointee_size,
            order: _,
        } => eval_pointer_arithmetic(
            typed_expr,
            pointer,
            eval(integral),
            *pointee_size,
            u64::checked_add_signed,
        ),

        Expression::PtrSub { pointer, integral, pointee_size } => eval_pointer_arithmetic(
            typed_expr,
            pointer,
            eval(integral),
            *pointee_size,
            u64::checked_sub_signed,
        ),

        Expression::PtrDiff { lhs, rhs, pointee_size } =>
            eval_pointer_binop(typed_expr, PointerBinopKind::Other, lhs, rhs, |lhs, rhs| {
                let difference = arithmetic::binop(lhs, rhs, u64::checked_signed_diff, || {
                    PointerDiffOverflow.at(typed_expr)
                });
                let pointee_size = pointee_size
                    .checked_cast_signed()
                    .ok_or_else(|| SizeOverflow.at(typed_expr));
                // TODO: this does not catch cases where both pointers are misaligned by the same
                // amount, e. g. `(int*)5 - (int*)1`
                let result: Result<i64, _> =
                    arithmetic::binop(difference, pointee_size, i64::div_exact, || {
                        UnalignedPointer.at(typed_expr)
                    });
                result.into_value(typed_expr)
            }),

        Expression::PtrCmp { lhs, kind, rhs } => {
            let binop_kind = match kind {
                Comparison::Equal | Comparison::NotEqual => PointerBinopKind::Equality,
                _ => PointerBinopKind::Other,
            };
            eval_pointer_binop(typed_expr, binop_kind, lhs, rhs, |lhs, rhs| {
                lhs.compare(*kind, rhs).into_value(typed_expr)
            })
        }

        // TODO: `constexpr` storage class
        Expression::Name(_) => not_constexpr(typed_expr, no_errors()),

        Expression::Assign { target, value } => not_constexpr(typed_expr, [*target, *value]),

        Expression::Call {
            callee,
            args,
            is_varargs: _,
            close_paren: _,
        } => not_constexpr(typed_expr, once(*callee).chain(*args)),

        Expression::Combine { first, second } => {
            let first = eval(first);
            let second = eval(second);
            let errors = gather_errors([first]);
            match errors.0.is_empty() {
                true => second,
                false => Value::with_errors(typed_expr, errors.chain(gather_errors([second]))),
            }
        }

        Expression::Logical { lhs, op, rhs } => {
            let result = match op.kind() {
                LogicalOpKind::And => try { as_bool(lhs)? && as_bool(rhs)? },
                LogicalOpKind::Or => try { as_bool(lhs)? || as_bool(rhs)? },
            };
            result.map(i32::from).into_value(typed_expr)
        }

        Expression::Conditional { condition, then, or_else } => match as_bool(condition) {
            Ok(true) => eval(then),
            Ok(false) => eval(or_else),
            Err(errors) => Value::with_errors(typed_expr, errors),
        },

        Expression::CompoundLiteral { open_paren: _, decl } => {
            let Declaration { reference: _, initialiser } = decl;
            let initialiser = initialiser
                .as_ref()
                .expect("compound literals always have initialisers");
            eval_static_initialiser(typed_expr, initialiser)
        }

        Expression::MemberAccess { lhs, member, member_loc: _ } => {
            assert_eq!(ty, member.ty.ty);
            let lhs = eval(lhs);
            let size = ty.size();
            let indices = member.offset..member.offset.strict_add(size);
            let repr = lhs.repr.get(indices);
            let result = Value { expr: typed_expr, repr };
            match member.kind {
                MemberKind::Normal => result,
                MemberKind::Bitfield(Bitfield { offset, width }) => {
                    let value = try {
                        let size = 64_u64;
                        let shl_amount =
                            u32::try_from(size.strict_sub(width).strict_sub(offset)).unwrap();
                        let shr_amount = u32::try_from(size.strict_sub(width)).unwrap();
                        match result.into_integral()? {
                            Integral::Signed(value) => value
                                .strict_shl(shl_amount)
                                .strict_shr(shr_amount)
                                .cast_unsigned(),
                            Integral::Unsigned(value) =>
                                value.strict_shl(shl_amount).strict_shr(shr_amount),
                        }
                    };
                    value.map_or_else(
                        |errors| Value::with_errors(typed_expr, errors),
                        |value| {
                            let size = usize::try_from(size).unwrap();
                            let bytes = value.to_le_bytes().map(Byte::Literal)[..size].into();
                            let repr = Repr::Bytes(bytes);
                            Value { expr: typed_expr, repr }
                        },
                    )
                }
            }
        }

        Expression::BuiltinName(builtin) => match builtin.kind {
            // `__panko_gp_offset` could be constexpr evaluable, but it probably doesn’t need to be
            BuiltinNameKind::GpOffset | BuiltinNameKind::OverflowArgArea =>
                not_constexpr(typed_expr, no_errors()),
            BuiltinNameKind::Func(_) => unreachable!(),
        },

        Expression::Deref { star: _, operand } => match eval(operand).into_pointer() {
            Pointer::FromInt(Ok(_)) => not_constexpr(typed_expr, no_errors()),
            Pointer::FromInt(Err(errors)) => Value::with_errors(typed_expr, errors),
            Pointer::Address(Address { target, offset }) => match target {
                Target::Reference(_reference) => todo!("`constexpr` and e. g. `static const`"),
                Target::String(byte_str) => {
                    let index = usize::try_from(offset).unwrap();
                    let c = match byte_str.get(index) {
                        Some(c) => *c,
                        None if index == byte_str.len() => 0,
                        None => return not_constexpr(typed_expr, no_errors()),
                    };
                    let repr = Repr::Bytes([Byte::Literal(c)].into());
                    Value { expr: typed_expr, repr }
                }
            },
        },
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PersistedValue<'a, Reference> {
    Chunks { chunks: &'a [Chunk<'a, Reference>] },
    Error(&'a [&'a dyn Report]),
}

#[derive(Debug, Clone, Copy)]
pub enum Chunk<'a, Reference> {
    Literal(&'a [u8]),
    Address {
        target: PersistedTarget<'a, Reference>,
        offset: u64,
    },
}

impl<'a, Reference> Chunk<'a, Reference> {
    pub(crate) fn map<F, R>(self, mut f: F) -> Chunk<'a, R>
    where
        F: FnMut(Reference) -> R,
    {
        match self {
            Self::Literal(bytes) => Chunk::Literal(bytes),
            Self::Address { target, offset } => {
                let target = match target {
                    PersistedTarget::Reference(reference) =>
                        PersistedTarget::Reference(f(reference)),
                    PersistedTarget::String(value) => PersistedTarget::String(value),
                };
                Chunk::Address { target, offset }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PersistedTarget<'a, Reference> {
    Reference(Reference),
    String(&'a ByteStr),
}

fn persist<'a>(sess: &Session<'a>, bytes: Box<[Byte<'a>]>) -> PersistedValue<'a, Reference<'a>> {
    let chunks = bytes
        .into_iter()
        .chunk_by(|t| matches!(t, Byte::Literal(_)));
    let chunks = gen {
        for (is_literal, chunk) in &chunks {
            match is_literal {
                true =>
                    yield Chunk::Literal(sess.alloc_slice_collect(chunk.map(|b| match b {
                        Byte::Literal(b) => b,
                        Byte::Address { .. } => unreachable!(),
                    }))),
                false => {
                    let mut chunks = chunk.array_chunks::<8>();
                    while let Some([Byte::Address { index: _, target, offset }, ..]) = chunks.next()
                    {
                        yield Chunk::Address { target: target.persist(), offset }
                    }
                    assert_matches!(chunks.next(), None);
                    assert_matches!(chunks.into_remainder().next(), None);
                }
            }
        }
    };
    PersistedValue::Chunks { chunks: sess.alloc_slice_collect(chunks) }
}

pub(super) fn run_static_initialiser<'a>(
    sess: &Session<'a>,
    reference: &Reference<'a>,
    initialiser: &'a Initialiser<'a>,
) -> Initialiser<'a> {
    let expr = TypedExpression {
        ty: reference.ty,
        expr: Expression::Name(*reference),
    };
    let value = eval_static_initialiser(&expr, initialiser);
    Initialiser::Static {
        initialiser: InitialiserRef(initialiser),
        value: value.persist(sess),
    }
}
