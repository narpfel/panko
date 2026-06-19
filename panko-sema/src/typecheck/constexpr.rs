use std::assert_matches;
use std::collections::LinkedList;
use std::iter::once;

use itertools::Itertools as _;
use itertools::zip_eq;
use panko_parser::BinOpKind;
use panko_parser::Comparison;
use panko_parser::LogicalOpKind;
use panko_parser::ast::Arithmetic;
use panko_parser::ast::Session;
use panko_parser::ast::Signedness;

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
pub(super) struct Errors<'a>(LinkedList<Diagnostic<'a>>);

impl<'a> Errors<'a> {
    fn new(diagnostic: Diagnostic<'a>) -> Self {
        Self(LinkedList::from_iter([diagnostic]))
    }

    fn chain(self, Self(mut other_errors): Self) -> Self {
        let Self(mut errors) = self;
        errors.append(&mut other_errors);
        Self(errors)
    }
}

impl<'a> IntoIterator for Errors<'a> {
    type IntoIter = std::collections::linked_list::IntoIter<Self::Item>;
    type Item = Diagnostic<'a>;

    fn into_iter(self) -> Self::IntoIter {
        let Self(errors) = self;
        errors.into_iter()
    }
}

enum Integral {
    Signed(i64),
    Unsigned(u64),
}

pub(super) struct Value<'a> {
    ty: Type<'a>,
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
        #[expect(unused, reason = "TODO: check for wellformedness while parsing")]
        index: u64,
        reference: &'a Reference<'a>,
        // TODO: this should be bounded by the `reference`’s type’s size
        offset: u64,
    },
}

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

#[derive(Debug, Clone, Copy)]
struct Address<'a> {
    reference: &'a Reference<'a>,
    offset: u64,
}

fn read_address<'a>(bytes: &[Byte<'a>]) -> Option<Address<'a>> {
    match bytes.first()? {
        Byte::Literal(_) => None,
        Byte::Address { index: _, reference, offset } =>
            Some(Address { reference, offset: *offset }),
    }
}

fn write_address<'a>(Address { reference, offset }: Address<'a>) -> Repr<'a> {
    Repr::Bytes(
        (0..8)
            .map(|index| Byte::Address { index, reference, offset })
            .collect(),
    )
}

enum Pointer<'a> {
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

impl<'a> Value<'a> {
    fn into_integral(self) -> Result<Integral, Errors<'a>> {
        let Self { ty, repr: _ } = &self;
        let signedness = match ty {
            Type::Arithmetic(Arithmetic::Integral(integral)) => integral.signedness,
            _ => todo!("type error"),
        };
        let ty = match signedness {
            Signedness::Unsigned => Type::ULONG,
            Signedness::Signed => Type::LONG,
        };
        let Value { ty: _, repr } = self.convert(ty, ConversionKind::SignExtend);
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

    fn into_pointer(self) -> Pointer<'a> {
        let Self { ty, repr } = self;
        assert_matches!(ty, Type::Pointer(_) | Type::Nullptr);
        match repr {
            Repr::Bytes(bytes) if let Some(value) = read_u64(&bytes) => Pointer::FromInt(Ok(value)),
            Repr::Bytes(bytes) if let Some(addr) = read_address(&bytes) => Pointer::Address(addr),
            Repr::Bytes(_) => unreachable!("mixing `Literal` and `Address` bytes is invalid"),
            Repr::Error(errors) => Pointer::FromInt(Err(errors)),
        }
    }

    fn error(ty: Type<'a>) -> Self {
        Self { ty, repr: Repr::Error(Errors::default()) }
    }

    fn with_error(ty: Type<'a>, error: Diagnostic<'a>) -> Self {
        Value { ty, repr: Repr::error(error) }
    }

    fn with_errors(ty: Type<'a>, errors: Errors<'a>) -> Self {
        Self { ty, repr: Repr::Error(errors) }
    }

    fn int(value: u64, ty: Type<'a>) -> Self {
        assert!(ty.can_represent(value), "`{ty}`.can_represent({value})");
        match ty {
            Type::Arithmetic(Arithmetic::Integral(integral)) => {
                let size = usize::try_from(integral.size()).unwrap();
                let bytes = value.to_le_bytes().map(Byte::Literal)[..size].into();
                let repr = Repr::Bytes(bytes);
                Self { ty, repr }
            }
            _ => todo!("error: invalid type for integral constexpr value"),
        }
    }

    fn convert(self, new_ty: Type<'a>, kind: ConversionKind) -> Self {
        let Self { ty, repr } = self;
        type Kind = ConversionKind;
        match kind {
            _ if let Repr::Error(_) = repr => Self { ty: new_ty, repr },
            // TODO: check for illegal pointer conversions
            Kind::Noop => Self { ty: new_ty, repr },
            Kind::Bool => {
                let is_nonzero = match repr {
                    Repr::Bytes(bytes) => bytes.iter().any(|&b| match b {
                        Byte::Literal(b) => b != 0,
                        Byte::Address { .. } => true,
                    }),
                    Repr::Error(_) => unreachable!(),
                };
                Self::int(is_nonzero.into(), new_ty)
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
                    bytes.resize(usize::try_from(new_ty.size()).unwrap(), fill_value);
                    let repr = Repr::Bytes(bytes.into_iter().map(Byte::Literal).collect());
                    Self { ty: new_ty, repr }
                }
                _ => todo!("error message, e. g. `static int x = (int)&static_var;`"),
            },
        }
    }

    fn bytes(&self) -> &[Byte<'a>] {
        match &self.repr {
            Repr::Bytes(bytes) => bytes,
            Repr::Error(_errors) => todo!(),
        }
    }
}

macro_rules! impl_as_ty {
    ($pattern:pat => $t:ident + $l:lifetime) => {
        fn ${concat(as_, $t)}(&self) -> Option<Result<$t, Errors<$l>>> {
            let Self { ty, repr } = self;
            match *ty {
                $pattern => Some(match repr {
                    Repr::Bytes(bytes) => Ok($t::from_le_bytes(read_literal_bytes(bytes)?)),
                    Repr::Error(errors) => Err(errors.clone()),
                }),
                _ => None,
            }
        }
    };
}

impl<'a> Value<'a> {
    impl_as_ty!(Type::ULLONG | Type::ULONG => u64 + 'a);

    impl_as_ty!(Type::LLONG | Type::LONG => i64 + 'a);

    impl_as_ty!(Type::UINT => u32 + 'a);

    impl_as_ty!(Type::INT => i32 + 'a);
}

impl<'a> Repr<'a> {
    fn error(diagnostic: Diagnostic<'a>) -> Self {
        Self::Error(Errors::new(diagnostic))
    }

    fn into_bytes(self) -> Result<Box<[Byte<'a>]>, Errors<'a>> {
        match self {
            Self::Bytes(bytes) => Ok(bytes),
            Self::Error(errors) => Err(errors),
        }
    }
}

trait IntoErrors<'a> {
    fn into_errors(self) -> Option<Errors<'a>>;
}

impl<'a> IntoErrors<'a> for Value<'a> {
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

fn no_errors<'a>() -> [Value<'a>; 0] {
    []
}

fn not_constexpr<'a>(
    expr: &TypedExpression<'a>,
    values: impl IntoIterator<Item = impl IntoErrors<'a>>,
) -> Value<'a> {
    let errors = gather_errors(values).chain(Errors::new(Diagnostic::NotConstexpr { at: *expr }));
    Value::with_errors(expr.ty.ty, errors)
}

fn as_bool<'a>(expr: &TypedExpression<'a>) -> Result<bool, Errors<'a>> {
    let value = eval(expr).convert(Type::BOOL, ConversionKind::Bool);
    Ok(value.into_unsigned()?.expect("`bool` is unsigned") != 0)
}

fn eval_pointer_arithmetic<'a>(
    expr: &TypedExpression<'a>,
    pointer: &TypedExpression<'a>,
    integral: Value<'a>,
    pointee_size: u64,
    op: impl FnOnce(u64, i64) -> Option<u64>,
) -> Value<'a> {
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
        Pointer::Address(Address { reference, offset }) =>
            add(Ok(offset)).map(|offset| write_address(Address { reference, offset })),
    }
    .unwrap_or_else(Repr::Error);

    Value { ty: expr.ty.ty, repr }
}

enum PointerBinopKind {
    Equality,
    Other,
}

fn eval_pointer_binop<'a>(
    typed_expr: &TypedExpression<'a>,
    kind: PointerBinopKind,
    lhs: &TypedExpression<'a>,
    rhs: &TypedExpression<'a>,
    compute: impl FnOnce(Result<u64, Errors<'a>>, Result<u64, Errors<'a>>) -> Value<'a>,
) -> Value<'a> {
    let lhs = eval(lhs);
    let rhs = eval(rhs);

    match (lhs.into_pointer(), rhs.into_pointer()) {
        (Pointer::FromInt(lhs), Pointer::FromInt(rhs)) => compute(lhs, rhs),

        (
            Pointer::Address(Address { reference, offset }),
            Pointer::Address(Address { reference: rhs_ref, offset: rhs_offset }),
        ) if reference.id == rhs_ref.id => compute(Ok(offset), Ok(rhs_offset)),

        (lhs, rhs) => match kind {
            PointerBinopKind::Equality => compute(lhs.with_value(0), rhs.with_value(1)),
            PointerBinopKind::Other => not_constexpr(typed_expr, [lhs, rhs]),
        },
    }
}

pub(super) fn eval<'a>(typed_expr: &TypedExpression<'a>) -> Value<'a> {
    let TypedExpression { ty, expr } = typed_expr;
    let ty = ty.ty;

    macro_rules! unary {
        ($operand:expr, $meth:ident, $($t:ident),*) => {{
            let operand = $operand;
            match () {
                $(
                    () if let Some(operand) = operand.${concat(as_, $t)}() =>
                        operand.$meth(typed_expr).into_value(ty),
                )*
                () => todo!(),
            }
        }};
    }

    match expr {
        Expression::Error(_) => Value::error(ty),

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
        } => Value::int(*value, ty),

        Expression::String(string_literal) => Value {
            ty,
            repr: Repr::Bytes(
                string_literal
                    .value()
                    .iter()
                    .copied()
                    .map(Byte::Literal)
                    .collect(),
            ),
        },

        Expression::Nullptr(_) => Value::int(0, Type::size_t()).convert(ty, ConversionKind::Noop),

        Expression::NoopTypeConversion(expr) => eval(expr).convert(ty, ConversionKind::Noop),
        Expression::Truncate(expr) => eval(expr).convert(ty, ConversionKind::Truncate),
        Expression::SignExtend(expr) => eval(expr).convert(ty, ConversionKind::SignExtend),
        Expression::ZeroExtend(expr) => eval(expr).convert(ty, ConversionKind::ZeroExtend),
        Expression::BoolCast(expr) => eval(expr).convert(ty, ConversionKind::Bool),

        Expression::VoidCast(expr) => {
            let errors = gather_errors([eval(expr)]);
            match errors.0.is_empty() {
                true => Value { ty, repr: Repr::Bytes(Box::new([])) },
                false => Value::with_errors(ty, errors),
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
                                    result.into_value(ty)
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
                                            return lhs.compare(comparison, rhs).into_value(ty),
                                    };
                                    result.into_value(ty)
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
            let operand = eval(operand).convert(ty, ConversionKind::Bool);
            unary!(operand, not, i32)
        }

        Expression::Addressof { ampersand, operand } => match &operand.expr {
            Expression::Parenthesised { open_paren: _, expr, close_paren: _ } =>
                eval(&TypedExpression {
                    expr: Expression::Addressof { ampersand: *ampersand, operand: expr },
                    ..*typed_expr
                }),
            Expression::Name(reference)
            | Expression::CompoundLiteral {
                open_paren: _,
                decl: Declaration { reference, initialiser: _ },
            } => match reference.storage_duration {
                StorageDuration::Static(_) => Value {
                    ty,
                    repr: write_address(Address { reference, offset: 0 }),
                },
                StorageDuration::Automatic => not_constexpr(typed_expr, no_errors()),
            },
            Expression::Deref { star: _, operand } => eval(operand),
            Expression::MemberAccess { lhs, member, member_loc: _ } => {
                let pointer = &TypedExpression {
                    ty: Type::Pointer(&lhs.ty).unqualified(),
                    expr: Expression::Addressof { ampersand: *ampersand, operand: lhs },
                };
                let offset = Value::int(member.offset, Type::ptrdiff_t());
                eval_pointer_arithmetic(typed_expr, pointer, offset, 1, u64::checked_add_signed)
            }
            _ => Value::with_error(ty, Diagnostic::NotImplementedYet { at: *typed_expr }),
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
                result.into_value(ty)
            }),

        Expression::PtrCmp { lhs, kind, rhs } => {
            let binop_kind = match kind {
                Comparison::Equal | Comparison::NotEqual => PointerBinopKind::Equality,
                _ => PointerBinopKind::Other,
            };
            eval_pointer_binop(typed_expr, binop_kind, lhs, rhs, |lhs, rhs| {
                lhs.compare(*kind, rhs).into_value(ty)
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
                false => Value::with_errors(ty, errors.chain(gather_errors([second]))),
            }
        }

        Expression::Logical { lhs, op, rhs } => {
            let result = match op.kind() {
                LogicalOpKind::And => try { as_bool(lhs)? && as_bool(rhs)? },
                LogicalOpKind::Or => try { as_bool(lhs)? || as_bool(rhs)? },
            };
            result.map(i32::from).into_value(ty)
        }

        Expression::Conditional { condition, then, or_else } => match as_bool(condition) {
            Ok(true) => eval(then),
            Ok(false) => eval(or_else),
            Err(errors) => Value::with_errors(ty, errors),
        },

        // TODO: `constexpr` storage class
        // TODO: clang (and gcc with a warning) accept compound literals without `constexpr` as
        // constant expressions
        Expression::CompoundLiteral { open_paren: _, decl } => {
            let Declaration { reference, initialiser } = decl;
            match reference.storage_duration {
                StorageDuration::Static(_) =>
                    Value::with_error(ty, Diagnostic::NotImplementedYet { at: *typed_expr }),
                StorageDuration::Automatic => {
                    let exprs = gen {
                        match initialiser {
                            Some(Initialiser::Braced { subobject_initialisers }) =>
                                for SubobjectInitialiser { subobject: _, initialiser } in
                                    *subobject_initialisers
                                {
                                    yield initialiser
                                },
                            Some(Initialiser::Expression(expr)) => yield expr,
                            None | Some(Initialiser::Static { .. }) => (),
                        }
                    };
                    not_constexpr(typed_expr, exprs)
                }
            }
        }

        Expression::Deref { .. } | Expression::MemberAccess { .. } | Expression::BuiltinName(_) =>
            Value::with_error(ty, Diagnostic::NotImplementedYet { at: *typed_expr }),
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PersistedValue<'a, Reference> {
    pub chunks: &'a [Chunk<'a, Reference>],
}

#[derive(Debug, Clone, Copy)]
pub enum Chunk<'a, Reference> {
    Literal(&'a [u8]),
    Address { reference: Reference, offset: u64 },
}

impl<'a, Reference> Chunk<'a, Reference> {
    pub(crate) fn map<F, R>(self, mut f: F) -> Chunk<'a, R>
    where
        F: FnMut(Reference) -> R,
    {
        match self {
            Self::Literal(bytes) => Chunk::Literal(bytes),
            Self::Address { reference, offset } =>
                Chunk::Address { reference: f(reference), offset },
        }
    }
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
                    while let Some([Byte::Address { index: _, reference, offset }, ..]) =
                        chunks.next()
                    {
                        yield Chunk::Address { reference: *reference, offset }
                    }
                    assert_matches!(chunks.next(), None);
                    assert_matches!(chunks.into_remainder().next(), None);
                }
            }
        }
    };
    PersistedValue { chunks: sess.alloc_slice_collect(chunks) }
}

pub(super) fn run_static_initialiser<'a>(
    sess: &Session<'a>,
    ty: &Type<'a>,
    initialiser: &'a Initialiser<'a>,
) -> Initialiser<'a> {
    match initialiser {
        Initialiser::Braced { subobject_initialisers: [] } =>
            Initialiser::Braced { subobject_initialisers: &[] },
        Initialiser::Braced { subobject_initialisers } => {
            let mut bytes =
                vec![Byte::Literal(0); usize::try_from(ty.size()).unwrap()].into_boxed_slice();
            for SubobjectInitialiser { subobject, initialiser } in *subobject_initialisers {
                let Subobject { ty, offset, kind } = *subobject;
                let subobject_size = usize::try_from(ty.size()).unwrap();
                let offset = usize::try_from(offset).unwrap();
                let value = eval(initialiser);
                match kind {
                    MemberKind::Normal =>
                        bytes[offset..][..subobject_size].copy_from_slice(value.bytes()),
                    MemberKind::Bitfield(Bitfield { offset: bitfield_offset, width: _ }) => {
                        let value = match value.into_integral() {
                            Ok(Integral::Signed(value)) => value.cast_unsigned(),
                            Ok(Integral::Unsigned(value)) => value,
                            Err(errors) => {
                                // TODO: all `emit_many` calls break `--defer-type-errors`; the
                                // errors must be preserved
                                sess.emit_many(errors);
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
                }
            }

            Initialiser::Static {
                initialiser: InitialiserRef(initialiser),
                value: persist(sess, bytes),
            }
        }
        Initialiser::Expression(expr) => {
            let value = match eval(expr).repr {
                Repr::Bytes(bytes) => persist(sess, bytes),
                Repr::Error(errors) => {
                    sess.emit_many(errors);
                    return Initialiser::Braced { subobject_initialisers: &[] };
                }
            };
            Initialiser::Static {
                initialiser: InitialiserRef(initialiser),
                value,
            }
        }
        Initialiser::Static { initialiser: _, value: _ } => unreachable!(),
    }
}
