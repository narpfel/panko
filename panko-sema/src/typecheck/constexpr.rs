use std::collections::LinkedList;
use std::iter::once;

use itertools::zip_eq;
use panko_parser::BinOpKind;
use panko_parser::Comparison;
use panko_parser::LogicalOpKind;
use panko_parser::ast::Arithmetic;
use panko_parser::ast::Session;
use panko_parser::ast::Signedness;
use panko_parser::error_todo;

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

pub(super) enum Integral {
    Signed(i64),
    Unsigned(u64),
}

pub(super) struct Value<'a> {
    ty: Type<'a>,
    repr: Repr<'a>,
}

#[derive(Debug)]
enum Repr<'a> {
    Bytes(Box<[u8]>),
    Address {
        reference: &'a Reference<'a>,
        offset: u64,
    },
    Error(Errors<'a>),
}

impl<'a> Value<'a> {
    pub(super) fn into_integral(self) -> Result<Integral, Errors<'a>> {
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
        let bytes = repr.into_bytes()?[..].try_into().unwrap();
        match signedness {
            Signedness::Signed => Ok(Integral::Signed(i64::from_le_bytes(bytes))),
            Signedness::Unsigned => Ok(Integral::Unsigned(u64::from_le_bytes(bytes))),
        }
    }

    fn into_unsigned(self) -> Result<Option<u64>, Errors<'a>> {
        match self.into_integral()? {
            Integral::Unsigned(value) => Ok(Some(value)),
            Integral::Signed(value) => Ok(try { u64::try_from(value).ok()? }),
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
                let repr = Repr::Bytes(value.to_le_bytes()[..size].into());
                Self { ty, repr }
            }
            _ => todo!("error: invalid type for integral constexpr value"),
        }
    }

    fn convert(self, new_ty: Type<'a>, kind: ConversionKind) -> Self {
        let Self { ty, repr } = self;
        if let Repr::Error(_) = repr {
            return Self { ty: new_ty, repr };
        }
        type Kind = ConversionKind;
        match kind {
            Kind::Noop => Self { ty: new_ty, repr },
            Kind::Bool => {
                let is_nonzero = match repr {
                    Repr::Bytes(bytes) => bytes.iter().any(|&b| b != 0),
                    Repr::Address { .. } => true,
                    Repr::Error(_) => unreachable!(),
                };
                Self::int(is_nonzero.into(), new_ty)
            }
            Kind::Truncate | Kind::ZeroExtend | Kind::SignExtend => match ty {
                Type::Arithmetic(Arithmetic::Integral(integral)) => {
                    let mut bytes = repr
                        .into_bytes()
                        .unwrap_or_else(|_| {
                            todo!("this is possible when converting the result of a ptr2int cast")
                        })
                        .to_vec();
                    let fill_value = match integral.signedness {
                        Signedness::Signed if let ConversionKind::SignExtend = kind =>
                            match bytes.last().unwrap().cast_signed() < 0 {
                                true => 0xff,
                                false => 0,
                            },
                        _ => 0,
                    };
                    bytes.resize(usize::try_from(new_ty.size()).unwrap(), fill_value);
                    let repr = Repr::Bytes(bytes.into_boxed_slice());
                    Self { ty: new_ty, repr }
                }
                _ => unreachable!(),
            },
        }
    }

    fn bytes(&self) -> &[u8] {
        match &self.repr {
            Repr::Bytes(bytes) => bytes,
            Repr::Address { .. } => todo!(),
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
                    Repr::Bytes(bytes) => Ok($t::from_le_bytes(bytes[..].try_into().unwrap())),
                    Repr::Address { .. } => todo!(),
                    Repr::Error(errors) => Err(errors.clone()),
                }),
                _ => None,
            }
        }
    };
}

#[expect(non_camel_case_types)]
type ptr = u64;

impl<'a> Value<'a> {
    impl_as_ty!(Type::ULLONG | Type::ULONG => u64 + 'a);

    impl_as_ty!(Type::LLONG | Type::LONG => i64 + 'a);

    impl_as_ty!(Type::UINT => u32 + 'a);

    impl_as_ty!(Type::INT => i32 + 'a);

    impl_as_ty!(Type::Pointer(_) => ptr + 'a);
}

impl<'a> Repr<'a> {
    fn error(diagnostic: Diagnostic<'a>) -> Self {
        Self::Error(Errors::new(diagnostic))
    }

    fn into_bytes(self) -> Result<Box<[u8]>, Errors<'a>> {
        match self {
            Self::Bytes(bytes) => Ok(bytes),
            Self::Address { .. } => todo!(),
            Self::Error(errors) => Err(errors),
        }
    }
}

fn gather_errors<'a>(values: impl IntoIterator<Item = Value<'a>>) -> Errors<'a> {
    values
        .into_iter()
        .fold(Errors::default(), |errors, value| match value.repr {
            Repr::Error(error) => errors.chain(error),
            _ => errors,
        })
}

fn not_constexpr<'a>(
    expr: &TypedExpression<'a>,
    values: impl IntoIterator<Item = Value<'a>>,
) -> Value<'a> {
    let errors = gather_errors(values).chain(Errors::new(Diagnostic::NotConstexpr { at: *expr }));
    Value::with_errors(expr.ty.ty, errors)
}

fn as_bool<'a>(expr: &TypedExpression<'a>) -> Result<bool, Errors<'a>> {
    let value = eval(expr).convert(Type::BOOL, ConversionKind::Bool);
    value.into_integral().map(|integral| match integral {
        Integral::Signed(value) => value != 0,
        Integral::Unsigned(value) => value != 0,
    })
}

fn eval_pointer_arithmetic<'a>(
    ty: Type<'a>,
    pointer: Value<'a>,
    integral: Value<'a>,
    pointee_size: u64,
    op: impl FnOnce(u64, u64) -> u64,
) -> Value<'a> {
    let integral = integral.as_u64().expect("not a type error");
    let integral = arithmetic::binop(integral, Ok(pointee_size), u64::checked_mul, || todo!());
    let repr = match pointer.repr {
        Repr::Bytes(_) => arithmetic::binop(pointer.as_ptr().unwrap(), integral, op, || todo!())
            .map_or_else(Repr::Error, |result| {
                Repr::Bytes(Box::new(result.to_le_bytes()))
            }),
        Repr::Address { reference, offset } =>
            arithmetic::binop(Ok(offset), integral, op, || todo!())
                .map_or_else(Repr::Error, |offset| Repr::Address { reference, offset }),
        Repr::Error(errors) => Repr::Error(errors),
    };
    Value { ty, repr }
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
            repr: Repr::Bytes(Box::from(&**string_literal.value())),
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
                                        let rhs = rhs.ok_or_else(|| NegativeShiftRhs.at(rhs_expr))?;
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
                                        BinOpKind::Comparison(comparison) => {
                                            let result = match comparison {
                                                Comparison::Equal => lhs.eq(rhs),
                                                Comparison::NotEqual => lhs.ne(rhs),
                                                Comparison::Less => lhs.lt(rhs),
                                                Comparison::LessEqual => lhs.le(rhs),
                                                Comparison::Greater => lhs.gt(rhs),
                                                Comparison::GreaterEqual => lhs.ge(rhs),
                                            };
                                            return result.map(i32::from).into_value(ty);
                                        }
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

        Expression::Addressof { ampersand: _, operand } => {
            match &operand.expr {
                // TODO: `constexpr` storage class
                Expression::Name(reference)
                | Expression::CompoundLiteral {
                    open_paren: _,
                    decl: Declaration { reference, initialiser: _ },
                } => match reference.storage_duration {
                    StorageDuration::Static(_) => Value {
                        ty,
                        repr: Repr::Address { reference, offset: 0 },
                    },
                    StorageDuration::Automatic => not_constexpr(typed_expr, []),
                },
                Expression::Deref { star: _, operand } => {
                    let operand = eval(operand);
                    match &operand.repr {
                        Repr::Bytes(_) => todo!(),
                        Repr::Address { .. } => todo!(),
                        Repr::Error(_) => not_constexpr(typed_expr, [operand]),
                    }
                }
                _ => Value::with_error(ty, Diagnostic::NotImplementedYet { at: *typed_expr }),
            }
        }

        Expression::PtrAdd {
            pointer,
            integral,
            pointee_size,
            order: _,
        } => eval_pointer_arithmetic(
            ty,
            eval(pointer),
            eval(integral),
            *pointee_size,
            u64::strict_add,
        ),

        Expression::PtrSub { pointer, integral, pointee_size } => eval_pointer_arithmetic(
            ty,
            eval(pointer),
            eval(integral),
            *pointee_size,
            u64::strict_sub,
        ),

        Expression::PtrDiff { lhs, rhs, pointee_size } => {
            let compute_difference = |lhs, rhs| {
                let difference = arithmetic::binop(lhs, rhs, u64::checked_sub, || todo!());
                let result: Result<u64, _> =
                    arithmetic::binop(difference, Ok(*pointee_size), u64::div_exact, || todo!());
                result.into_value(ty)
            };

            let lhs = eval(lhs);
            let rhs = eval(rhs);

            match (&lhs.repr, &rhs.repr) {
                (Repr::Bytes(_), Repr::Bytes(_)) => {
                    let lhs = lhs.as_ptr().unwrap();
                    let rhs = rhs.as_ptr().unwrap();
                    compute_difference(lhs, rhs)
                }

                (
                    Repr::Address { reference, offset },
                    Repr::Address { reference: rhs_ref, offset: rhs_offset },
                ) => match reference.id == rhs_ref.id {
                    true => compute_difference(Ok(*offset), Ok(*rhs_offset)),
                    false => not_constexpr(typed_expr, []),
                },

                (Repr::Bytes(_), Repr::Address { .. }) | (Repr::Address { .. }, Repr::Bytes(_)) =>
                    not_constexpr(typed_expr, []),

                (Repr::Error(errors), Repr::Bytes(_) | Repr::Address { .. })
                | (Repr::Bytes(_) | Repr::Address { .. }, Repr::Error(errors)) =>
                    Value::with_errors(ty, errors.clone()),

                (Repr::Error(errors), Repr::Error(rhs)) =>
                    Value::with_errors(ty, errors.clone().chain(rhs.clone())),
            }
        }

        // TODO: `constexpr` storage class
        Expression::Name(_) => not_constexpr(typed_expr, []),

        Expression::Assign { target, value } =>
            not_constexpr(typed_expr, [eval(target), eval(value)]),

        Expression::Call {
            callee,
            args,
            is_varargs: _,
            close_paren: _,
        } => not_constexpr(typed_expr, once(eval(callee)).chain(args.iter().map(eval))),

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
            let Declaration { reference: _, initialiser } = decl;
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
            not_constexpr(typed_expr, exprs.map(eval))
        }

        Expression::BuiltinName(_) =>
            Value::with_error(ty, Diagnostic::NotImplementedYet { at: *typed_expr }),

        _ => error_todo!(typed_expr, "unimplemented constexpr eval"),
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum PersistedValue<'a> {
    Bytes(&'a [u8]),
    Pointer {
        reference: &'a Reference<'a>,
        offset: u64,
    },
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
            let mut bytes = vec![0; usize::try_from(ty.size()).unwrap()].into_boxed_slice();
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
                                sess.emit_many(errors);
                                continue;
                            }
                        };
                        for (tgt_byte, src_byte) in zip_eq(
                            &mut bytes[offset..][..subobject_size],
                            &(value << bitfield_offset).to_le_bytes()[..subobject_size],
                        ) {
                            *tgt_byte |= src_byte;
                        }
                    }
                }
            }
            let value = PersistedValue::Bytes(sess.alloc_slice_copy(&bytes));
            Initialiser::Static {
                initialiser: InitialiserRef(initialiser),
                value,
            }
        }
        Initialiser::Expression(expr) => {
            let value = match eval(expr).repr {
                Repr::Bytes(bytes) => PersistedValue::Bytes(sess.alloc_slice_copy(&bytes)),
                Repr::Address { reference, offset } =>
                    PersistedValue::Pointer { reference, offset },
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
