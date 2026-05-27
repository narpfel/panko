use panko_parser::ast::Arithmetic;
use panko_parser::ast::Signedness;
use panko_parser::error_todo;

use super::Expression;
use crate::typecheck::Type;
use crate::typecheck::TypedExpression;

enum ConversionKind {
    Truncate,
    ZeroExtend,
    SignExtend,
    Noop,
    Bool,
}

pub(super) struct Errors;

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
    #[expect(unused)]
    Address {
        name: &'a str,
        offset: u64,
    },
    Error,
}

impl<'a> Value<'a> {
    pub(super) fn into_integral(self) -> Result<Integral, Errors> {
        let Self { ty, repr: _ } = &self;
        let signedness = match ty {
            Type::Arithmetic(Arithmetic::Integral(integral)) => integral.signedness,
            _ => return Err(Errors),
        };
        let ty = match signedness {
            Signedness::Unsigned => Type::ULONG,
            Signedness::Signed => Type::LONG,
        };
        let Value { ty: _, repr } = self.convert(ty, ConversionKind::SignExtend);
        let bytes = repr.bytes()?[..].try_into().unwrap();
        match signedness {
            Signedness::Signed => Ok(Integral::Signed(i64::from_le_bytes(bytes))),
            Signedness::Unsigned => Ok(Integral::Unsigned(u64::from_le_bytes(bytes))),
        }
    }

    fn error(ty: Type<'a>) -> Value<'a> {
        Self { ty, repr: Repr::Error }
    }

    fn int(value: u64, ty: Type<'a>) -> Self {
        assert!(ty.can_represent(value));
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
        if let Repr::Error = repr {
            return Self { ty: new_ty, repr };
        }
        type Kind = ConversionKind;
        match kind {
            Kind::Noop => Self { ty: new_ty, repr },
            Kind::Bool => {
                let is_nonzero = repr
                    .bytes()
                    .unwrap_or_else(|_| todo!("error recovery"))
                    .iter()
                    .any(|&b| b != 0);
                let byte = if is_nonzero { 1 } else { 0 };
                Self::int(byte, new_ty)
            }
            Kind::Truncate | Kind::ZeroExtend | Kind::SignExtend => match ty {
                Type::Arithmetic(Arithmetic::Integral(integral)) => {
                    let mut bytes = repr
                        .bytes()
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
}

impl<'a> Repr<'a> {
    fn bytes(self) -> Result<Box<[u8]>, Errors> {
        match self {
            Self::Bytes(bytes) => Ok(bytes),
            Self::Address { .. } => Err(Errors),
            Self::Error => Err(Errors),
        }
    }
}

pub(super) fn eval<'a>(typed_expr: &TypedExpression<'a>) -> Value<'a> {
    let TypedExpression { ty, expr } = typed_expr;
    let ty = ty.ty;

    match expr {
        Expression::Error(_) => Value::error(ty),

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

        Expression::NoopTypeConversion(expr) => eval(expr).convert(ty, ConversionKind::Noop),
        Expression::Truncate(expr) => eval(expr).convert(ty, ConversionKind::Truncate),
        Expression::SignExtend(expr) => eval(expr).convert(ty, ConversionKind::SignExtend),
        Expression::ZeroExtend(expr) => eval(expr).convert(ty, ConversionKind::ZeroExtend),
        Expression::BoolCast(expr) => eval(expr).convert(ty, ConversionKind::Bool),

        _ => error_todo!(typed_expr, "unimplemented constexpr eval"),
    }
}
