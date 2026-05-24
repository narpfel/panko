use panko_parser::ast::Arithmetic;
use panko_parser::ast::Signedness;
use panko_parser::error_todo;

use super::Expression;
use crate::typecheck::Type;
use crate::typecheck::TypedExpression;

#[expect(unused)]
enum ConversionKind {
    Truncate,
    ZeroExtend,
    SignExtend,
    Noop,
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
    #[expect(unused)]
    Address {
        name: &'a str,
        offset: u64,
    },
}

impl<'a> Value<'a> {
    pub(super) fn into_integral(self) -> Integral {
        let Self { ty, repr: _ } = &self;
        let signedness = match ty {
            Type::Arithmetic(Arithmetic::Integral(integral)) => integral.signedness,
            _ => todo!(),
        };
        let ty = match signedness {
            Signedness::Unsigned => Type::ULONG,
            Signedness::Signed => Type::LONG,
        };
        let Value { ty: _, repr } = self.convert(ty, ConversionKind::SignExtend);
        let bytes = repr.bytes();
        match signedness {
            Signedness::Signed => Integral::Signed(i64::from_le_bytes(bytes.try_into().unwrap())),
            Signedness::Unsigned =>
                Integral::Unsigned(u64::from_le_bytes(bytes.try_into().unwrap())),
        }
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
        match kind {
            ConversionKind::Noop => Self { ty: new_ty, repr },
            _ => match ty {
                Type::Arithmetic(Arithmetic::Integral(integral)) => {
                    let mut bytes = repr.bytes().to_owned();
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
    fn bytes(&self) -> &[u8] {
        match self {
            Self::Bytes(bytes) => bytes,
            Self::Address { .. } => unreachable!(),
        }
    }
}

pub(super) fn eval<'a>(typed_expr: &TypedExpression<'a>) -> Value<'a> {
    let TypedExpression { ty, expr } = typed_expr;

    match expr {
        Expression::Integer { value, token: _ } => Value::int(*value, ty.ty),

        Expression::Sizeof { sizeof: _, operand: _, size }
        | Expression::SizeofTy { sizeof: _, ty: _, size, close_paren: _ } =>
            Value::int(*size, ty.ty),

        _ => error_todo!(typed_expr, "unimplemented constexpr eval"),
    }
}
