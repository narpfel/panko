use std::ops::Add;
use std::ops::BitAnd;
use std::ops::BitOr;
use std::ops::BitXor;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Neg;
use std::ops::Not;
use std::ops::Rem;
use std::ops::Shl;
use std::ops::Shr;
use std::ops::Sub;

use panko_lex::Integer;
use panko_lex::TokenKind;

use crate::BinOp;
use crate::BinOpKind;
use crate::Expression;
use crate::LogicalOp;
use crate::LogicalOpKind;
use crate::UnaryOp;
use crate::UnaryOpKind;

#[derive(Debug, Clone, Copy)]
pub(super) enum Value {
    Signed(i64),
    Unsigned(u64),
}

impl Value {
    pub(super) fn is_truthy(self) -> bool {
        match self {
            Self::Signed(value) => value != 0,
            Self::Unsigned(value) => value != 0,
        }
    }

    fn into_u64(self) -> u64 {
        match self {
            Value::Signed(value) => value.cast_unsigned(),
            Value::Unsigned(value) => value,
        }
    }
}

impl Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::Signed(value) =>
                Self::Signed(value.checked_neg().expect("TODO: signed overflow error")),
            Self::Unsigned(value) => Self::Unsigned(value.wrapping_neg()),
        }
    }
}

impl Not for Value {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Signed(value) => Self::Signed(!value),
            Self::Unsigned(value) => Self::Unsigned(!value),
        }
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Signed(lhs), Self::Signed(rhs)) =>
                Self::Signed(lhs.checked_mul(rhs).expect("TODO: signed overflow error")),
            (Self::Signed(lhs), Self::Unsigned(rhs)) =>
                Self::Unsigned(lhs.cast_unsigned().wrapping_mul(rhs)),
            (Self::Unsigned(lhs), Self::Signed(rhs)) =>
                Self::Unsigned(lhs.wrapping_mul(rhs.cast_unsigned())),
            (Self::Unsigned(lhs), Self::Unsigned(rhs)) => Self::Unsigned(lhs.wrapping_mul(rhs)),
        }
    }
}

impl Div for Value {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        if rhs.into_u64() == 0 {
            todo!("error: division by zero")
        }
        else {
            match (self, rhs) {
                (Value::Signed(lhs), Value::Signed(rhs)) =>
                    Self::Signed(lhs.checked_div(rhs).expect("TODO: signed overflow error")),
                (Value::Signed(lhs), Value::Unsigned(rhs)) =>
                    Self::Unsigned(lhs.cast_unsigned().wrapping_div(rhs)),
                (Value::Unsigned(lhs), Value::Signed(rhs)) =>
                    Self::Unsigned(lhs.wrapping_div(rhs.cast_unsigned())),
                (Value::Unsigned(lhs), Value::Unsigned(rhs)) =>
                    Self::Unsigned(lhs.wrapping_div(rhs)),
            }
        }
    }
}

impl Rem for Value {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        if rhs.into_u64() == 0 {
            todo!("error: division by zero")
        }
        else {
            match (self, rhs) {
                (Value::Signed(lhs), Value::Signed(rhs)) =>
                    Self::Signed(lhs.checked_rem(rhs).expect("TODO: signed overflow error")),
                (Value::Signed(lhs), Value::Unsigned(rhs)) =>
                    Self::Unsigned(lhs.cast_unsigned().wrapping_rem(rhs)),
                (Value::Unsigned(lhs), Value::Signed(rhs)) =>
                    Self::Unsigned(lhs.wrapping_rem(rhs.cast_unsigned())),
                (Value::Unsigned(lhs), Value::Unsigned(rhs)) =>
                    Self::Unsigned(lhs.wrapping_rem(rhs)),
            }
        }
    }
}

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Signed(lhs), Self::Signed(rhs)) =>
                Self::Signed(lhs.checked_add(rhs).expect("TODO: signed overflow error")),
            (Self::Signed(lhs), Self::Unsigned(rhs)) =>
                Self::Unsigned(lhs.cast_unsigned().wrapping_add(rhs)),
            (Self::Unsigned(lhs), Self::Signed(rhs)) =>
                Self::Unsigned(lhs.wrapping_add(rhs.cast_unsigned())),
            (Self::Unsigned(lhs), Self::Unsigned(rhs)) => Self::Unsigned(lhs.wrapping_add(rhs)),
        }
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Signed(lhs), Self::Signed(rhs)) =>
                Self::Signed(lhs.checked_sub(rhs).expect("TODO: signed overflow error")),
            (Self::Signed(lhs), Self::Unsigned(rhs)) =>
                Self::Unsigned(lhs.cast_unsigned().wrapping_sub(rhs)),
            (Self::Unsigned(lhs), Self::Signed(rhs)) =>
                Self::Unsigned(lhs.wrapping_sub(rhs.cast_unsigned())),
            (Self::Unsigned(lhs), Self::Unsigned(rhs)) => Self::Unsigned(lhs.wrapping_sub(rhs)),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.into_u64() == other.into_u64()
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Signed(lhs), Self::Signed(rhs)) => lhs.partial_cmp(rhs),
            (Self::Signed(lhs), Self::Unsigned(rhs)) => lhs.cast_unsigned().partial_cmp(rhs),
            (Self::Unsigned(lhs), Self::Signed(rhs)) => lhs.partial_cmp(&rhs.cast_unsigned()),
            (Self::Unsigned(lhs), Self::Unsigned(rhs)) => lhs.partial_cmp(rhs),
        }
    }
}

impl Shl for Value {
    type Output = Self;

    fn shl(self, rhs: Self) -> Self::Output {
        let rhs = match rhs {
            Self::Signed(rhs) if (0..64).contains(&rhs) => rhs.cast_unsigned(),
            Self::Unsigned(rhs) if (0..64).contains(&rhs) => rhs,
            _ => todo!("error: shift rhs is invalid"),
        };
        match self {
            Self::Signed(value) if value < 0 =>
                todo!("error: left shift is UB for negative values"),
            Self::Signed(value) => Self::Signed(
                value
                    .checked_mul(1 << rhs)
                    .expect("TODO: signed overflow error"),
            ),
            Self::Unsigned(value) => Self::Unsigned(value << rhs),
        }
    }
}

impl Shr for Value {
    type Output = Self;

    fn shr(self, rhs: Self) -> Self::Output {
        let rhs = match rhs {
            Self::Signed(rhs) if (0..64).contains(&rhs) => rhs.cast_unsigned(),
            Self::Unsigned(rhs) if (0..64).contains(&rhs) => rhs,
            _ => todo!("error: shift rhs is invalid"),
        };
        let rhs = u32::try_from(rhs).unwrap();
        match self {
            Self::Signed(value) => Self::Signed(value.wrapping_shr(rhs)),
            Self::Unsigned(value) => Self::Unsigned(value.wrapping_shr(rhs)),
        }
    }
}

impl BitAnd for Value {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Signed(lhs), Self::Signed(rhs)) => Self::Signed(lhs & rhs),
            (Self::Signed(lhs), Self::Unsigned(rhs)) => Self::Unsigned(lhs.cast_unsigned() & rhs),
            (Self::Unsigned(lhs), Self::Signed(rhs)) => Self::Unsigned(lhs & rhs.cast_unsigned()),
            (Self::Unsigned(lhs), Self::Unsigned(rhs)) => Self::Unsigned(lhs & rhs),
        }
    }
}

impl BitXor for Value {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Signed(lhs), Self::Signed(rhs)) => Self::Signed(lhs ^ rhs),
            (Self::Signed(lhs), Self::Unsigned(rhs)) => Self::Unsigned(lhs.cast_unsigned() ^ rhs),
            (Self::Unsigned(lhs), Self::Signed(rhs)) => Self::Unsigned(lhs ^ rhs.cast_unsigned()),
            (Self::Unsigned(lhs), Self::Unsigned(rhs)) => Self::Unsigned(lhs ^ rhs),
        }
    }
}

impl BitOr for Value {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Signed(lhs), Self::Signed(rhs)) => Self::Signed(lhs | rhs),
            (Self::Signed(lhs), Self::Unsigned(rhs)) => Self::Unsigned(lhs.cast_unsigned() | rhs),
            (Self::Unsigned(lhs), Self::Signed(rhs)) => Self::Unsigned(lhs | rhs.cast_unsigned()),
            (Self::Unsigned(lhs), Self::Unsigned(rhs)) => Self::Unsigned(lhs | rhs),
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Signed(value.into())
    }
}

fn eval_binop(op: &BinOp, lhs: &Expression, rhs: &Expression) -> Value {
    let lhs = eval(lhs);
    let rhs = eval(rhs);
    match op.kind {
        BinOpKind::Multiply => lhs * rhs,
        BinOpKind::Divide => lhs / rhs,
        BinOpKind::Modulo => lhs % rhs,
        BinOpKind::Add => lhs + rhs,
        BinOpKind::Subtract => lhs - rhs,
        BinOpKind::Equal => (lhs == rhs).into(),
        BinOpKind::NotEqual => (lhs != rhs).into(),
        BinOpKind::Less => (lhs < rhs).into(),
        BinOpKind::LessEqual => (lhs <= rhs).into(),
        BinOpKind::Greater => (lhs > rhs).into(),
        BinOpKind::GreaterEqual => (lhs >= rhs).into(),
        BinOpKind::LeftShift => lhs << rhs,
        BinOpKind::RightShift => lhs >> rhs,
        BinOpKind::BitAnd => lhs & rhs,
        BinOpKind::BitXor => lhs ^ rhs,
        BinOpKind::BitOr => lhs | rhs,
    }
}

fn eval_unary_op(operator: &UnaryOp, operand: &Expression) -> Value {
    let value = eval(operand);
    match operator.kind {
        UnaryOpKind::Addressof => todo!("error: there are no lvalues"),
        UnaryOpKind::Deref => todo!("error: not a pointer"),
        UnaryOpKind::Plus => value,
        UnaryOpKind::Negate => -value,
        UnaryOpKind::Compl => !value,
        UnaryOpKind::Not => Value::Signed(i64::from(!value.is_truthy())),
        UnaryOpKind::Sizeof => unreachable!("starts with an identifier"),
        UnaryOpKind::Lengthof => unreachable!("starts with an identifier"),
    }
}

fn eval_logical_op(op: &LogicalOp, lhs: &Expression, rhs: &Expression) -> Value {
    // TODO: Lazy eval swallows type errors in `rhs`. We should eagerly eval `rhs` and
    // differentiate between type errors (always propagate) and value-range errors (division by
    // zero, integer overflow etc.) that are only propagated when `rhs` should be evaluated.
    let result = match op.kind {
        LogicalOpKind::And => eval(lhs).is_truthy() && eval(rhs).is_truthy(),
        LogicalOpKind::Or => eval(lhs).is_truthy() || eval(rhs).is_truthy(),
    };
    Value::Signed(result.into())
}

pub(super) fn eval(expr: &Expression) -> Value {
    match expr {
        Expression::Error(_report) => unreachable!("the parser does not produce this expr kind"),
        Expression::Name(_token) =>
            unreachable!("we have expanded all macros and replaced all non-macros with 0"),
        Expression::Integer(token) => {
            let TokenKind::Integer(Integer { suffix: _, suffix_len, base, prefix_len }) =
                token.kind
            else {
                unreachable!()
            };

            let slice = token.slice();
            let number = &slice[prefix_len..slice.len() - suffix_len];
            let number: String = number.chars().filter(|&c| c != '\'').collect();
            match u64::from_str_radix(&number, base) {
                Ok(value) => i64::try_from(value).map_or(Value::Unsigned(value), Value::Signed),
                Err(_) => todo!(),
            }
        }
        Expression::CharConstant(_token) => todo!(),
        Expression::String(_tokens) => todo!("error: apparently not allowed?"),
        Expression::Parenthesised { open_paren: _, expr, close_paren: _ } => eval(expr),
        Expression::Assign { .. } => unreachable!("prevented by grammar"),
        Expression::CompoundAssign { .. } => unreachable!("prevented by grammar"),
        Expression::BinOp { lhs, op, rhs } => eval_binop(op, lhs, rhs),
        Expression::UnaryOp { operator, operand } => eval_unary_op(operator, operand),
        Expression::Call { .. } => todo!("always a type error"),
        Expression::Sizeof { .. } => unreachable!("starts with an identifier"),
        Expression::Lengthof { .. } => unreachable!("starts with an identifier"),
        Expression::Alignof { .. } => unreachable!("starts with an identifier"),
        Expression::Cast { .. } => unreachable!("the type name would contain an identifier"),
        Expression::Subscript { .. } => todo!("error: invalid op"),
        Expression::Generic { .. } => unreachable!("starts with an identifier"),
        Expression::Logical { lhs, op, rhs } => eval_logical_op(op, lhs, rhs),
        Expression::Conditional {
            condition,
            question_mark: _,
            then,
            or_else,
        } =>
            if eval(condition).is_truthy() {
                eval(then)
            }
            else {
                eval(or_else)
            },
        Expression::Comma { .. } => todo!("not allowed"),
        Expression::Increment { .. } => todo!("not allowed"),
    }
}
