use std::cmp::Ordering;
use std::collections::LinkedList;
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

use ariadne::Color::Red;
use panko_lex::Integer;
use panko_lex::IntegerSuffix;
use panko_lex::TokenKind;
use panko_report::Report;

use crate::BinOp;
use crate::BinOpKind;
use crate::Expression;
use crate::LogicalOp;
use crate::LogicalOpKind;
use crate::UnaryOp;
use crate::UnaryOpKind;

#[derive(Debug, Clone, Copy)]
pub(super) enum EvalError {
    SignedOverflow,
}

#[derive(Debug, Clone, Copy, Report)]
#[exit_code(1)]
pub(super) enum Diagnostic<'a> {
    #[error("error while evaluating constant expression: {kind:?}")]
    #[diagnostics(at(colour = Red, label = "while evaluating this expression"))]
    EvalError { at: Expression<'a>, kind: EvalError },
}

#[derive(Debug, Default)]
pub(super) struct Reports<'a>(LinkedList<Diagnostic<'a>>);

impl<'a> Reports<'a> {
    fn new(report: Diagnostic<'a>) -> Self {
        Self(LinkedList::from_iter([report]))
    }

    fn chain(mut self, mut others: Self) -> Self {
        self.0.append(&mut others.0);
        self
    }
}

impl<'a> IntoIterator for Reports<'a> {
    type IntoIter = std::collections::linked_list::IntoIter<Diagnostic<'a>>;
    type Item = Diagnostic<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Debug)]
pub(super) enum Value<'a> {
    Error(Reports<'a>),
    Signed(i64),
    Unsigned(u64),
}

impl<'a> Value<'a> {
    pub(super) fn into_bool(self) -> Result<bool, Reports<'a>> {
        match self {
            Self::Error(reports) => Err(reports),
            Self::Signed(value) => Ok(value != 0),
            Self::Unsigned(value) => Ok(value != 0),
        }
    }

    fn as_u64(&self) -> u64 {
        match self {
            Self::Error(_) => panic!(),
            Self::Signed(value) => value.cast_unsigned(),
            Self::Unsigned(value) => *value,
        }
    }

    fn into_reports(self) -> Reports<'a> {
        match self {
            Self::Error(reports) => reports,
            _ => Reports::default(),
        }
    }

    fn chain_errors(self, other: Self) -> Self {
        Self::Error(self.into_reports().chain(other.into_reports()))
    }

    fn cmp(self, other: Self) -> Result<Ordering, Reports<'a>> {
        match (&self, &other) {
            (Self::Error(_), _) | (_, Self::Error(_)) =>
                Err(self.into_reports().chain(other.into_reports())),
            (Self::Signed(lhs), Self::Signed(rhs)) => Ok(lhs.cmp(rhs)),
            (lhs, rhs) => Ok(lhs.as_u64().cmp(&rhs.as_u64())),
        }
    }

    fn logical_negate(self) -> Self {
        match self {
            Self::Error(_) => self,
            Self::Signed(value) => (value == 0).into(),
            Self::Unsigned(value) => (value == 0).into(),
        }
    }

    fn logical_and(self, rhs: Value<'a>) -> Value<'a> {
        match self.into_bool() {
            Ok(true) => rhs.into_bool().into(),
            Ok(false) => false.into(),
            Err(reports) => Self::Error(reports).chain_errors(rhs),
        }
    }

    fn logical_or(self, rhs: Value<'a>) -> Value<'a> {
        match self.into_bool() {
            Ok(true) => true.into(),
            Ok(false) => rhs.into_bool().into(),
            Err(reports) => Self::Error(reports).chain_errors(rhs),
        }
    }
}

impl<'a> Neg for Value<'a> {
    type Output = Result<Self, EvalError>;

    fn neg(self) -> Self::Output {
        Ok(match self {
            Self::Error(_) => self,
            Self::Signed(value) =>
                Self::Signed(value.checked_neg().ok_or(EvalError::SignedOverflow)?),
            Self::Unsigned(value) => Self::Unsigned(value.wrapping_neg()),
        })
    }
}

impl<'a> Not for Value<'a> {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Error(_) => self,
            Self::Signed(value) => Self::Signed(!value),
            Self::Unsigned(value) => Self::Unsigned(!value),
        }
    }
}

impl<'a> Mul for Value<'a> {
    type Output = Result<Self, EvalError>;

    fn mul(self, rhs: Self) -> Self::Output {
        Ok(match (&self, &rhs) {
            (Self::Error(_), _) | (_, Self::Error(_)) => self.chain_errors(rhs),
            (Self::Signed(lhs), Self::Signed(rhs)) =>
                Self::Signed(lhs.checked_mul(*rhs).ok_or(EvalError::SignedOverflow)?),
            (lhs, rhs) => Self::Unsigned(lhs.as_u64().wrapping_mul(rhs.as_u64())),
        })
    }
}

impl<'a> Div for Value<'a> {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        if rhs.as_u64() == 0 {
            todo!("error: division by zero")
        }
        else {
            match (&self, &rhs) {
                (Self::Error(_), _) | (_, Self::Error(_)) => self.chain_errors(rhs),
                (Self::Signed(lhs), Self::Signed(rhs)) =>
                    Self::Signed(lhs.checked_div(*rhs).expect("TODO: signed overflow error")),
                (lhs, rhs) => Self::Unsigned(lhs.as_u64().wrapping_div(rhs.as_u64())),
            }
        }
    }
}

impl<'a> Rem for Value<'a> {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        if rhs.as_u64() == 0 {
            todo!("error: division by zero")
        }
        else {
            match (&self, &rhs) {
                (Self::Error(_), _) | (_, Self::Error(_)) => self.chain_errors(rhs),
                (Self::Signed(lhs), Self::Signed(rhs)) =>
                    Self::Signed(lhs.checked_rem(*rhs).expect("TODO: signed overflow error")),
                (lhs, rhs) => Self::Unsigned(lhs.as_u64().wrapping_rem(rhs.as_u64())),
            }
        }
    }
}

impl<'a> Add for Value<'a> {
    type Output = Result<Self, EvalError>;

    fn add(self, rhs: Self) -> Self::Output {
        Ok(match (&self, &rhs) {
            (Self::Error(_), _) | (_, Self::Error(_)) => self.chain_errors(rhs),
            (Self::Signed(lhs), Self::Signed(rhs)) =>
                Self::Signed(lhs.checked_add(*rhs).ok_or(EvalError::SignedOverflow)?),
            (lhs, rhs) => Self::Unsigned(lhs.as_u64().wrapping_add(rhs.as_u64())),
        })
    }
}

impl<'a> Sub for Value<'a> {
    type Output = Result<Self, EvalError>;

    fn sub(self, rhs: Self) -> Self::Output {
        Ok(match (&self, &rhs) {
            (Self::Error(_), _) | (_, Self::Error(_)) => self.chain_errors(rhs),
            (Self::Signed(lhs), Self::Signed(rhs)) =>
                Self::Signed(lhs.checked_sub(*rhs).ok_or(EvalError::SignedOverflow)?),
            (lhs, rhs) => Self::Unsigned(lhs.as_u64().wrapping_sub(rhs.as_u64())),
        })
    }
}

fn check_shift_rhs_is_valid(rhs: Value) -> Result<u64, Reports> {
    match rhs {
        Value::Error(reports) => Err(reports),
        Value::Signed(rhs) if (0..64).contains(&rhs) => Ok(rhs.cast_unsigned()),
        Value::Unsigned(rhs) if (0..64).contains(&rhs) => Ok(rhs),
        _ => todo!("error: shift rhs is invalid"),
    }
}

impl<'a> Shl for Value<'a> {
    type Output = Self;

    fn shl(self, rhs: Self) -> Self::Output {
        let rhs = match check_shift_rhs_is_valid(rhs) {
            Ok(rhs) => rhs,
            Err(reports) => return self.chain_errors(Self::Error(reports)),
        };
        match self {
            Self::Error(_) => self,
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

impl<'a> Shr for Value<'a> {
    type Output = Self;

    fn shr(self, rhs: Self) -> Self::Output {
        let rhs = match check_shift_rhs_is_valid(rhs) {
            Ok(rhs) => rhs,
            Err(reports) => return self.chain_errors(Self::Error(reports)),
        };
        let rhs = u32::try_from(rhs).unwrap();
        match self {
            Self::Error(_) => self,
            Self::Signed(value) => Self::Signed(value.wrapping_shr(rhs)),
            Self::Unsigned(value) => Self::Unsigned(value.wrapping_shr(rhs)),
        }
    }
}

impl<'a> BitAnd for Value<'a> {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Self::Error(_), _) | (_, Self::Error(_)) => self.chain_errors(rhs),
            (Self::Signed(lhs), Self::Signed(rhs)) => Self::Signed(lhs & rhs),
            (lhs, rhs) => Self::Unsigned(lhs.as_u64() & rhs.as_u64()),
        }
    }
}

impl<'a> BitXor for Value<'a> {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Self::Error(_), _) | (_, Self::Error(_)) => self.chain_errors(rhs),
            (Self::Signed(lhs), Self::Signed(rhs)) => Self::Signed(lhs ^ rhs),
            (lhs, rhs) => Self::Unsigned(lhs.as_u64() ^ rhs.as_u64()),
        }
    }
}

impl<'a> BitOr for Value<'a> {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Self::Error(_), _) | (_, Self::Error(_)) => self.chain_errors(rhs),
            (Self::Signed(lhs), Self::Signed(rhs)) => Self::Signed(lhs | rhs),
            (lhs, rhs) => Self::Unsigned(lhs.as_u64() | rhs.as_u64()),
        }
    }
}

impl<'a> From<bool> for Value<'a> {
    fn from(value: bool) -> Self {
        Self::Signed(value.into())
    }
}

impl<'a> From<Result<bool, Reports<'a>>> for Value<'a> {
    fn from(value: Result<bool, Reports<'a>>) -> Self {
        match value {
            Ok(b) => b.into(),
            Err(reports) => Self::Error(reports),
        }
    }
}

fn ctx<'a>(value: Result<Value<'a>, EvalError>, expr: &Expression<'a>) -> Value<'a> {
    match value {
        Ok(value) => value,
        Err(kind) => Value::Error(Reports::new(Diagnostic::EvalError { at: *expr, kind })),
    }
}

fn eval_binop<'a>(
    expr: &Expression<'a>,
    op: &BinOp,
    lhs: &Expression<'a>,
    rhs: &Expression<'a>,
) -> Value<'a> {
    let lhs = eval(lhs);
    let rhs = eval(rhs);
    match op.kind {
        BinOpKind::Multiply => ctx(lhs * rhs, expr),
        BinOpKind::Divide => lhs / rhs,
        BinOpKind::Modulo => lhs % rhs,
        BinOpKind::Add => ctx(lhs + rhs, expr),
        BinOpKind::Subtract => ctx(lhs - rhs, expr),
        BinOpKind::Equal => lhs.cmp(rhs).map(Ordering::is_eq).into(),
        BinOpKind::NotEqual => lhs.cmp(rhs).map(Ordering::is_ne).into(),
        BinOpKind::Less => lhs.cmp(rhs).map(Ordering::is_lt).into(),
        BinOpKind::LessEqual => lhs.cmp(rhs).map(Ordering::is_le).into(),
        BinOpKind::Greater => lhs.cmp(rhs).map(Ordering::is_gt).into(),
        BinOpKind::GreaterEqual => lhs.cmp(rhs).map(Ordering::is_ge).into(),
        BinOpKind::LeftShift => lhs << rhs,
        BinOpKind::RightShift => lhs >> rhs,
        BinOpKind::BitAnd => lhs & rhs,
        BinOpKind::BitXor => lhs ^ rhs,
        BinOpKind::BitOr => lhs | rhs,
    }
}

fn eval_unary_op<'a>(
    expr: &Expression<'a>,
    operator: &UnaryOp,
    operand: &Expression<'a>,
) -> Value<'a> {
    let value = eval(operand);
    match operator.kind {
        UnaryOpKind::Addressof => todo!("error: there are no lvalues"),
        UnaryOpKind::Deref => todo!("error: not a pointer"),
        UnaryOpKind::Plus => value,
        UnaryOpKind::Negate => ctx(-value, expr),
        UnaryOpKind::Compl => !value,
        UnaryOpKind::Not => value.logical_negate(),
        UnaryOpKind::Sizeof => unreachable!("starts with an identifier"),
        UnaryOpKind::Lengthof => unreachable!("starts with an identifier"),
    }
}

fn eval_logical_op<'a>(op: &LogicalOp, lhs: &Expression<'a>, rhs: &Expression<'a>) -> Value<'a> {
    match op.kind {
        LogicalOpKind::And => eval(lhs).logical_and(eval(rhs)),
        LogicalOpKind::Or => eval(lhs).logical_or(eval(rhs)),
    }
}

pub(super) fn eval<'a>(expr: &Expression<'a>) -> Value<'a> {
    match expr {
        Expression::Error(_report) => unreachable!("the parser does not produce this expr kind"),
        Expression::Name(_token) =>
            unreachable!("we have expanded all macros and replaced all non-macros with 0"),
        Expression::Integer(token) => {
            let TokenKind::Integer(Integer { suffix, suffix_len, base, prefix_len }) = token.kind
            else {
                unreachable!()
            };

            let from_u64 = match suffix {
                IntegerSuffix::Unsigned
                | IntegerSuffix::UnsignedLong
                | IntegerSuffix::UnsignedLongLong => Value::Unsigned,
                IntegerSuffix::None | IntegerSuffix::Long | IntegerSuffix::LongLong =>
                    |value| i64::try_from(value).map_or(Value::Unsigned(value), Value::Signed),
                IntegerSuffix::BitInt => todo!(),
                IntegerSuffix::UnsignedBitInt => todo!(),
                IntegerSuffix::Invalid => todo!(),
            };

            let slice = token.slice();
            let number = &slice[prefix_len..slice.len() - suffix_len];
            let number: String = number.chars().filter(|&c| c != '\'').collect();
            match u64::from_str_radix(&number, base) {
                Ok(value) => from_u64(value),
                Err(_) => todo!(),
            }
        }
        Expression::CharConstant(_token) => todo!(),
        Expression::String(_tokens) => todo!("error: apparently not allowed?"),
        Expression::Parenthesised { open_paren: _, expr, close_paren: _ } => eval(expr),
        Expression::Assign { .. } => unreachable!("prevented by grammar"),
        Expression::CompoundAssign { .. } => unreachable!("prevented by grammar"),
        Expression::BinOp { lhs, op, rhs } => eval_binop(expr, op, lhs, rhs),
        Expression::UnaryOp { operator, operand } => eval_unary_op(expr, operator, operand),
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
        } => {
            let condition = eval(condition);
            let then = eval(then);
            let or_else = eval(or_else);
            match condition.into_bool() {
                Ok(true) => then,
                Ok(false) => or_else,
                Err(reports) => Value::Error(reports),
            }
        }
        Expression::Comma { .. } => todo!("not allowed"),
        Expression::Increment { .. } => todo!("not allowed"),
    }
}
