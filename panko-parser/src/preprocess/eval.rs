use std::cmp::Ordering;
use std::collections::LinkedList;
use std::num::IntErrorKind;
use std::ops::Add;
use std::ops::BitAnd;
use std::ops::BitOr;
use std::ops::BitXor;
use std::ops::Mul;
use std::ops::Neg;
use std::ops::Not;
use std::ops::Sub;

use ariadne::Color::Blue;
use ariadne::Color::Red;
use ariadne::Fmt as _;
use panko_lex::Integer;
use panko_lex::IntegerSuffix;
use panko_lex::Loc;
use panko_lex::Token;
use panko_lex::TokenKind;
use panko_report::Report;
use panko_report::Sliced as _;

use crate::BinOp;
use crate::BinOpKind;
use crate::Expression;
use crate::IntegerLiteralDiagnostic;
use crate::LogicalOp;
use crate::LogicalOpKind;
use crate::UnaryOp;
use crate::UnaryOpKind;
use crate::ast::FromError;
use crate::ast::Session;
use crate::preprocess::tokens_loc;

#[derive(Debug, Clone, Copy)]
pub(super) enum EvalError {
    SignedOverflow,
    DivisionByZero,
    RemainderByZero,
    ShiftRhsTooLarge,
    NegativeShiftLhs,
    NegativeShiftRhs,
}

#[derive(Debug, Clone, Copy, Report)]
#[exit_code(1)]
pub(super) enum Diagnostic<'a> {
    #[error("error while evaluating constant expression: {kind:?}")]
    #[diagnostics(at(colour = Red, label = "while evaluating this expression"))]
    EvalError { at: Expression<'a>, kind: EvalError },

    #[error("unary `{op}` is not a preprocessor operator")]
    #[diagnostics(
        op(colour = Red, label = "unary operator `{op}` is not allowed in preprocessor expressions"),
        at(colour = Blue, label = "in this expression"),
    )]
    DisallowedOperator { at: Expression<'a>, op: Token<'a> },

    #[error("{kind} are not allowed in preprocessor expressions")]
    #[diagnostics(at(colour = Red))]
    #[with(kind = kind.fg(Red))]
    InvalidExpression { at: Loc<'a>, kind: &'a str },
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

    fn try_as_u64(&self) -> Option<u64> {
        match self {
            Self::Error(_) => None,
            Self::Signed(value) => Some(value.cast_unsigned()),
            Self::Unsigned(value) => Some(*value),
        }
    }

    fn as_u64(&self) -> u64 {
        self.try_as_u64().expect("called `as_u64` on an error")
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

    fn div(self, rhs: Self, expr: &Expression<'a>) -> Result<Self, EvalError> {
        let rhs = match rhs.try_as_u64() {
            Some(0) => ctx(Err(EvalError::DivisionByZero), expr),
            _ => rhs,
        };
        Ok(match (&self, &rhs) {
            (Self::Error(_), _) | (_, Self::Error(_)) => self.chain_errors(rhs),
            (Self::Signed(lhs), &Self::Signed(rhs)) =>
                Self::Signed(lhs.checked_div(rhs).ok_or(EvalError::SignedOverflow)?),
            (lhs, rhs) => Self::Unsigned(lhs.as_u64().wrapping_div(rhs.as_u64())),
        })
    }

    fn rem(self, rhs: Self, expr: &Expression<'a>) -> Result<Self, EvalError> {
        let rhs = match rhs.try_as_u64() {
            Some(0) => ctx(Err(EvalError::RemainderByZero), expr),
            _ => rhs,
        };
        Ok(match (&self, &rhs) {
            (Self::Error(_), _) | (_, Self::Error(_)) => self.chain_errors(rhs),
            (Self::Signed(lhs), Self::Signed(rhs)) =>
                Self::Signed(lhs.checked_rem(*rhs).ok_or(EvalError::SignedOverflow)?),
            (lhs, rhs) => Self::Unsigned(lhs.as_u64().wrapping_rem(rhs.as_u64())),
        })
    }

    fn shl(self, rhs: Self, expr: &Expression<'a>) -> Result<Self, EvalError> {
        let lhs = match self {
            Self::Signed(..0) => ctx(Err(EvalError::NegativeShiftLhs), expr),
            _ => self,
        };
        let rhs = check_shift_rhs_is_valid(rhs, expr);
        Ok(match (&lhs, &rhs) {
            (Self::Error(_), _) | (_, Self::Error(_)) => lhs.chain_errors(rhs),
            (Self::Signed(value), rhs) => Self::Signed(
                value
                    .checked_mul(1 << rhs.as_u64())
                    .ok_or(EvalError::SignedOverflow)?,
            ),
            (Self::Unsigned(value), rhs) =>
                Self::Unsigned(value.wrapping_shl(u32::try_from(rhs.as_u64()).unwrap())),
        })
    }

    fn shr(self, rhs: Self, expr: &Expression<'a>) -> Result<Self, EvalError> {
        let rhs = check_shift_rhs_is_valid(rhs, expr);
        let rhs_value = rhs.try_as_u64().map(|rhs| u32::try_from(rhs).unwrap());
        Ok(match (&self, rhs_value) {
            (Self::Error(_), _) | (_, None) => self.chain_errors(rhs),
            (Self::Signed(value), Some(rhs)) => Self::Signed(value.wrapping_shr(rhs)),
            (Self::Unsigned(value), Some(rhs)) => Self::Unsigned(value.wrapping_shr(rhs)),
        })
    }
}

impl<'a> FromError<'a> for Value<'a> {
    fn from_error(_error: &'a dyn Report) -> Self {
        Self::Error(Reports::default())
    }
}

fn check_shift_rhs_is_valid<'a>(rhs: Value<'a>, expr: &Expression<'a>) -> Value<'a> {
    match rhs {
        Value::Signed(..0) => ctx(Err(EvalError::NegativeShiftRhs), expr),
        Value::Signed(64..) | Value::Unsigned(64..) => ctx(Err(EvalError::ShiftRhsTooLarge), expr),
        rhs => rhs,
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
    sess: &Session<'a>,
    expr: &Expression<'a>,
    op: &BinOp,
    lhs: &Expression<'a>,
    rhs: &Expression<'a>,
) -> Value<'a> {
    let lhs = eval(sess, lhs);
    let rhs = eval(sess, rhs);
    match op.kind {
        BinOpKind::Multiply => ctx(lhs * rhs, expr),
        BinOpKind::Divide => ctx(lhs.div(rhs, expr), expr),
        BinOpKind::Modulo => ctx(lhs.rem(rhs, expr), expr),
        BinOpKind::Add => ctx(lhs + rhs, expr),
        BinOpKind::Subtract => ctx(lhs - rhs, expr),
        BinOpKind::Equal => lhs.cmp(rhs).map(Ordering::is_eq).into(),
        BinOpKind::NotEqual => lhs.cmp(rhs).map(Ordering::is_ne).into(),
        BinOpKind::Less => lhs.cmp(rhs).map(Ordering::is_lt).into(),
        BinOpKind::LessEqual => lhs.cmp(rhs).map(Ordering::is_le).into(),
        BinOpKind::Greater => lhs.cmp(rhs).map(Ordering::is_gt).into(),
        BinOpKind::GreaterEqual => lhs.cmp(rhs).map(Ordering::is_ge).into(),
        BinOpKind::LeftShift => ctx(lhs.shl(rhs, expr), expr),
        BinOpKind::RightShift => ctx(lhs.shr(rhs, expr), expr),
        BinOpKind::BitAnd => lhs & rhs,
        BinOpKind::BitXor => lhs ^ rhs,
        BinOpKind::BitOr => lhs | rhs,
    }
}

fn eval_unary_op<'a>(
    sess: &Session<'a>,
    expr: &Expression<'a>,
    operator: &UnaryOp<'a>,
    operand: &Expression<'a>,
) -> Value<'a> {
    let value = eval(sess, operand);
    match operator.kind {
        UnaryOpKind::Addressof | UnaryOpKind::Deref =>
            sess.emit(Diagnostic::DisallowedOperator { at: *expr, op: operator.token }),
        UnaryOpKind::Plus => value,
        UnaryOpKind::Negate => ctx(-value, expr),
        UnaryOpKind::Compl => !value,
        UnaryOpKind::Not => value.logical_negate(),
        UnaryOpKind::Sizeof => unreachable!("starts with an identifier"),
        UnaryOpKind::Lengthof => unreachable!("starts with an identifier"),
    }
}

fn eval_logical_op<'a>(
    sess: &Session<'a>,
    op: &LogicalOp,
    lhs: &Expression<'a>,
    rhs: &Expression<'a>,
) -> Value<'a> {
    match op.kind {
        LogicalOpKind::And => eval(sess, lhs).logical_and(eval(sess, rhs)),
        LogicalOpKind::Or => eval(sess, lhs).logical_or(eval(sess, rhs)),
    }
}

pub(super) fn eval<'a>(sess: &Session<'a>, expr: &Expression<'a>) -> Value<'a> {
    match expr {
        Expression::Error(report) => Value::from_error(*report),
        Expression::Name(_token) =>
            unreachable!("we have expanded all macros and replaced all non-macros with 0"),
        Expression::Integer(token) => {
            let TokenKind::Integer(Integer { suffix, suffix_len, base, prefix_len }) = token.kind
            else {
                unreachable!()
            };

            let slice = token.slice();

            let from_u64 = match suffix {
                IntegerSuffix::Unsigned
                | IntegerSuffix::UnsignedLong
                | IntegerSuffix::UnsignedLongLong => Value::Unsigned,
                IntegerSuffix::None | IntegerSuffix::Long | IntegerSuffix::LongLong =>
                    |value| i64::try_from(value).map_or(Value::Unsigned(value), Value::Signed),
                IntegerSuffix::BitInt => todo!(),
                IntegerSuffix::UnsignedBitInt => todo!(),
                IntegerSuffix::Invalid =>
                    return sess.emit(IntegerLiteralDiagnostic::InvalidSuffix { at: *token }),
            };

            let number = &slice[prefix_len..slice.len() - suffix_len];
            let number: String = number.chars().filter(|&c| c != '\'').collect();
            match u64::from_str_radix(&number, base) {
                Ok(value) => from_u64(value),
                Err(error) => match error.kind() {
                    IntErrorKind::PosOverflow =>
                        sess.emit(IntegerLiteralDiagnostic::TooLarge { at: *token }),
                    _ => unreachable!(),
                },
            }
        }
        Expression::CharConstant(_token) => todo!(),
        Expression::String(tokens) => sess.emit(Diagnostic::InvalidExpression {
            at: tokens_loc(tokens),
            kind: "string literals",
        }),
        Expression::Parenthesised { open_paren: _, expr, close_paren: _ } => eval(sess, expr),
        Expression::Assign { .. } => unreachable!("prevented by grammar"),
        Expression::CompoundAssign { .. } => unreachable!("prevented by grammar"),
        Expression::BinOp { lhs, op, rhs } => eval_binop(sess, expr, op, lhs, rhs),
        Expression::UnaryOp { operator, operand } => eval_unary_op(sess, expr, operator, operand),
        Expression::Call { .. } => todo!("always a type error"),
        Expression::Sizeof { .. } => unreachable!("starts with an identifier"),
        Expression::Lengthof { .. } => unreachable!("starts with an identifier"),
        Expression::Alignof { .. } => unreachable!("starts with an identifier"),
        Expression::Cast { .. } => unreachable!("the type name would contain an identifier"),
        Expression::Subscript { .. } => todo!("error: invalid op"),
        Expression::Generic { .. } => unreachable!("starts with an identifier"),
        Expression::Logical { lhs, op, rhs } => eval_logical_op(sess, op, lhs, rhs),
        Expression::Conditional {
            condition,
            question_mark: _,
            then,
            or_else,
        } => {
            let condition = eval(sess, condition);
            let then = eval(sess, then);
            let or_else = eval(sess, or_else);
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
