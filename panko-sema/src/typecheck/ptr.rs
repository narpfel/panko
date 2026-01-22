use std::assert_matches::assert_matches;

use panko_lex::Token;
use panko_parser::BinOp;
use panko_parser::Comparison;
use panko_parser::ast::Arithmetic;
use panko_parser::ast::Session;

use super::Expression;
use super::QualifiedType;
use super::Type;
use super::TypedExpression;
use super::convert_as_if_by_assignment;
use crate::typecheck::diagnostics::Diagnostic;

#[derive(Debug, Clone, Copy)]
pub enum PtrAddOrder {
    PtrFirst,
    IntegralFirst,
}

impl PtrAddOrder {
    pub fn select<T>(self, pointer: T, integral: T) -> (T, T) {
        match self {
            Self::PtrFirst => (pointer, integral),
            Self::IntegralFirst => (integral, pointer),
        }
    }
}

pub(crate) fn typeck_ptradd<'a>(
    sess: &'a Session<'a>,
    op: &BinOp<'a>,
    pointer: TypedExpression<'a>,
    pointee_ty: &QualifiedType<'a>,
    integral: TypedExpression<'a>,
    order: PtrAddOrder,
) -> TypedExpression<'a> {
    assert_matches!(pointer.ty.ty, Type::Pointer(_));
    assert_matches!(integral.ty.ty, Type::Arithmetic(Arithmetic::Integral(_)));
    if pointee_ty.ty.is_complete() {
        let integral = convert_as_if_by_assignment(sess, Type::size_t().unqualified(), integral);
        TypedExpression {
            ty: pointer.ty.make_unqualified(),
            expr: Expression::PtrAdd {
                pointer: sess.alloc(pointer),
                integral: sess.alloc(integral),
                pointee_size: pointee_ty.ty.size(),
                order,
            },
        }
    }
    else {
        let (lhs, rhs) = order.select(pointer, integral);
        sess.emit(Diagnostic::PointerArithmeticWithIncompletePointee {
            at: *op,
            pointee_ty: *pointee_ty,
            lhs,
            rhs,
        })
    }
}

pub(crate) fn typeck_ptrsub<'a>(
    sess: &'a Session<'a>,
    op: &BinOp<'a>,
    pointer: TypedExpression<'a>,
    pointee_ty: &QualifiedType<'a>,
    integral: TypedExpression<'a>,
) -> TypedExpression<'a> {
    assert_matches!(pointer.ty.ty, Type::Pointer(_));
    assert_matches!(integral.ty.ty, Type::Arithmetic(Arithmetic::Integral(_)));
    if pointee_ty.ty.is_complete() {
        let integral = convert_as_if_by_assignment(sess, Type::size_t().unqualified(), integral);
        TypedExpression {
            ty: pointer.ty.make_unqualified(),
            expr: Expression::PtrSub {
                pointer: sess.alloc(pointer),
                integral: sess.alloc(integral),
                pointee_size: pointee_ty.ty.size(),
            },
        }
    }
    else {
        sess.emit(Diagnostic::PointerArithmeticWithIncompletePointee {
            at: *op,
            pointee_ty: *pointee_ty,
            lhs: pointer,
            rhs: integral,
        })
    }
}

pub(crate) fn typeck_ptrcmp<'a>(
    sess: &'a Session<'a>,
    lhs: TypedExpression<'a>,
    kind: Comparison,
    op_token: &Token<'a>,
    rhs: TypedExpression<'a>,
) -> TypedExpression<'a> {
    // TODO: should check for type compatibility, not exact equality
    let expr = if lhs.ty.ty != Type::Nullptr && rhs.ty.ty != Type::Nullptr && lhs.ty.ty != rhs.ty.ty
    {
        sess.emit(Diagnostic::IncompatibleTypesInPtrCmp { at: *op_token, lhs, rhs })
    }
    else {
        Expression::PtrCmp {
            lhs: sess.alloc(lhs),
            kind,
            rhs: sess.alloc(rhs),
        }
    };
    TypedExpression { ty: Type::int().unqualified(), expr }
}

pub(crate) fn typeck_ptrdiff<'a>(
    sess: &'a Session<'a>,
    op: &BinOp<'a>,
    lhs: TypedExpression<'a>,
    lhs_pointee_ty: &QualifiedType<'a>,
    rhs: TypedExpression<'a>,
    rhs_pointee_ty: &QualifiedType<'a>,
) -> TypedExpression<'a> {
    // TODO: should check for type compatibility, not exact equality
    let expr = if lhs_pointee_ty.ty != rhs_pointee_ty.ty {
        sess.emit(Diagnostic::PtrDiffIncompatiblePointeeTypes { at: op.token, lhs, rhs })
    }
    else {
        match lhs_pointee_ty.ty.is_complete() && rhs_pointee_ty.ty.is_complete() {
            true => Expression::PtrDiff {
                lhs: sess.alloc(lhs),
                rhs: sess.alloc(rhs),
                pointee_size: lhs_pointee_ty.ty.size(),
            },
            // TODO: this assumes that both pointee types are the same; pass the actual incomplete
            // pointee ty here.
            false => sess.emit(Diagnostic::PointerArithmeticWithIncompletePointee {
                at: *op,
                pointee_ty: *lhs_pointee_ty,
                lhs,
                rhs,
            }),
        }
    };

    TypedExpression {
        ty: Type::ptrdiff_t().unqualified(),
        expr,
    }
}
