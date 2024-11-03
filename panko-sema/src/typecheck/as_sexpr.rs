use std::iter;

use itertools::Either;
use panko_parser::sexpr_builder::AsSExpr;
use panko_parser::sexpr_builder::SExpr;

use super::CompoundStatement;
use super::Declaration;
use super::Expression;
use super::ExternalDeclaration;
use super::FunctionDefinition;
use super::Statement;
use super::TranslationUnit;
use super::TypedExpression;

impl AsSExpr for TranslationUnit<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("translation-unit").lines(self.decls)
    }
}

impl AsSExpr for ExternalDeclaration<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            ExternalDeclaration::FunctionDefinition(def) => def.as_sexpr(),
            ExternalDeclaration::Declaration(decl) => decl.as_sexpr(),
        }
    }
}

impl AsSExpr for FunctionDefinition<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("function-definition")
            .lines([&self.reference])
            .lines(if self.params.0.is_empty() {
                Either::Left(iter::empty())
            }
            else {
                Either::Right(iter::once(&self.params))
            })
            .lines([&self.body])
    }
}

impl AsSExpr for Declaration<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new(self.reference.kind().str())
            .inherit(&self.reference)
            .inherit(&self.initialiser)
    }
}

impl AsSExpr for CompoundStatement<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("compound-statement").lines(self.0)
    }
}

impl AsSExpr for Statement<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            Statement::Declaration(decl) => decl.as_sexpr(),
            Statement::Expression(expr) => SExpr::new("expression").inherit(expr),
            Statement::Compound(compound_statement) => compound_statement.as_sexpr(),
            Statement::Return(expr) => SExpr::new("return").inherit(expr),
        }
    }
}

impl AsSExpr for TypedExpression<'_> {
    fn as_sexpr(&self) -> SExpr {
        self.expr.as_sexpr().inherit(&self.ty)
    }
}

impl AsSExpr for Expression<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            Expression::Error(_error) => SExpr::new("error"),
            Expression::Name(reference) => SExpr::string(reference.unique_name()),
            Expression::Integer { value: _, token } => SExpr::string(token.slice()),
            Expression::NoopTypeConversion(expr) =>
                SExpr::new("noop-type-conversion").inherit(expr),
            Expression::Truncate(truncate) => SExpr::new("truncate").inherit(truncate),
            Expression::SignExtend(sign_extend) => SExpr::new("sign-extend").inherit(sign_extend),
            Expression::ZeroExtend(zero_extend) => SExpr::new("zero-extend").inherit(zero_extend),
            Expression::VoidCast(expr) => SExpr::new("void-cast").inherit(expr),
            Expression::Parenthesised { open_paren: _, expr, close_paren: _ } => expr.as_sexpr(),
            Expression::Assign { target, value } =>
                SExpr::new("assign").inherit(target).inherit(value),
            Expression::IntegralBinOp { ty: _, lhs, op, rhs } =>
                SExpr::new(op.str()).inherit(lhs).inherit(rhs),
            Expression::PtrAdd {
                pointer,
                integral,
                pointee_size: _,
                order,
            } => {
                let (lhs, rhs) = order.select(pointer, integral);
                SExpr::new("ptr-add").inherit(lhs).inherit(rhs)
            }
            Expression::PtrSub { pointer, integral, pointee_size: _ } =>
                SExpr::new("ptr-sub").inherit(pointer).inherit(integral),
            Expression::PtrDiff { lhs, rhs, pointee_size: _ } =>
                SExpr::new("ptr-diff").inherit(lhs).inherit(rhs),
            Expression::PtrCmp { lhs, kind, rhs } =>
                SExpr::new(kind.str()).inherit(lhs).inherit(rhs),
            Expression::Addressof { ampersand: _, operand } =>
                SExpr::new("addressof").inherit(operand),
            Expression::Deref { star: _, operand } => SExpr::new("deref").inherit(operand),
            Expression::Call {
                callee,
                args,
                #[expect(unused)]
                is_varargs,
                close_paren: _,
            } => SExpr::new("call").inherit(callee).lines(*args),
            Expression::Negate { minus: _, operand } => SExpr::new("negate").inherit(operand),
            Expression::Compl { compl: _, operand } => SExpr::new("compl").inherit(operand),
            Expression::Not { not: _, operand } => SExpr::new("not").inherit(operand),
            Expression::Sizeof { sizeof: _, operand, size } => SExpr::new("sizeof")
                .inline_string(size.to_string())
                .inherit(operand),
            Expression::SizeofTy { sizeof: _, ty, size, close_paren: _ } => SExpr::new("sizeof")
                .inline_string(size.to_string())
                .inherit(ty),
            Expression::Alignof { alignof: _, ty, align, close_paren: _ } => SExpr::new("alignof")
                .inline_string(align.to_string())
                .inherit(ty),
            Expression::Combine { first, second } => SExpr::new("combine").lines([first, second]),
            Expression::Logical { lhs, op, rhs } =>
                SExpr::new(format!("logical-{}", op.str())).lines([lhs, rhs]),
            Expression::Conditional { condition, then, or_else } =>
                SExpr::new("conditional").lines([condition, then, or_else]),
        }
    }
}
