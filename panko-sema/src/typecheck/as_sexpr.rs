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
            Expression::Name(reference) => SExpr::string(reference.unique_name()),
            Expression::Integer(int) => SExpr::string(int.slice()),
            Expression::NoopTypeConversion(expr) =>
                SExpr::new("noop-type-conversion").inherit(expr),
            Expression::Truncate(truncate) => SExpr::new("truncate").inherit(truncate),
            Expression::SignExtend(sign_extend) => SExpr::new("sign-extend").inherit(sign_extend),
            Expression::ZeroExtend(zero_extend) => SExpr::new("zero-extend").inherit(zero_extend),
            Expression::Parenthesised { open_paren: _, expr, close_paren: _ } => expr.as_sexpr(),
            Expression::Assign { target, value } =>
                SExpr::new("assign").inherit(target).inherit(value),
            Expression::IntegralBinOp { ty: _, lhs, kind, rhs } =>
                SExpr::new(kind.str()).inherit(lhs).inherit(rhs),
            Expression::PtrAdd {
                pointer,
                integral,
                pointee_size: _,
                order,
            } => {
                let (lhs, rhs) = order.select(pointer, integral);
                SExpr::new("ptr-add").inherit(lhs).inherit(rhs)
            }
        }
    }
}
