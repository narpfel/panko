use std::iter;

use itertools::Either;
use panko_parser::sexpr_builder::AsSExpr;
use panko_parser::sexpr_builder::SExpr;

use super::CompoundStatement;
use super::Declaration;
use super::Expression;
use super::ExternalDeclaration;
use super::FunctionDefinition;
use super::LayoutedExpression;
use super::ParamRefs;
use super::Reference;
use super::Slot;
use super::Statement;
use super::TranslationUnit;

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

impl AsSExpr for ParamRefs<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("params").lines(self.0)
    }
}

impl AsSExpr for Declaration<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new(self.reference.kind.str())
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

impl AsSExpr for LayoutedExpression<'_> {
    fn as_sexpr(&self) -> SExpr {
        self.expr
            .as_sexpr()
            .inherit_front(&self.slot)
            .inherit_front(&self.ty)
    }
}

impl AsSExpr for Expression<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            Expression::Name(reference) => SExpr::string(reference.unique_name()),
            Expression::Integer(int) => SExpr::string(int.slice()),
            Expression::NoopTypeConversion(expr) =>
                SExpr::new("noop-type-conversion").inherit(expr),
            Expression::Truncate(from) => SExpr::new("truncate").inherit(from),
            Expression::SignExtend(from) => SExpr::new("sign-extend").inherit(from),
            Expression::ZeroExtend(from) => SExpr::new("zero-extend").inherit(from),
            Expression::Assign { target, value } => SExpr::new("assign").lines([target, value]),
            Expression::IntegralBinOp { ty: _, lhs, kind, rhs } =>
                SExpr::new(kind.str()).lines([lhs, rhs]),
            Expression::PtrAdd {
                pointer,
                integral,
                pointee_size: _,
                order,
            } => {
                let (lhs, rhs) = order.select(pointer, integral);
                SExpr::new("ptr-add").lines([lhs, rhs])
            }
            Expression::PtrSub { pointer, integral, pointee_size: _ } =>
                SExpr::new("ptr-sub").lines([pointer, integral]),
            Expression::PtrCmp { lhs, kind, rhs } => SExpr::new(kind.str()).lines([lhs, rhs]),
            Expression::Addressof(operand) => SExpr::new("addressof").inherit(operand),
            Expression::Deref(operand) => SExpr::new("deref").inherit(operand),
            Expression::Call {
                callee,
                args,
                #[expect(unused)]
                is_varargs,
            } => SExpr::new("call").inherit(callee).lines(*args),
        }
    }
}

impl AsSExpr for Reference<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new(format!("{} `{}`", self.unique_name(), self.ty)).inherit(&self.slot)
    }
}

impl AsSExpr for Slot<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            Slot::Static(name) => SExpr::new("static").inline_string(name.to_string()),
            Slot::Automatic(offset) => SExpr::string(format!("@{offset}")),
            Self::Void => SExpr::string("@void"),
        }
    }
}
