use std::iter;

use itertools::Either;
use panko_parser::sexpr_builder::AsSExpr;
use panko_parser::sexpr_builder::SExpr;

use super::CompoundStatement;
use super::Declaration;
use super::Expression;
use super::ExternalDeclaration;
use super::FunctionDefinition;
use super::GenericAssociation;
use super::ParamRefs;
use super::Reference;
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
            Statement::Return { return_: _, expr } => SExpr::new("return").inherit(expr),
        }
    }
}

impl AsSExpr for Expression<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            Expression::Name(name) => SExpr::new("name").inherit(name),
            Expression::Integer(int) => SExpr::string(int.slice()),
            Expression::Parenthesised { open_paren: _, expr, close_paren: _ } => expr.as_sexpr(),
            Expression::Assign { target, value } =>
                SExpr::new("assign").inherit(target).inherit(value),
            Expression::BinOp { lhs, kind, rhs } =>
                SExpr::new(kind.str()).inherit(lhs).inherit(rhs),
            Expression::UnaryOp { operator, operand } =>
                SExpr::new(operator.str()).inherit(operand),
            Expression::Call { callee, args, close_paren: _ } =>
                SExpr::new("call").inherit(callee).lines(*args),
            Expression::Sizeof { sizeof: _, ty, close_paren: _ } =>
                SExpr::new("sizeof").inherit(ty),
            Expression::Alignof { alignof: _, ty, close_paren: _ } =>
                SExpr::new("alignof").inherit(ty),
            Expression::Cast { open_paren: _, ty, expr } =>
                SExpr::new("cast").inherit(ty).inherit(expr),
            Expression::Subscript { lhs, rhs, close_bracket: _ } =>
                SExpr::new("subscript").inherit(lhs).inherit(rhs),
            Expression::Generic {
                generic: _,
                selector,
                assocs,
                close_paren: _,
            } => SExpr::new("generic").lines([selector]).lines(assocs.0),
        }
    }
}

impl AsSExpr for Reference<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::string(format!("{} `{}`", self.unique_name(), self.ty))
    }
}

impl AsSExpr for GenericAssociation<'_> {
    fn as_sexpr(&self) -> SExpr {
        let (ty, expr): (&dyn AsSExpr, _) = match self {
            GenericAssociation::Ty { ty, expr } => (ty, expr),
            GenericAssociation::Default { default, expr } => (default, expr),
        };
        SExpr::new("assoc").inherit(ty).inherit(expr)
    }
}
