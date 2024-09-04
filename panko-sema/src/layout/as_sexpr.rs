use std::iter;

use itertools::Either;
use panko_parser::sexpr_builder::AsSExpr;
use panko_parser::sexpr_builder::SExpr;

use super::CompoundStatement;
use super::Declaration;
use super::Expression;
use super::ExternalDeclaration;
use super::FunctionDefinition;
use super::ImplicitConversion;
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
        self.expr.as_sexpr().inherit(&self.ty).inherit(&self.slot)
    }
}

impl AsSExpr for Expression<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            Expression::Name(reference) => SExpr::string(&reference.unique_name()),
            Expression::Integer(int) => SExpr::string(int.slice()),
            Expression::ImplicitConversion(implicit_conversion) => implicit_conversion.as_sexpr(),
        }
    }
}

impl AsSExpr for ImplicitConversion<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("implicit-conversion").inherit(&self.from)
    }
}

impl AsSExpr for Reference<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::string(&format!("{} `{}`", self.unique_name(), self.ty,)).inherit(&self.slot)
    }
}

impl AsSExpr for Slot<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            Slot::Static(name) => SExpr::string("static").inline_string(name.to_string()),
            Slot::Automatic(offset) => SExpr::string(&format!("@{offset}")),
        }
    }
}
