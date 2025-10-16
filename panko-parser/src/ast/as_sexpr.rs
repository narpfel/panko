#![coverage(off)]

use super::Type;
use crate::ast::CompoundStatement;
use crate::ast::Declaration;
use crate::ast::ExternalDeclaration;
use crate::ast::FunctionDefinition;
use crate::ast::ParameterDeclaration;
use crate::ast::QualifiedType;
use crate::ast::Statement;
use crate::ast::TranslationUnit;
use crate::sexpr_builder::AsSExpr;
use crate::sexpr_builder::SExpr;

impl AsSExpr for TranslationUnit<'_> {
    fn as_sexpr(&self) -> SExpr {
        let Self { filename, decls } = self;
        SExpr::new("translation-unit")
            .inherit(filename)
            .lines(*decls)
    }
}

impl AsSExpr for ExternalDeclaration<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            ExternalDeclaration::FunctionDefinition(def) => def.as_sexpr(),
            ExternalDeclaration::Declaration(decl) => decl.as_sexpr(),
            ExternalDeclaration::Error(_error) => SExpr::string("error"),
        }
    }
}

impl AsSExpr for FunctionDefinition<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("function-definition")
            .inherit(&self.name)
            .lines([&self.ty])
            .lines([&self.body])
    }
}

impl AsSExpr for Declaration<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("declaration")
            .inherit(&self.name)
            .inherit(&self.ty)
            .inherit(&self.initialiser)
    }
}

impl AsSExpr for QualifiedType<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::display(self)
    }
}

impl AsSExpr for Type<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::display(self)
    }
}

impl AsSExpr for ParameterDeclaration<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("param").inherit(&self.name).inherit(&self.ty)
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
