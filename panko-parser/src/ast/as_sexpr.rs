use crate::ast::CompoundStatement;
use crate::ast::Declaration;
use crate::ast::ExternalDeclaration;
use crate::ast::FunctionDefinition;
use crate::ast::FunctionType;
use crate::ast::Integral;
use crate::ast::ParameterDeclaration;
use crate::ast::QualifiedType;
use crate::ast::Signedness;
use crate::ast::Statement;
use crate::ast::TranslationUnit;
use crate::ast::Type;
use crate::sexpr_builder::AsSExpr;
use crate::sexpr_builder::SExpr;

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
        let mut sexpr = SExpr::new("qualified-type");
        if self.is_const {
            sexpr = sexpr.inline_string("const".to_owned());
        }
        if self.is_volatile {
            sexpr = sexpr.inline_string("volatile".to_owned());
        }
        sexpr.inherit(&self.ty)
    }
}

impl AsSExpr for Type<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            Type::Integral(int) => int.as_sexpr(),
            Type::Char => SExpr::string("char"),
            Type::Pointer(pointer) => SExpr::new("pointer").inherit(pointer),
            Type::Function(function) => function.as_sexpr(),
        }
    }
}

impl AsSExpr for Integral {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("integral")
            .inline_string(format!("{:?}", self.kind))
            .inherit(&self.signedness)
    }
}

impl AsSExpr for Signedness {
    fn as_sexpr(&self) -> SExpr {
        match self {
            Signedness::Signed => "signed".as_sexpr(),
            Signedness::Unsigned => "unsigned".as_sexpr(),
        }
    }
}

impl AsSExpr for FunctionType<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("function")
            .lines([&self.return_type])
            .inherit_many_explicit_empty(self.params)
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
            Statement::Return(expr) => SExpr::new("return").inherit(expr),
        }
    }
}
