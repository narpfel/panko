use std::borrow::Cow;
use std::iter;

use itertools::Either;
use panko_parser::sexpr_builder::AsSExpr;
use panko_parser::sexpr_builder::SExpr;

use super::CompoundStatement;
use super::Declaration;
use super::DesignatedInitialiser;
use super::Designation;
use super::Designator;
use super::Expression;
use super::ExternalDeclaration;
use super::FunctionDefinition;
use super::GenericAssociation;
use super::Initialiser;
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

impl AsSExpr for Initialiser<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            Self::Braced {
                open_brace: _,
                initialiser_list,
                close_brace: _,
            } => SExpr::new("braced").lines_explicit_empty(*initialiser_list),
            Self::Expression(expr) => expr.as_sexpr(),
        }
    }
}

impl AsSExpr for DesignatedInitialiser<'_> {
    fn as_sexpr(&self) -> SExpr {
        let Self { designation, initialiser } = self;
        match designation {
            Some(designation) => designation.as_sexpr().inherit(initialiser),
            None => initialiser.as_sexpr(),
        }
    }
}

impl AsSExpr for Designation<'_> {
    fn as_sexpr(&self) -> SExpr {
        let Self(designators) = self;
        SExpr::new("designation").inherit(designators)
    }
}

impl AsSExpr for Designator<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            Self::Bracketed { open_bracket: _, index, close_bracket: _ } =>
                SExpr::new("index").inherit(index),
            Self::Identifier { dot: _, ident } => SExpr::new("member").inherit(ident),
        }
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
            Expression::Error(_error) => SExpr::new("error"),
            Expression::Name(name) => SExpr::new("name").inherit(name),
            Expression::Integer { value, token: _ } => SExpr::string(Cow::Borrowed(*value)),
            Expression::CharConstant(char) => SExpr::string(char.slice()),
            Expression::String(tokens) => SExpr::new("string").inherit_many(*tokens),
            Expression::Parenthesised { open_paren: _, expr, close_paren: _ } => expr.as_sexpr(),
            Expression::Assign { target, value } =>
                SExpr::new("assign").inherit(target).inherit(value),
            Expression::CompoundAssign { target, target_temporary: _, op, value } =>
                SExpr::new(format!("{}-assign", op.str()))
                    .inherit(target)
                    .inherit(value),
            Expression::BinOp { lhs, op, rhs } => SExpr::new(op.str()).inherit(lhs).inherit(rhs),
            Expression::UnaryOp { operator, operand } =>
                SExpr::new(operator.str()).inherit(operand),
            Expression::Call { callee, args, close_paren: _ } =>
                SExpr::new("call").inherit(callee).lines(*args),
            Expression::Sizeof { sizeof: _, ty, close_paren: _ } =>
                SExpr::new("sizeof").inherit(ty),
            Expression::Lengthof { lengthof: _, ty, close_paren: _ } =>
                SExpr::new("lengthof").inherit(ty),
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
            Expression::Logical { lhs, op, rhs } =>
                SExpr::new(format!("logical-{}", op.str())).lines([lhs, rhs]),
            Expression::Conditional {
                condition,
                question_mark: _,
                then,
                or_else,
            } => SExpr::new("conditional").lines([condition, then, or_else]),
            Expression::Comma { lhs, rhs } => SExpr::new("comma").lines([lhs, rhs]),
            Expression::Increment { operator, operand, fixity, reference: _ } =>
                SExpr::new(format!("{}-{}", fixity.str(), operator.str())).inherit(operand),
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
