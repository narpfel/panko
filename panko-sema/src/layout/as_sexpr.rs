use std::iter;

use itertools::Either;
use panko_parser::sexpr_builder::AsSExpr;
use panko_parser::sexpr_builder::SExpr;
use yansi::Paint as _;

use super::CompoundStatement;
use super::Declaration;
use super::Expression;
use super::ExternalDeclaration;
use super::FunctionDefinition;
use super::Initialiser;
use super::LayoutedExpression;
use super::ParamRefs;
use super::Reference;
use super::Slot;
use super::Statement;
use super::SubobjectInitialiser;
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
            ExternalDeclaration::Typedef(typedef) => typedef.as_sexpr(),
            ExternalDeclaration::Error(_error) => SExpr::new("error"),
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
            Self::Braced { subobject_initialisers } =>
                SExpr::new("braced").lines_explicit_empty(*subobject_initialisers),
            Self::Expression(expr) => expr.as_sexpr(),
        }
    }
}

impl<Expression> AsSExpr for SubobjectInitialiser<'_, Expression>
where
    Expression: AsSExpr,
{
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("subobject")
            .inline_string(format!("+{}", self.subobject.offset))
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
            Statement::Typedef(typedef) => typedef.as_sexpr(),
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
            Expression::Error(error) => SExpr::new("error").inline_string(format!(
                "\"{}\"",
                error
                    .to_string()
                    .lines()
                    .next()
                    .unwrap_or("")
                    .escape_debug(),
            )),
            Expression::Name(reference) => SExpr::string(reference.unique_name()),
            Expression::Integer(int) => SExpr::string(int.to_string()),
            Expression::String(string) =>
                SExpr::new("string").inline_string(format!("{string:?}").blue().bold().to_string()),
            Expression::NoopTypeConversion(expr) =>
                SExpr::new("noop-type-conversion").inherit(expr),
            Expression::Truncate(from) => SExpr::new("truncate").inherit(from),
            Expression::SignExtend(from) => SExpr::new("sign-extend").inherit(from),
            Expression::ZeroExtend(from) => SExpr::new("zero-extend").inherit(from),
            Expression::VoidCast(from) => SExpr::new("void-cast").inherit(from),
            Expression::Assign { target, value } => SExpr::new("assign").lines([target, value]),
            Expression::IntegralBinOp { ty: _, lhs, op, rhs } =>
                SExpr::new(op.str()).lines([lhs, rhs]),
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
            Expression::PtrDiff { lhs, rhs, pointee_size: _ } =>
                SExpr::new("ptr-diff").lines([lhs, rhs]),
            Expression::PtrCmp { lhs, kind, rhs } => SExpr::new(kind.str()).lines([lhs, rhs]),
            Expression::Addressof(operand) => SExpr::new("addressof").lines([operand]),
            Expression::Deref(operand) => SExpr::new("deref").inherit(operand),
            Expression::Call {
                callee,
                args,
                #[expect(unused)]
                is_varargs,
            } => SExpr::new("call").inherit(callee).lines(*args),
            Expression::Negate(operand) => SExpr::new("negate").inherit(operand),
            Expression::Compl(operand) => SExpr::new("compl").inherit(operand),
            Expression::Not(operand) => SExpr::new("not").inherit(operand),
            Expression::Combine { first, second } => SExpr::new("combine").lines([first, second]),
            Expression::Logical { lhs, op, rhs } =>
                SExpr::new(format!("logical-{}", op.str())).lines([lhs, rhs]),
            Expression::Conditional { condition, then, or_else } =>
                SExpr::new("conditional").lines([condition, then, or_else]),
        }
    }
}

impl AsSExpr for Reference<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new(self.unique_name())
            .inherit(&self.ty)
            .inherit(&self.slot)
    }
}

impl AsSExpr for Slot<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            Slot::Static(name) => SExpr::new("static").inline_string(name.to_string()),
            Slot::Automatic(offset) => SExpr::string(format!("@{offset}")),
            Self::Void => SExpr::string("@void"),
            Self::StaticWithOffset { name, offset } => SExpr::new("static")
                .inline_string(name.to_string())
                .inline_string(format!("+{offset}")),
        }
    }
}
