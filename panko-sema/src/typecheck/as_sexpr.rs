use std::iter;

use itertools::Either;
use panko_parser::NO_VALUE;
use panko_parser::sexpr_builder::AsSExpr;
use panko_parser::sexpr_builder::SExpr;

use super::ArrayLength;
use super::CompoundStatement;
use super::Declaration;
use super::Expression;
use super::ExternalDeclaration;
use super::FunctionDefinition;
use super::Initialiser;
use super::ParamRefs;
use super::Reference;
use super::Statement;
use super::SubobjectInitialiser;
use super::TranslationUnit;
use super::TypedExpression;
use super::Typedef;
use crate::ty::struct_decl_as_sexpr;

impl<Expression> AsSExpr for ArrayLength<Expression>
where
    Expression: AsSExpr,
{
    fn as_sexpr(&self) -> SExpr {
        match self {
            ArrayLength::Constant(length) => SExpr::string(length.to_string()),
            ArrayLength::Variable(length) => SExpr::new("variable").inherit(length),
            ArrayLength::Unknown => SExpr::string(NO_VALUE),
        }
    }
}

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
            ExternalDeclaration::StructDecl(decl) => struct_decl_as_sexpr(decl),
            ExternalDeclaration::FunctionDefinition(def) => def.as_sexpr(),
            ExternalDeclaration::Declaration(decl) => decl.as_sexpr(),
            ExternalDeclaration::Typedef(typedef) => typedef.as_sexpr(),
            ExternalDeclaration::ProvideExternalDefinitionForInlineFunction(name) =>
                SExpr::new("provide-external-definition").inline_string(name.to_string()),
            ExternalDeclaration::Error(_error) => SExpr::new("error"),
        }
    }
}

impl AsSExpr for Typedef<'_> {
    fn as_sexpr(&self) -> SExpr {
        let Self { ty, name } = self;
        SExpr::new("typedef")
            .inline_string(name.slice().to_owned())
            .inherit(ty)
    }
}

impl AsSExpr for FunctionDefinition<'_> {
    fn as_sexpr(&self) -> SExpr {
        self.reference
            .linkage()
            .in_sexpr(SExpr::new("function-definition"))
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
        self.reference
            .linkage()
            .in_sexpr(SExpr::new(self.reference.kind().str()))
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

impl AsSExpr for SubobjectInitialiser<'_> {
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
            Statement::StructDecl(decl) => struct_decl_as_sexpr(decl),
            Statement::Declaration(decl) => decl.as_sexpr(),
            Statement::Typedef(typedef) => typedef.as_sexpr(),
            Statement::Expression(expr) => SExpr::new("expression").inherit(expr),
            Statement::Compound(compound_statement) => compound_statement.as_sexpr(),
            Statement::Return(expr) => SExpr::new("return").inherit(expr),
        }
    }
}

impl AsSExpr for TypedExpression<'_> {
    fn as_sexpr(&self) -> SExpr {
        self.expr.as_sexpr().inherit_front(&self.ty)
    }
}

impl AsSExpr for Expression<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            Expression::Error(_error) => SExpr::new("error"),
            Expression::Name(reference) => SExpr::string(reference.unique_name()),
            Expression::Integer { value: _, token } => SExpr::string(token.slice()),
            Expression::String(string) =>
                SExpr::new("string").inline_string(format!("{:?}", string.value())),
            Expression::Nullptr(nullptr) => SExpr::string(nullptr.slice()),
            Expression::NoopTypeConversion(expr) =>
                SExpr::new("noop-type-conversion").inherit(expr),
            Expression::Truncate(truncate) => SExpr::new("truncate").inherit(truncate),
            Expression::SignExtend(sign_extend) => SExpr::new("sign-extend").inherit(sign_extend),
            Expression::ZeroExtend(zero_extend) => SExpr::new("zero-extend").inherit(zero_extend),
            Expression::VoidCast(expr) => SExpr::new("void-cast").inherit(expr),
            Expression::BoolCast(expr) => SExpr::new("bool-cast").inherit(expr),
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
            Expression::PtrCmp { lhs, kind, rhs } => SExpr::new(format!("ptr-{}", kind.str()))
                .inherit(lhs)
                .inherit(rhs),
            Expression::Addressof { ampersand: _, operand } =>
                SExpr::new("addressof").lines([operand]),
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
            Expression::Lengthof { lengthof: _, operand, length } => SExpr::new("lengthof")
                .inline_string(length.to_string())
                .inherit(operand),
            Expression::SizeofTy { sizeof: _, ty, size, close_paren: _ } => SExpr::new("sizeof")
                .inline_string(size.to_string())
                .inherit(ty),
            Expression::LengthofTy { lengthof: _, ty, length, close_paren: _ } =>
                SExpr::new("lengthof")
                    .inline_string(length.to_string())
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

impl AsSExpr for Reference<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::string(format!("{} `{}`", self.unique_name(), self.ty))
    }
}
