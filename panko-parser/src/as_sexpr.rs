#![coverage(off)]

use crate::sexpr_builder::AsSExpr;
use crate::sexpr_builder::SExpr;
use crate::ArrayDeclarator;
use crate::BlockItem;
use crate::CompoundStatement;
use crate::Declaration;
use crate::DeclarationSpecifier;
use crate::DeclarationSpecifiers;
use crate::Declarator;
use crate::DirectDeclarator;
use crate::Expression;
use crate::ExpressionStatement;
use crate::ExternalDeclaration;
use crate::FunctionDeclarator;
use crate::FunctionDefinition;
use crate::FunctionSpecifier;
use crate::GenericAssociation;
use crate::InitDeclarator;
use crate::Initialiser;
use crate::JumpStatement;
use crate::ParameterDeclaration;
use crate::Pointer;
use crate::PrimaryBlock;
use crate::StorageClassSpecifier;
use crate::TranslationUnit;
use crate::TypeQualifier;
use crate::TypeSpecifier;
use crate::TypeSpecifierQualifier;
use crate::UnlabeledStatement;
use crate::NO_VALUE;

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

impl AsSExpr for Declaration<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("declaration")
            .inherit(&self.specifiers)
            .short_inline_explicit_empty(self.init_declarator_list)
    }
}

impl AsSExpr for DeclarationSpecifiers<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("declaration-specifiers").inherit_many_explicit_empty(self.0)
    }
}

impl AsSExpr for DeclarationSpecifier<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            DeclarationSpecifier::StorageClass(storage_class) => storage_class.as_sexpr(),
            DeclarationSpecifier::TypeSpecifierQualifier(type_specifier_qualifier) =>
                type_specifier_qualifier.as_sexpr(),
            DeclarationSpecifier::FunctionSpecifier(function_specifier) =>
                function_specifier.as_sexpr(),
        }
    }
}

impl AsSExpr for StorageClassSpecifier<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::string(self.token.slice())
    }
}

impl AsSExpr for TypeSpecifierQualifier<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            TypeSpecifierQualifier::Qualifier(qualifier) => qualifier.as_sexpr(),
            TypeSpecifierQualifier::Specifier(specifier) => specifier.as_sexpr(),
        }
    }
}

impl AsSExpr for TypeQualifier<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::string(self.token.slice())
    }
}

impl AsSExpr for TypeSpecifier<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::string(self.token.slice())
    }
}

impl AsSExpr for FunctionSpecifier<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::string(self.token.slice())
    }
}

impl AsSExpr for InitDeclarator<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("init-declarator")
            .inherit(&self.declarator)
            .inherit(&self.initialiser)
    }
}

impl AsSExpr for Initialiser<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            Self::Braced { open_brace: _, close_brace: _ } =>
                SExpr::new("braced").inherit(&NO_VALUE),
            Self::Expression(expr) => expr.as_sexpr(),
        }
    }
}

impl AsSExpr for Declarator<'_> {
    fn as_sexpr(&self) -> SExpr {
        match &self.pointers {
            None => self.direct_declarator.as_sexpr(),
            Some(pointers) => pointers.as_sexpr().inherit(&self.direct_declarator),
        }
    }
}

impl AsSExpr for Pointer<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("pointer").inherit_many(self.qualifiers)
    }
}

impl AsSExpr for DirectDeclarator<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            DirectDeclarator::Abstract => ().as_sexpr(),
            DirectDeclarator::Identifier(ident) => SExpr::string(ident.slice()),
            DirectDeclarator::Parenthesised(declarator) => declarator.as_sexpr(),
            DirectDeclarator::ArrayDeclarator(array_declarator) => array_declarator.as_sexpr(),
            DirectDeclarator::FunctionDeclarator(function_declarator) =>
                function_declarator.as_sexpr(),
        }
    }
}

impl AsSExpr for ArrayDeclarator<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("array-declarator")
            .inherit(&self.length)
            .inherit(self.direct_declarator)
    }
}

impl AsSExpr for FunctionDeclarator<'_> {
    fn as_sexpr(&self) -> SExpr {
        let sexpr = SExpr::new("function-declarator").inherit(self.direct_declarator);
        let sexpr = if self.parameter_type_list.is_varargs {
            sexpr.inherit(&"varargs")
        }
        else {
            sexpr
        };
        sexpr.lines(self.parameter_type_list.parameter_list)
    }
}

impl AsSExpr for ParameterDeclaration<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("param")
            .inherit(&self.declaration_specifiers)
            .inherit(&self.declarator)
    }
}

impl AsSExpr for FunctionDefinition<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("function-definition")
            .inherit(&self.declaration_specifiers)
            .inherit(&self.declarator)
            .inherit(&self.body)
    }
}

impl AsSExpr for CompoundStatement<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("compound-statement").lines(self.0)
    }
}

impl AsSExpr for BlockItem<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            BlockItem::Declaration(decl) => decl.as_sexpr(),
            BlockItem::UnlabeledStatement(unlabeled_statement) => unlabeled_statement.as_sexpr(),
        }
    }
}

impl AsSExpr for UnlabeledStatement<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            UnlabeledStatement::ExpressionStatement(stmt) => stmt.as_sexpr(),
            UnlabeledStatement::PrimaryBlock(block) => block.as_sexpr(),
            UnlabeledStatement::JumpStatement(jump) => jump.as_sexpr(),
        }
    }
}

impl AsSExpr for ExpressionStatement<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("expression").inherit(&self.0)
    }
}

impl AsSExpr for PrimaryBlock<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            PrimaryBlock::CompoundStatement(stmt) => SExpr::new("primary-block").inherit(stmt),
        }
    }
}

impl AsSExpr for JumpStatement<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            JumpStatement::Return { return_: _, expr } => SExpr::new("return").inherit(expr),
        }
    }
}

impl AsSExpr for Expression<'_> {
    fn as_sexpr(&self) -> SExpr {
        match self {
            Expression::Name(name) => SExpr::new("name").inline_string(name.slice().to_owned()),
            Expression::Integer(int) => SExpr::string(int.slice()),
            Expression::Parenthesised { open_paren: _, expr, close_paren: _ } => expr.as_sexpr(),
            Expression::Assign { target, value } =>
                SExpr::new("assign").inherit(target).inherit(value),
            Expression::CompoundAssign { target, op, value } =>
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
            Expression::Conditional { condition, then, or_else } =>
                SExpr::new("conditional").lines([condition, then, or_else]),
            Expression::Comma { lhs, rhs } => SExpr::new("comma").lines([lhs, rhs]),
            Expression::Increment { operator, operand, fixity } =>
                SExpr::new(format!("{}-{}", fixity.str(), operator.str())).inherit(operand),
        }
    }
}

impl AsSExpr for GenericAssociation<'_> {
    fn as_sexpr(&self) -> SExpr {
        let (ty, expr): (&dyn AsSExpr, _) = match self {
            Self::Ty { ty, expr } => (ty, expr),
            Self::Default { default, expr } => (default, expr),
        };
        SExpr::new("assoc").inherit(ty).inherit(expr)
    }
}
