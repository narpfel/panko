#![coverage(off)]

use crate::sexpr_builder::AsSExpr;
use crate::sexpr_builder::SExpr;
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
use crate::InitDeclarator;
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
            DirectDeclarator::FunctionDeclarator(function_declarator) =>
                function_declarator.as_sexpr(),
        }
    }
}

impl AsSExpr for FunctionDeclarator<'_> {
    fn as_sexpr(&self) -> SExpr {
        SExpr::new("function-declarator")
            .inherit(self.direct_declarator)
            .lines(self.parameter_type_list)
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
            Expression::BinOp { lhs, kind, rhs } =>
                SExpr::new(kind.str()).inherit(lhs).inherit(rhs),
        }
    }
}
