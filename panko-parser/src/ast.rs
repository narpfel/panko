use std::iter::once;

use bumpalo::Bump;
use itertools::Either;
use itertools::Itertools as _;
use panko_lex::Token;

use crate as cst;
use crate::CompoundStatement;
use crate::DirectDeclarator;
use crate::TypeQualifier;
use crate::TypeQualifierKind;
use crate::TypeSpecifierKind;
use crate::TypeSpecifierQualifier::Qualifier;
use crate::TypeSpecifierQualifier::Specifier;

mod as_sexpr;

#[derive(Debug, Clone, Copy)]
pub struct TranslationUnit<'a> {
    decls: &'a [ExternalDeclaration<'a>],
}

#[derive(Debug, Clone, Copy)]
enum ExternalDeclaration<'a> {
    FunctionDefinition(FunctionDefinition<'a>),
    Declaration(Declaration<'a>),
}

#[derive(Debug, Clone, Copy)]
struct Declaration<'a> {
    ty: QualifiedType<'a>,
    name: Token<'a>,
    initialiser: Option<cst::Initialiser<'a>>,
}

#[expect(unused)]
#[derive(Debug, Clone, Copy)]
struct FunctionDefinition<'a> {
    name: Token<'a>,
    storage_class: cst::StorageClassSpecifier<'a>,
    inline: Option<cst::FunctionSpecifier<'a>>,
    noreturn: Option<cst::FunctionSpecifier<'a>>,
    ty: QualifiedType<'a>,
    body: CompoundStatement<'a>,
}

#[derive(Debug, Clone, Copy)]
struct QualifiedType<'a> {
    is_const: bool,
    is_volatile: bool,
    ty: Type<'a>,
}

#[derive(Debug, Clone, Copy)]
enum Type<'a> {
    Integral(Integral),
    #[expect(unused)]
    Char,
    Pointer(&'a QualifiedType<'a>),
    #[expect(unused)]
    Function(FunctionType<'a>),
    // TODO
}

#[derive(Debug, Clone, Copy)]
struct Integral {
    signedness: Option<Signedness>,
    kind: IntegralKind,
}

#[derive(Debug, Clone, Copy)]
enum IntegralKind {
    #[expect(unused)]
    /// explicitly `signed char` or `unsigned char`
    Char,
    #[expect(unused)]
    Short,
    Int,
    #[expect(unused)]
    Long,
    #[expect(unused)]
    LongLong,
}

#[derive(Debug, Clone, Copy)]
enum Signedness {
    #[expect(unused)]
    Signed,
    #[expect(unused)]
    Unsigned,
}

#[derive(Debug, Clone, Copy)]
struct FunctionType<'a> {
    params: &'a [QualifiedType<'a>],
    return_type: &'a QualifiedType<'a>,
}

impl<'a> ExternalDeclaration<'a> {
    fn from_parse_tree(
        bump: &'a Bump,
        decl: &'a cst::ExternalDeclaration<'a>,
    ) -> Either<impl Iterator<Item = Self>, impl Iterator<Item = Self>> {
        match decl {
            cst::ExternalDeclaration::FunctionDefinition(def) => Either::Left(once(
                ExternalDeclaration::FunctionDefinition(FunctionDefinition::from_parse_tree(def)),
            )),
            cst::ExternalDeclaration::Declaration(decl) => Either::Right(
                Declaration::from_parse_tree(bump, decl).map(ExternalDeclaration::Declaration),
            ),
        }
    }
}

impl FunctionDefinition<'_> {
    fn from_parse_tree(#[expect(unused)] def: &cst::FunctionDefinition) -> Self {
        todo!()
    }
}

impl<'a> Declaration<'a> {
    fn from_parse_tree(
        bump: &'a Bump,
        decl: &'a cst::Declaration<'a>,
    ) -> impl Iterator<Item = Self> + 'a {
        let specifiers = decl.specifiers.0;
        let mut is_const = false;
        let mut is_volatile = false;
        let mut ty = None;
        for specifier in specifiers {
            match specifier {
                cst::DeclarationSpecifier::StorageClass(storage_class) =>
                    unimplemented!("{storage_class:#?}"),
                cst::DeclarationSpecifier::TypeSpecifierQualifier(Specifier(specifier)) =>
                    match specifier.kind {
                        TypeSpecifierKind::Int =>
                            ty = Some(Type::Integral(Integral {
                                signedness: None,
                                kind: IntegralKind::Int,
                            })),
                        _ => unimplemented!("{specifier:#?}"),
                    },
                cst::DeclarationSpecifier::TypeSpecifierQualifier(Qualifier(qualifier)) =>
                    qualifier.parse(&mut is_const, &mut is_volatile),
                cst::DeclarationSpecifier::FunctionSpecifier(function_specifier) =>
                    unimplemented!("{function_specifier:#?}"),
            }
        }

        let ty = ty.unwrap_or_else(|| unimplemented!("error: no type given in declaration"));
        let ty = QualifiedType { is_const, is_volatile, ty };
        decl.init_declarator_list.iter().map(move |declarator| {
            let mut ty = ty;
            for pointer in declarator.declarator.pointers.unwrap_or_default() {
                let mut is_const = false;
                let mut is_volatile = false;
                for qualifier in pointer.qualifiers {
                    qualifier.parse(&mut is_const, &mut is_volatile);
                }
                ty = QualifiedType {
                    is_const,
                    is_volatile,
                    ty: Type::Pointer(bump.alloc(ty)),
                };
            }
            let name = match declarator.declarator.direct_declarator {
                DirectDeclarator::Identifier(name) => name,
                direct_declarator => unimplemented!("{direct_declarator:#?}"),
            };
            Self {
                ty,
                name,
                initialiser: declarator.initialiser,
            }
        })
    }
}

impl TypeQualifier<'_> {
    fn parse(self, is_const: &mut bool, is_volatile: &mut bool) {
        match self.kind {
            TypeQualifierKind::Const =>
                if *is_const {
                    unimplemented!("duplicate `const` in declaration");
                }
                else {
                    *is_const = true;
                },
            TypeQualifierKind::Volatile =>
                if *is_volatile {
                    unimplemented!("duplicate `volatile` in declaration");
                }
                else {
                    *is_volatile = true;
                },
            _ => unimplemented!("{self:#?}"),
        }
    }
}

pub(crate) fn from_parse_tree<'a>(
    bump: &'a Bump,
    parse_tree: cst::TranslationUnit<'a>,
) -> TranslationUnit<'a> {
    TranslationUnit {
        decls: bump.alloc_slice_copy(
            &parse_tree
                .decls
                .iter()
                .flat_map(|decl| ExternalDeclaration::from_parse_tree(bump, decl))
                .collect_vec(),
        ),
    }
}
