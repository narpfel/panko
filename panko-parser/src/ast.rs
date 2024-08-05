use std::cell::RefCell;
use std::fmt;
use std::iter::once;

use bumpalo::Bump;
use itertools::Either;
use itertools::Itertools as _;
use panko_lex::Token;

use crate as cst;
use crate::BlockItem;
use crate::DirectDeclarator;
use crate::InitDeclarator;
use crate::JumpStatement;
use crate::PrimaryBlock;
use crate::TypeQualifier;
use crate::TypeQualifierKind;
use crate::TypeSpecifierKind;
use crate::TypeSpecifierQualifier::Qualifier;
use crate::TypeSpecifierQualifier::Specifier;
use crate::UnlabeledStatement;
use crate::NO_VALUE;

mod as_sexpr;

#[derive(Debug, Clone, Copy)]
enum Diagnostic<'a> {
    DuplicateConst {
        #[expect(unused)]
        first: TypeQualifier<'a>,
        #[expect(unused)]
        repeated: TypeQualifier<'a>,
    },
    DuplicateVolatile {
        #[expect(unused)]
        first: TypeQualifier<'a>,
        #[expect(unused)]
        repeated: TypeQualifier<'a>,
    },
}

pub struct Session<'a> {
    pub(crate) bump: &'a Bump,
    diagnostics: Diagnostics<'a>,
}

#[derive(Default, Debug)]
pub struct Diagnostics<'a>(RefCell<Vec<Diagnostic<'a>>>);

impl Diagnostics<'_> {
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.borrow().is_empty()
    }
}

impl<'a> Session<'a> {
    pub fn new(bump: &'a Bump) -> Self {
        Self {
            bump,
            diagnostics: Diagnostics::default(),
        }
    }

    fn alloc<T>(&self, value: T) -> &'a T {
        self.bump.alloc(value)
    }

    fn alloc_slice_fill_iter<I, T>(&self, value: I) -> &'a [T]
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
    {
        self.bump.alloc_slice_fill_iter(value)
    }

    fn alloc_slice_copy<T>(&self, values: &[T]) -> &'a [T]
    where
        T: Copy,
    {
        self.bump.alloc_slice_copy(values)
    }

    fn emit(&self, diagnostic: Diagnostic<'a>) {
        self.diagnostics.0.borrow_mut().push(diagnostic)
    }

    pub fn diagnostics(&self) -> &Diagnostics<'a> {
        &self.diagnostics
    }
}

pub type Expression<'a> = cst::Expression<'a>;

#[derive(Debug, Clone, Copy)]
pub struct TranslationUnit<'a> {
    pub decls: &'a [ExternalDeclaration<'a>],
}

#[derive(Debug, Clone, Copy)]
pub enum ExternalDeclaration<'a> {
    FunctionDefinition(FunctionDefinition<'a>),
    Declaration(Declaration<'a>),
}

#[derive(Debug, Clone, Copy)]
pub struct Declaration<'a> {
    pub ty: QualifiedType<'a>,
    pub name: Token<'a>,
    pub initialiser: Option<Expression<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionDefinition<'a> {
    pub name: Token<'a>,
    pub storage_class: Option<cst::StorageClassSpecifier<'a>>,
    pub inline: Option<cst::FunctionSpecifier<'a>>,
    pub noreturn: Option<cst::FunctionSpecifier<'a>>,
    pub ty: QualifiedType<'a>,
    pub body: CompoundStatement<'a>,
}

#[derive(Debug, Clone, Copy)]
pub struct QualifiedType<'a> {
    pub is_const: bool,
    pub is_volatile: bool,
    pub ty: Type<'a>,
}

#[derive(Debug, Clone, Copy)]
pub enum Type<'a> {
    Integral(Integral),
    Char,
    Pointer(&'a QualifiedType<'a>),
    Function(FunctionType<'a>),
    // TODO
}

#[derive(Debug, Clone, Copy)]
pub struct Integral {
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
pub struct FunctionType<'a> {
    pub params: &'a [ParameterDeclaration<'a>],
    return_type: &'a QualifiedType<'a>,
}

#[derive(Debug, Clone, Copy)]
pub struct ParameterDeclaration<'a> {
    pub ty: QualifiedType<'a>,
    pub name: Option<Token<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub struct CompoundStatement<'a>(pub &'a [Statement<'a>]);

#[derive(Debug, Clone, Copy)]
pub enum Statement<'a> {
    // TODO: restrict the kinds of decls that are allowed in function scope?
    Declaration(Declaration<'a>),
    Expression(Option<Expression<'a>>),
    Compound(CompoundStatement<'a>),
    Return(Option<Expression<'a>>),
}

impl<'a> ExternalDeclaration<'a> {
    fn from_parse_tree(
        sess: &'a Session<'a>,
        decl: &'a cst::ExternalDeclaration<'a>,
    ) -> impl Iterator<Item = Self> {
        match decl {
            cst::ExternalDeclaration::FunctionDefinition(def) =>
                Either::Left(once(ExternalDeclaration::FunctionDefinition(
                    FunctionDefinition::from_parse_tree(sess, def),
                ))),
            cst::ExternalDeclaration::Declaration(decl) => Either::Right(
                Declaration::from_parse_tree(sess, decl).map(ExternalDeclaration::Declaration),
            ),
        }
    }
}

impl<'a> FunctionDefinition<'a> {
    fn from_parse_tree(sess: &'a Session<'a>, def: &cst::FunctionDefinition<'a>) -> Self {
        let cst::FunctionDefinition { declaration_specifiers, declarator, body } = *def;
        let ty = parse_type_specifiers(sess, declaration_specifiers);
        let (ty, name) = parse_declarator(sess, ty, declarator);
        let name =
            name.unwrap_or_else(|| unreachable!("[parser] syntax error: declaration without name"));
        Self {
            name,
            storage_class: None,
            inline: None,
            noreturn: None,
            ty,
            body: CompoundStatement::from_parse_tree(sess, &body),
        }
    }
}

impl fmt::Display for QualifiedType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.ty.fmt(f)?;
        if self.is_const {
            write!(f, " const")?;
        }
        if self.is_volatile {
            write!(f, " volatile")?;
        }
        Ok(())
    }
}

impl fmt::Display for Type<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Integral(integral) => write!(f, "{integral}"),
            Type::Char => write!(f, "char"),
            Type::Pointer(pointee) => write!(f, "ptr<{pointee}>"),
            Type::Function(function) => write!(f, "{function}"),
        }
    }
}

impl fmt::Display for Integral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(signedness) = self.signedness {
            write!(f, "{signedness:?}")?;
        }
        write!(f, "{:?}", self.kind)
    }
}

impl fmt::Display for FunctionType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn(")?;
        for (i, param) in self.params.iter().enumerate() {
            write!(
                f,
                "{}: {}",
                param.name.map_or(NO_VALUE, |name| name.slice()),
                param.ty,
            )?;
            if i != self.params.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ") -> {}", self.return_type)
    }
}

impl<'a> Declaration<'a> {
    fn from_parse_tree(
        sess: &'a Session<'a>,
        decl: &'a cst::Declaration<'a>,
    ) -> impl Iterator<Item = Self> + 'a {
        let ty = parse_type_specifiers(sess, decl.specifiers);
        decl.init_declarator_list
            .iter()
            .map(move |&InitDeclarator { declarator, initialiser }| {
                let (ty, name) = parse_declarator(sess, ty, declarator);
                let name = name.unwrap_or_else(|| {
                    unreachable!("[parser] syntax error: declaration without name")
                });
                Self { ty, name, initialiser }
            })
    }
}

impl<'a> TypeQualifier<'a> {
    fn parse(
        self,
        sess: &'a Session<'a>,
        const_qualifier: &mut Option<Self>,
        volatile_qualifier: &mut Option<Self>,
    ) {
        match self.kind {
            TypeQualifierKind::Const =>
                if let Some(first) = *const_qualifier {
                    sess.emit(Diagnostic::DuplicateConst { first, repeated: self });
                }
                else {
                    *const_qualifier = Some(self);
                },
            TypeQualifierKind::Volatile =>
                if let Some(first) = *volatile_qualifier {
                    sess.emit(Diagnostic::DuplicateVolatile { first, repeated: self });
                }
                else {
                    *volatile_qualifier = Some(self);
                },
            _ => todo!("{self:#?}"),
        }
    }
}

impl<'a> CompoundStatement<'a> {
    fn from_parse_tree(sess: &'a Session<'a>, stmt: &cst::CompoundStatement<'a>) -> Self {
        Self(
            sess.alloc_slice_copy(
                &stmt
                    .0
                    .iter()
                    .flat_map(|stmt| Statement::from_parse_tree(sess, stmt))
                    .collect_vec(),
            ),
        )
    }
}

impl<'a> Statement<'a> {
    fn from_parse_tree(
        sess: &'a Session<'a>,
        item: &'a cst::BlockItem<'a>,
    ) -> impl Iterator<Item = Self> + 'a {
        match item {
            BlockItem::Declaration(decl) =>
                Either::Left(Declaration::from_parse_tree(sess, decl).map(Self::Declaration)),
            BlockItem::UnlabeledStatement(stmt) =>
                Either::Right(once(Self::from_unlabeled_statement(sess, stmt))),
        }
    }

    fn from_unlabeled_statement(sess: &'a Session<'a>, stmt: &cst::UnlabeledStatement<'a>) -> Self {
        match stmt {
            UnlabeledStatement::ExpressionStatement(expr) => Self::Expression(expr.0),
            UnlabeledStatement::PrimaryBlock(PrimaryBlock::CompoundStatement(block)) =>
                Self::Compound(CompoundStatement::from_parse_tree(sess, block)),
            UnlabeledStatement::JumpStatement(JumpStatement::Return(expr)) => Self::Return(*expr),
        }
    }
}

fn parse_type_specifiers<'a>(
    sess: &'a Session<'a>,
    specifiers: cst::DeclarationSpecifiers<'a>,
) -> QualifiedType<'a> {
    let mut const_qualifier = None;
    let mut volatile_qualifier = None;
    let mut ty = None;
    for specifier in specifiers.0 {
        match specifier {
            cst::DeclarationSpecifier::StorageClass(storage_class) => todo!("{storage_class:#?}"),
            cst::DeclarationSpecifier::TypeSpecifierQualifier(Specifier(specifier)) =>
                match specifier.kind {
                    TypeSpecifierKind::Int =>
                        ty = Some(Type::Integral(Integral {
                            signedness: None,
                            kind: IntegralKind::Int,
                        })),
                    TypeSpecifierKind::Char => ty = Some(Type::Char),
                    _ => todo!("{specifier:#?}"),
                },
            cst::DeclarationSpecifier::TypeSpecifierQualifier(Qualifier(qualifier)) =>
                qualifier.parse(sess, &mut const_qualifier, &mut volatile_qualifier),
            cst::DeclarationSpecifier::FunctionSpecifier(function_specifier) =>
                todo!("{function_specifier:#?}"),
        }
    }

    let ty = ty.unwrap_or_else(|| todo!("error: no type given in declaration"));
    QualifiedType {
        is_const: const_qualifier.is_some(),
        is_volatile: volatile_qualifier.is_some(),
        ty,
    }
}

fn parse_declarator<'a>(
    sess: &'a Session<'a>,
    mut ty: QualifiedType<'a>,
    mut declarator: cst::Declarator<'a>,
) -> (QualifiedType<'a>, Option<Token<'a>>) {
    let name = loop {
        for pointer in declarator.pointers.unwrap_or_default() {
            let mut const_qualifier = None;
            let mut volatile_qualifier = None;
            for qualifier in pointer.qualifiers {
                qualifier.parse(sess, &mut const_qualifier, &mut volatile_qualifier);
            }
            ty = QualifiedType {
                is_const: const_qualifier.is_some(),
                is_volatile: volatile_qualifier.is_some(),
                ty: Type::Pointer(sess.alloc(ty)),
            };
        }
        match declarator.direct_declarator {
            DirectDeclarator::Abstract => break None,
            DirectDeclarator::Identifier(name) => break Some(name),
            DirectDeclarator::Parenthesised(decl) => declarator = *decl,
            DirectDeclarator::FunctionDeclarator(function_declarator) => {
                declarator = cst::Declarator {
                    pointers: None,
                    direct_declarator: *function_declarator.direct_declarator,
                };
                ty = QualifiedType {
                    is_const: false,
                    is_volatile: false,
                    ty: Type::Function(FunctionType {
                        params: sess.alloc_slice_fill_iter(
                            function_declarator.parameter_type_list.iter().map(|param| {
                                let ty = parse_type_specifiers(sess, param.declaration_specifiers);
                                let (ty, name) =
                                    param.declarator.map_or((ty, None), |declarator| {
                                        parse_declarator(sess, ty, declarator)
                                    });
                                ParameterDeclaration { ty, name }
                            }),
                        ),
                        return_type: sess.alloc(ty),
                    }),
                };
            }
        }
    };
    (ty, name)
}

pub(crate) fn from_parse_tree<'a>(
    sess: &'a Session<'a>,
    parse_tree: cst::TranslationUnit<'a>,
) -> TranslationUnit<'a> {
    TranslationUnit {
        decls: sess.alloc_slice_copy(
            &parse_tree
                .decls
                .iter()
                .flat_map(|decl| ExternalDeclaration::from_parse_tree(sess, decl))
                .collect_vec(),
        ),
    }
}
