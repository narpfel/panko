use std::cell::Ref;
use std::cell::RefCell;
use std::fmt;
use std::iter::once;
use std::process::ExitCode;

use ariadne::Color::Blue;
use ariadne::Color::Red;
use bumpalo::Bump;
use itertools::Either;
use itertools::Itertools as _;
use panko_lex::Token;
use panko_report::Report;

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

#[derive(Debug, Clone, Copy, Report)]
#[exit_code(1)]
enum Diagnostic<'a> {
    #[error("declaration with duplicate `{first}` declaration specifier")]
    #[diagnostics(
        first(colour = Blue, label = "first `{first}` here"),
        at(colour = Red, label = "help: remove this `{at}`"),
    )]
    DuplicateDeclarationSpecifier {
        first: TypeQualifier<'a>,
        at: TypeQualifier<'a>,
    },

    #[error("declaration does not specify a type")]
    #[diagnostics(at(colour = Red, label = "type missing"))]
    DeclarationWithoutType { at: cst::DeclarationSpecifiers<'a> },
}

type Diagnostics<'a> = RefCell<Vec<&'a dyn Report>>;

#[derive(Debug)]
pub struct Session<'a> {
    pub(crate) bump: &'a Bump,
    diagnostics: Diagnostics<'a>,
}

impl<'a> Session<'a> {
    pub fn new(bump: &'a Bump) -> Self {
        Self {
            bump,
            diagnostics: Diagnostics::default(),
        }
    }

    pub fn alloc<T>(&self, value: T) -> &'a T {
        self.bump.alloc(value)
    }

    pub fn alloc_slice_fill_iter<I, T>(&self, value: I) -> &'a [T]
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
    {
        self.bump.alloc_slice_fill_iter(value)
    }

    pub fn alloc_slice_copy<T>(&self, values: &[T]) -> &'a [T]
    where
        T: Copy,
    {
        self.bump.alloc_slice_copy(values)
    }

    pub fn emit<T>(&self, diagnostic: T)
    where
        T: Report + 'a,
    {
        self.diagnostics.borrow_mut().push(self.alloc(diagnostic))
    }

    fn diagnostics(&self) -> Ref<Vec<&'a dyn Report>> {
        // FIXME: maybe `self.diagnostics` should be a `BinaryHeap` so it is naturally sorted?
        self.diagnostics
            .borrow_mut()
            .sort_by_key(|diagnostic| diagnostic.location().start());
        self.diagnostics.borrow()
    }

    pub fn handle_diagnostics(&self) -> Result<(), ExitCode> {
        let diagnostics = self.diagnostics();
        if diagnostics.is_empty() {
            Ok(())
        }
        else {
            for (i, diagnostic) in diagnostics.iter().enumerate() {
                if i != 0 {
                    eprintln!();
                }
                diagnostic.print();
            }
            let exit_code = diagnostics
                .iter()
                .map(|diagnostic| diagnostic.exit_code())
                .max()
                .unwrap();
            // TODO: not all diagnostics are fatal
            Err(ExitCode::from(exit_code))
        }
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

// TODO: The instances for `PartialEq` and `Eq` are only needed until correct type checking is
// implemented and should be removed afterwards.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct QualifiedType<'a> {
    pub is_const: bool,
    pub is_volatile: bool,
    pub ty: Type<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type<'a> {
    Integral(Integral),
    Char,
    Pointer(&'a QualifiedType<'a>),
    Function(FunctionType<'a>),
    // TODO
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Integral {
    pub signedness: Option<Signedness>,
    pub kind: IntegralKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntegralKind {
    /// explicitly `signed char` or `unsigned char`
    Char,
    Short,
    Int,
    Long,
    LongLong,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Signedness {
    Signed,
    Unsigned,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionType<'a> {
    pub params: &'a [ParameterDeclaration<'a>],
    pub return_type: &'a QualifiedType<'a>,
}

#[derive(Debug, Clone, Copy)]
pub struct ParameterDeclaration<'a> {
    pub ty: QualifiedType<'a>,
    pub name: Option<Token<'a>>,
}

impl PartialEq for ParameterDeclaration<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

impl Eq for ParameterDeclaration<'_> {}

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
                    sess.emit(Diagnostic::DuplicateDeclarationSpecifier { at: self, first });
                }
                else {
                    *const_qualifier = Some(self);
                },
            TypeQualifierKind::Volatile =>
                if let Some(first) = *volatile_qualifier {
                    sess.emit(Diagnostic::DuplicateDeclarationSpecifier { at: self, first });
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

    let ty = ty.unwrap_or_else(|| {
        sess.emit(Diagnostic::DeclarationWithoutType { at: specifiers });
        // FIXME: don’t use implicit int here, an explicit “type error” type will be better
        Type::Integral(Integral {
            signedness: None,
            kind: IntegralKind::Int,
        })
    });
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
