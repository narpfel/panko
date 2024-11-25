use std::cell::Ref;
use std::cell::RefCell;
use std::fmt;
use std::iter::once;

use ariadne::Color::Blue;
use ariadne::Color::Red;
use bumpalo::Bump;
use itertools::Either;
use itertools::Itertools as _;
use panko_lex::Loc;
use panko_lex::Token;
use panko_report::Report;

use crate as cst;
use crate::sexpr_builder::AsSExpr as _;
use crate::ArrayDeclarator;
use crate::BlockItem;
use crate::DirectDeclarator;
use crate::FunctionDeclarator;
use crate::InitDeclarator;
use crate::JumpStatement;
use crate::PrimaryBlock;
use crate::TypeQualifier;
use crate::TypeQualifierKind;
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

pub trait ErrorExpr<'a> {
    fn from_error(error: &'a dyn Report) -> Self;
}

impl ErrorExpr<'_> for () {
    fn from_error(_error: &dyn Report) -> Self {}
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

    pub fn alloc_str(&self, s: &str) -> &'a str {
        self.bump.alloc_str(s)
    }

    pub fn emit<T, Expr>(&self, diagnostic: T) -> Expr
    where
        T: Report + 'a,
        Expr: ErrorExpr<'a>,
    {
        let diagnostic = self.alloc(diagnostic);
        self.diagnostics.borrow_mut().push(diagnostic);
        Expr::from_error(diagnostic)
    }

    fn diagnostics(&self) -> Ref<Vec<&'a dyn Report>> {
        // FIXME: maybe `self.diagnostics` should be a `BinaryHeap` so it is naturally sorted?
        self.diagnostics
            .borrow_mut()
            .sort_by_key(|diagnostic| diagnostic.location().start());
        self.diagnostics.borrow()
    }

    pub fn handle_diagnostics(&self) {
        let diagnostics = self.diagnostics();
        if !diagnostics.is_empty() {
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
            std::process::exit(exit_code.into())
        }
    }
}

pub type Expression<'a> = cst::Expression<'a>;
pub type GenericAssociation<'a> = cst::GenericAssociation<'a>;

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
    pub loc: Loc<'a>,
}

#[derive(Debug, Clone, Copy)]
pub enum Type<'a> {
    Arithmetic(Arithmetic),
    Pointer(&'a QualifiedType<'a>),
    Array(ArrayType<'a>),
    Function(FunctionType<'a>),
    Void,
    // TODO
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Arithmetic {
    Integral(Integral),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Integral {
    pub signedness: Signedness,
    pub kind: IntegralKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntegralKind {
    PlainChar,
    /// explicitly `signed char` or `unsigned char`
    Char,
    Short,
    Int,
    Long,
    LongLong,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Signedness {
    Signed,
    Unsigned,
}

#[derive(Debug, Clone, Copy)]
pub struct ArrayType<'a> {
    pub ty: &'a QualifiedType<'a>,
    pub length: Option<&'a Expression<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionType<'a> {
    pub params: &'a [ParameterDeclaration<'a>],
    pub return_type: &'a QualifiedType<'a>,
    pub is_varargs: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct ParameterDeclaration<'a> {
    pub loc: Loc<'a>,
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
    Return {
        return_: Token<'a>,
        expr: Option<Expression<'a>>,
    },
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
            Type::Arithmetic(Arithmetic::Integral(integral)) => write!(f, "{integral}"),
            Type::Pointer(pointee) => write!(f, "ptr<{pointee}>"),
            Type::Array(array) => write!(f, "{array}"),
            Type::Function(function) => write!(f, "{function}"),
            Type::Void => write!(f, "void"),
        }
    }
}

impl fmt::Display for Integral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { kind, signedness } = self;
        if let (IntegralKind::Char, signedness) | (_, signedness @ Signedness::Unsigned) =
            (kind, signedness)
        {
            write!(f, "{signedness} ")?;
        }
        write!(f, "{}", kind)
    }
}

impl fmt::Display for ArrayType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { ty, length } = self;
        write!(f, "array<{ty}; {length}>", length = length.as_sexpr())
    }
}

impl fmt::Display for FunctionType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { params, return_type, is_varargs } = *self;
        let maybe_ellipsis = match (is_varargs, params.is_empty()) {
            (true, true) => "...",
            (true, false) => ", ...",
            _ => "",
        };
        write!(
            f,
            "fn({}{}) -> {}",
            params.iter().format_with(", ", |param, f| f(&format_args!(
                "{}: {}",
                param.name.map_or(NO_VALUE, |name| name.slice()),
                param.ty,
            ))),
            maybe_ellipsis,
            return_type,
        )
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
                    sess.emit(Diagnostic::DuplicateDeclarationSpecifier { at: self, first })
                }
                else {
                    *const_qualifier = Some(self);
                },
            TypeQualifierKind::Volatile =>
                if let Some(first) = *volatile_qualifier {
                    sess.emit(Diagnostic::DuplicateDeclarationSpecifier { at: self, first })
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
            UnlabeledStatement::JumpStatement(JumpStatement::Return { return_, expr }) =>
                Self::Return { return_: *return_, expr: *expr },
        }
    }
}

impl Arithmetic {
    pub fn size(&self) -> u64 {
        match self {
            Arithmetic::Integral(Integral { signedness: _, kind }) => kind.size(),
        }
    }

    pub fn signedness(&self) -> Signedness {
        match self {
            Arithmetic::Integral(Integral { signedness, kind: _ }) => *signedness,
        }
    }

    pub fn conversion_rank(&self) -> u64 {
        match self {
            Arithmetic::Integral(Integral {
                signedness: _,
                kind: IntegralKind::Char | IntegralKind::PlainChar,
            }) => 1,
            Arithmetic::Integral(Integral { signedness: _, kind: IntegralKind::Short }) => 2,
            Arithmetic::Integral(Integral { signedness: _, kind: IntegralKind::Int }) => 3,
            Arithmetic::Integral(Integral { signedness: _, kind: IntegralKind::Long }) => 4,
            Arithmetic::Integral(Integral {
                signedness: _,
                kind: IntegralKind::LongLong,
            }) => 5,
        }
    }
}

impl Integral {
    pub fn size(&self) -> u64 {
        self.kind.size()
    }

    pub fn can_represent<T>(&self, value: T) -> bool
    where
        i8: TryFrom<T>,
        i16: TryFrom<T>,
        i32: TryFrom<T>,
        i64: TryFrom<T>,
        u8: TryFrom<T>,
        u16: TryFrom<T>,
        u32: TryFrom<T>,
        u64: TryFrom<T>,
    {
        match (self.signedness, self.kind) {
            (Signedness::Signed, IntegralKind::PlainChar) => i8::try_from(value).is_ok(),
            (Signedness::Signed, IntegralKind::Char) => i8::try_from(value).is_ok(),
            (Signedness::Signed, IntegralKind::Short) => i16::try_from(value).is_ok(),
            (Signedness::Signed, IntegralKind::Int) => i32::try_from(value).is_ok(),
            (Signedness::Signed, IntegralKind::Long) => i64::try_from(value).is_ok(),
            (Signedness::Signed, IntegralKind::LongLong) => i64::try_from(value).is_ok(),
            (Signedness::Unsigned, IntegralKind::PlainChar) => unreachable!(),
            (Signedness::Unsigned, IntegralKind::Char) => u8::try_from(value).is_ok(),
            (Signedness::Unsigned, IntegralKind::Short) => u16::try_from(value).is_ok(),
            (Signedness::Unsigned, IntegralKind::Int) => u32::try_from(value).is_ok(),
            (Signedness::Unsigned, IntegralKind::Long) => u64::try_from(value).is_ok(),
            (Signedness::Unsigned, IntegralKind::LongLong) => u64::try_from(value).is_ok(),
        }
    }
}

impl IntegralKind {
    fn size(&self) -> u64 {
        match self {
            IntegralKind::PlainChar => 1,
            IntegralKind::Char => 1,
            IntegralKind::Short => 2,
            IntegralKind::Int => 4,
            IntegralKind::Long => 8,
            IntegralKind::LongLong => 8,
        }
    }
}

impl fmt::Display for IntegralKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IntegralKind::PlainChar => write!(f, "char"),
            IntegralKind::Char => write!(f, "char"),
            IntegralKind::Short => write!(f, "short"),
            IntegralKind::Int => write!(f, "int"),
            IntegralKind::Long => write!(f, "long"),
            IntegralKind::LongLong => write!(f, "long long"),
        }
    }
}

impl fmt::Display for Signedness {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Signedness::Signed => write!(f, "signed"),
            Signedness::Unsigned => write!(f, "unsigned"),
        }
    }
}

pub(crate) fn parse_type_specifiers<'a>(
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
                specifier.parse(sess, &mut ty),
            cst::DeclarationSpecifier::TypeSpecifierQualifier(Qualifier(qualifier)) =>
                qualifier.parse(sess, &mut const_qualifier, &mut volatile_qualifier),
            cst::DeclarationSpecifier::FunctionSpecifier(function_specifier) =>
                todo!("{function_specifier:#?}"),
        }
    }

    let ty = ty.unwrap_or_else(|| {
        // TODO: implement `ErrorExpr` for `Type`
        let () = sess.emit(Diagnostic::DeclarationWithoutType { at: specifiers });
        // FIXME: don’t use implicit int here, an explicit “type error” type will be better
        Type::Arithmetic(Arithmetic::Integral(Integral {
            signedness: Signedness::Signed,
            kind: IntegralKind::Int,
        }))
    });
    QualifiedType {
        is_const: const_qualifier.is_some(),
        is_volatile: volatile_qualifier.is_some(),
        ty,
        loc: specifiers.loc(),
    }
}

pub(crate) fn parse_declarator<'a>(
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
                loc: pointer.star.loc().until(ty.loc),
            };
        }
        match declarator.direct_declarator {
            DirectDeclarator::Abstract => break None,
            DirectDeclarator::Identifier(name) => break Some(name),
            DirectDeclarator::Parenthesised(decl) => declarator = *decl,
            DirectDeclarator::ArrayDeclarator(ArrayDeclarator {
                direct_declarator,
                type_qualifiers,
                length,
                close_bracket,
            }) => {
                if !type_qualifiers.is_empty() {
                    todo!("array in function parameter not implemented");
                }
                declarator = cst::Declarator {
                    pointers: None,
                    direct_declarator: *direct_declarator,
                };
                ty = QualifiedType {
                    is_const: false,
                    is_volatile: false,
                    ty: Type::Array(ArrayType {
                        ty: sess.alloc(ty),
                        length: try { sess.alloc(length?) },
                    }),
                    loc: ty.loc.until(close_bracket.loc()),
                }
            }
            DirectDeclarator::FunctionDeclarator(FunctionDeclarator {
                direct_declarator,
                parameter_type_list,
                close_paren,
            }) => {
                declarator = cst::Declarator {
                    pointers: None,
                    direct_declarator: *direct_declarator,
                };
                ty = QualifiedType {
                    is_const: false,
                    is_volatile: false,
                    ty: Type::Function(FunctionType {
                        params: sess.alloc_slice_fill_iter(
                            parameter_type_list.parameter_list.iter().map(|param| {
                                let ty = parse_type_specifiers(sess, param.declaration_specifiers);
                                let (ty, name) =
                                    param.declarator.map_or((ty, None), |declarator| {
                                        parse_declarator(sess, ty, declarator)
                                    });
                                let loc = name.map_or_else(
                                    || param.declaration_specifiers.loc(),
                                    |name| name.loc(),
                                );
                                ParameterDeclaration { loc, ty, name }
                            }),
                        ),
                        is_varargs: parameter_type_list.is_varargs,
                        return_type: sess.alloc(ty),
                    }),
                    loc: ty.loc.until(close_paren.loc()),
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
