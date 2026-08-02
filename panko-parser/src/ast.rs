use std::cell::Ref;
use std::cell::RefCell;
use std::fmt;
use std::path::Path;

use ariadne::Color::Blue;
use ariadne::Color::Red;
use ariadne::Fmt as _;
use itertools::Either;
use panko_lex::Bump;
use panko_lex::Loc;
use panko_lex::Token;
use panko_report::Report;
use panko_report::Sliced as _;

use crate as cst;
use crate::BlockItem;
pub use crate::DesignatedInitialiser;
pub use crate::InitDeclarator;
pub use crate::Initialiser;
use crate::JumpStatement;
use crate::NO_VALUE;
use crate::PrimaryBlock;
use crate::StorageClassSpecifierKind;
use crate::StructKind;
use crate::TypeName;
use crate::TypeQualifier;
use crate::TypeQualifierKind;
use crate::TypeSpecifierQualifier::Qualifier;
use crate::TypeSpecifierQualifier::Specifier;
use crate::UnlabeledStatement;
use crate::sexpr_builder::AsSExpr as _;
use crate::unimplemented_todo;

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

    #[error("declaration with duplicate `{first}` storage class specifier")]
    #[diagnostics(
        first(colour = Blue, label = "first `{first}` here"),
        at(colour = Red, label = "help: remove this `{at}`"),
    )]
    DuplicateStorageClassSpecifier {
        first: cst::StorageClassSpecifier<'a>,
        at: cst::StorageClassSpecifier<'a>,
    },

    #[error("declaration does not specify a type")]
    #[diagnostics(at(colour = Red, label = "type missing"))]
    DeclarationWithoutType { at: cst::DeclarationSpecifiers<'a> },

    #[error("{kind} `{name}` declared with function-specifier `{at}`")]
    #[diagnostics(
        at(colour = Red, label = "help: remove this `{at}`"),
        name(colour = Blue, label = "in the declaration for `{name}`"),
    )]
    NonFunctionDeclaredWithFunctionSpecifier {
        at: cst::FunctionSpecifier<'a>,
        name: Loc<'a>,
        kind: &'a str,
    },

    #[error("members cannot have storage classes")]
    #[diagnostics(
        at(colour = Red, label = "member `{name}` declared with storage class `{at}`"),
        member_loc(colour = Blue, label = "in this member declaration"),
    )]
    #[with(name = try { name.as_ref()?.slice() }.unwrap_or(NO_VALUE).fg(Blue))]
    StorageClassInMemberDeclaration {
        at: Token<'a>,
        name: Option<Token<'a>>,
        member_loc: Loc<'a>,
    },
}

// TODO: could this be `From<&'a dyn Report>`?
pub trait FromError<'a> {
    fn from_error(error: &'a dyn Report) -> Self;
}

impl FromError<'_> for () {
    fn from_error(_error: &dyn Report) -> Self {}
}

impl<'a> FromError<'a> for &'a (dyn Report + 'a) {
    fn from_error(error: &'a dyn Report) -> Self {
        error
    }
}

impl<'a, T, E> FromError<'a> for Either<E, T>
where
    E: FromError<'a>,
{
    fn from_error(error: &'a dyn Report) -> Self {
        Self::Left(E::from_error(error))
    }
}

type Diagnostics<'a> = RefCell<Vec<&'a dyn Report>>;

#[derive(Debug)]
pub struct Session<'a> {
    bump: &'a Bump,
    diagnostics: Diagnostics<'a>,
    treat_error_as_bug: Option<usize>,
}

impl<'a> Session<'a> {
    pub fn new(bump: &'a Bump, treat_error_as_bug: Option<usize>) -> Self {
        Self {
            bump,
            diagnostics: Diagnostics::default(),
            treat_error_as_bug,
        }
    }

    pub fn bump(&self) -> &'a Bump {
        self.bump
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

    pub fn alloc_slice_collect<I, T>(&self, values: I) -> &'a [T]
    where
        I: IntoIterator<Item = T>,
        T: Copy,
    {
        self.bump.alloc_slice_collect(values)
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

    #[track_caller]
    pub fn emit_many<I, T>(&self, diagnostics: I)
    where
        I: IntoIterator<Item = T>,
        T: Report + 'a,
    {
        for diagnostic in diagnostics {
            self.emit(diagnostic)
        }
    }

    #[track_caller]
    pub fn emit<T, Expr>(&self, diagnostic: T) -> Expr
    where
        T: Report + 'a,
        Expr: FromError<'a>,
    {
        if let Some(treat_error_as_bug) = self.treat_error_as_bug
            && self.diagnostics.borrow().len() == treat_error_as_bug.saturating_sub(1)
        {
            diagnostic.print();
            panic!("error treated as bug");
        }
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
    pub filename: &'a Path,
    pub decls: &'a [ExternalDeclaration<'a>],
}

#[derive(Debug, Clone, Copy)]
pub enum ExternalDeclaration<'a> {
    FunctionDefinition(FunctionDefinition<'a>),
    Declaration(Declaration<'a, InitDeclarator<'a>>),
}

#[derive(Debug, Clone, Copy)]
pub struct Declaration<'a, InitDeclarator> {
    pub storage_class: Option<cst::StorageClassSpecifier<'a>>,
    pub function_specifiers: FunctionSpecifiers<'a>,
    pub ty: QualifiedType<'a>,
    pub declarators: &'a [InitDeclarator],
}

#[derive(Debug, Clone, Copy)]
pub enum FunctionStorageClass<'a> {
    Extern,
    Static,
    Invalid(cst::StorageClassSpecifier<'a>),
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionDefinition<'a> {
    pub declarator: cst::Declarator<'a>,
    pub storage_class: Option<FunctionStorageClass<'a>>,
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
    Void,
    Typedef(Token<'a>),
    Typeof {
        unqual: bool,
        expr: &'a Expression<'a>,
    },
    TypeofTy {
        unqual: bool,
        ty: &'a TypeName<'a>,
    },
    Struct(Struct<'a>),
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
    Bool,
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
pub enum Struct<'a> {
    Incomplete {
        name: Token<'a>,
        kind: StructKind,
    },
    Complete {
        name: Option<Token<'a>>,
        kind: StructKind,
        members: &'a [Declaration<'a, Member<'a>>],
    },
}

impl<'a> Struct<'a> {
    pub fn loc(&self) -> Loc<'a> {
        match self {
            Self::Incomplete { name, kind: _ } => name.loc(),
            Self::Complete { name, kind: _, members: _ } => name
                .expect(
                    "only needed for tag mismatches in redeclarations; \
                    and unnamed structs are never redeclared",
                )
                .loc(),
        }
    }

    pub fn kind(&self) -> StructKind {
        match self {
            Self::Incomplete { name: _, kind } | Self::Complete { name: _, kind, members: _ } =>
                *kind,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Member<'a> {
    pub declarator: cst::Declarator<'a>,
    pub bitfield_width: Option<Expression<'a>>,
}

impl<'a> Member<'a> {
    fn parse(sess: &'a Session<'a>, member: &'a cst::Declaration<'a>) -> Declaration<'a, Self> {
        let Declaration {
            storage_class,
            function_specifiers,
            ty,
            declarators,
        } = Declaration::from_parse_tree(sess, member);
        let declarators = declarators.iter().map(|&init_declarator| {
            let InitDeclarator { declarator, bitfield_width, initialiser } = init_declarator;
            let loc =
                try { declarator.direct_declarator.name()?.loc() }.unwrap_or_else(|| ty.loc());
            reject_function_specifiers(sess, &function_specifiers, loc, "struct member");
            if let Some(storage_class) = storage_class {
                sess.emit(Diagnostic::StorageClassInMemberDeclaration {
                    at: storage_class.token,
                    name: try { declarator.direct_declarator.name().copied()? },
                    member_loc: loc,
                })
            }
            if let Some(initialiser) = initialiser {
                sess.emit(cst::Diagnostic::InvalidDefaultValue {
                    at: initialiser,
                    decl_loc: loc,
                    kind: "struct member",
                })
            }
            Self { declarator, bitfield_width }
        });

        Declaration {
            storage_class: None,
            function_specifiers: FunctionSpecifiers::default(),
            ty,
            declarators: sess.alloc_slice_fill_iter(declarators),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CompoundStatement<'a>(pub &'a [Statement<'a>]);

#[derive(Debug, Clone, Copy)]
pub enum Statement<'a> {
    // TODO: restrict the kinds of decls that are allowed in function scope?
    Declaration(Declaration<'a, InitDeclarator<'a>>),
    Expression(Option<Expression<'a>>),
    Compound(CompoundStatement<'a>),
    Return {
        return_: Token<'a>,
        expr: Option<Expression<'a>>,
    },
}

impl<'a> ExternalDeclaration<'a> {
    fn from_parse_tree(sess: &'a Session<'a>, decl: &'a cst::ExternalDeclaration<'a>) -> Self {
        match decl {
            cst::ExternalDeclaration::FunctionDefinition(def) =>
                Self::FunctionDefinition(FunctionDefinition::from_parse_tree(sess, def)),
            cst::ExternalDeclaration::Declaration(decl) =>
                Self::Declaration(Declaration::from_parse_tree(sess, decl)),
        }
    }
}

impl<'a> FunctionDefinition<'a> {
    fn from_parse_tree(sess: &'a Session<'a>, def: &cst::FunctionDefinition<'a>) -> Self {
        let cst::FunctionDefinition { declaration_specifiers, declarator, body } = *def;
        let DeclarationSpecifiers { storage_class, function_specifiers, ty } =
            parse_declaration_specifiers(sess, declaration_specifiers);
        let FunctionSpecifiers { inline, noreturn } = function_specifiers;
        let storage_class = match try { storage_class?.kind } {
            Some(StorageClassSpecifierKind::Extern) => Some(FunctionStorageClass::Extern),
            Some(StorageClassSpecifierKind::Static) => Some(FunctionStorageClass::Static),
            Some(_) => Some(FunctionStorageClass::Invalid(storage_class.unwrap())),
            None => None,
        };
        Self {
            declarator,
            storage_class,
            inline,
            noreturn,
            ty,
            body: CompoundStatement::from_parse_tree(sess, &body),
        }
    }
}

impl<'a> QualifiedType<'a> {
    pub fn loc(&self) -> Loc<'a> {
        self.loc
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
            Type::Void => write!(f, "void"),
            Type::Typedef(name) => write!(f, "typedef<{}>", name.slice()),
            Type::Typeof { unqual, expr } => write!(
                f,
                "typeof{}({})",
                if *unqual { "_unqual" } else { "" },
                expr.as_sexpr(),
            ),
            Type::TypeofTy { unqual, ty } => write!(
                f,
                "typeof{}({})",
                if *unqual { "_unqual" } else { "" },
                ty.as_sexpr(),
            ),
            Type::Struct(Struct::Incomplete { name, kind }) => write!(f, "{kind} {}", name.slice()),
            Type::Struct(Struct::Complete { name, kind, members: _ }) =>
                write!(f, "{kind} {} complete", name.as_sexpr()),
        }
    }
}

impl fmt::Display for Integral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { kind, signedness } = self;
        if !matches!(kind, IntegralKind::Bool)
            && let (IntegralKind::Char, _) | (_, Signedness::Unsigned) = (kind, signedness)
        {
            write!(f, "{signedness} ")?;
        }
        write!(f, "{kind}")
    }
}

impl<'a> Declaration<'a, InitDeclarator<'a>> {
    fn from_parse_tree(sess: &'a Session<'a>, decl: &'a cst::Declaration<'a>) -> Self {
        let cst::Declaration { specifiers, init_declarator_list } = decl;
        let DeclarationSpecifiers { storage_class, function_specifiers, ty } =
            parse_declaration_specifiers(sess, *specifiers);
        Self {
            storage_class,
            function_specifiers,
            ty,
            declarators: *init_declarator_list,
        }
    }
}

impl<'a> TypeQualifier<'a> {
    fn parse(self, sess: &'a Session<'a>, qualifiers: &mut Qualifiers<'a>) {
        let Qualifiers { const_qualifier, volatile_qualifier } = qualifiers;
        match self.kind {
            TypeQualifierKind::Const => match *const_qualifier {
                Some(first) =>
                    sess.emit(Diagnostic::DuplicateDeclarationSpecifier { at: self, first }),
                None => *const_qualifier = Some(self),
            },
            TypeQualifierKind::Volatile => match *volatile_qualifier {
                Some(first) =>
                    sess.emit(Diagnostic::DuplicateDeclarationSpecifier { at: self, first }),
                None => *volatile_qualifier = Some(self),
            },
            _ => unimplemented_todo!(self, "unimplemented type qualifier {:#?}", self),
        }
    }
}

impl<'a> CompoundStatement<'a> {
    fn from_parse_tree(sess: &'a Session<'a>, stmt: &cst::CompoundStatement<'a>) -> Self {
        Self(
            sess.alloc_slice_fill_iter(
                stmt.0
                    .iter()
                    .map(|stmt| Statement::from_parse_tree(sess, stmt)),
            ),
        )
    }
}

impl<'a> Statement<'a> {
    fn from_parse_tree(sess: &'a Session<'a>, item: &'a BlockItem<'a>) -> Self {
        match item {
            BlockItem::Declaration(decl) =>
                Self::Declaration(Declaration::from_parse_tree(sess, decl)),
            BlockItem::UnlabeledStatement(stmt) => Self::from_unlabeled_statement(sess, stmt),
        }
    }

    fn from_unlabeled_statement(sess: &'a Session<'a>, stmt: &UnlabeledStatement<'a>) -> Self {
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
            Arithmetic::Integral(Integral { signedness: _, kind: IntegralKind::Bool }) => 1,
            Arithmetic::Integral(Integral {
                signedness: _,
                kind: IntegralKind::Char | IntegralKind::PlainChar,
            }) => 2,
            Arithmetic::Integral(Integral { signedness: _, kind: IntegralKind::Short }) => 3,
            Arithmetic::Integral(Integral { signedness: _, kind: IntegralKind::Int }) => 4,
            Arithmetic::Integral(Integral { signedness: _, kind: IntegralKind::Long }) => 5,
            Arithmetic::Integral(Integral {
                signedness: _,
                kind: IntegralKind::LongLong,
            }) => 6,
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
            (Signedness::Signed, IntegralKind::Bool) => unreachable!(),
            (Signedness::Signed, IntegralKind::PlainChar) => i8::try_from(value).is_ok(),
            (Signedness::Signed, IntegralKind::Char) => i8::try_from(value).is_ok(),
            (Signedness::Signed, IntegralKind::Short) => i16::try_from(value).is_ok(),
            (Signedness::Signed, IntegralKind::Int) => i32::try_from(value).is_ok(),
            (Signedness::Signed, IntegralKind::Long) => i64::try_from(value).is_ok(),
            (Signedness::Signed, IntegralKind::LongLong) => i64::try_from(value).is_ok(),
            (Signedness::Unsigned, IntegralKind::Bool) => matches!(u8::try_from(value), Ok(0 | 1)),
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
            IntegralKind::Bool => 1,
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
            IntegralKind::Bool => write!(f, "bool"),
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

pub struct DeclarationSpecifiers<'a> {
    pub storage_class: Option<cst::StorageClassSpecifier<'a>>,
    pub function_specifiers: FunctionSpecifiers<'a>,
    pub ty: QualifiedType<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ParsedSpecifiers<'a> {
    None,
    Void,
    Char(Option<Signedness>),
    Int {
        kind: Option<IntegralKind>,
        signedness: Option<Signedness>,
    },
    Typedef(Token<'a>),
    Typeof {
        unqual: bool,
        expr: &'a Expression<'a>,
    },
    TypeofTy {
        unqual: bool,
        ty: &'a TypeName<'a>,
    },
    Struct(cst::Struct<'a>),
}

impl<'a> ParsedSpecifiers<'a> {
    pub(crate) fn into_type(
        self,
        sess: &'a Session<'a>,
        if_none: impl FnOnce() -> Type<'a>,
    ) -> Type<'a> {
        match self {
            Self::None => if_none(),
            Self::Void => Type::Void,
            Self::Char(signedness) => {
                let kind = match signedness {
                    Some(_) => IntegralKind::Char,
                    None => IntegralKind::PlainChar,
                };
                Type::Arithmetic(Arithmetic::Integral(Integral {
                    signedness: signedness.unwrap_or(Signedness::Signed),
                    kind,
                }))
            }
            Self::Int { kind, signedness } => Type::Arithmetic(Arithmetic::Integral(Integral {
                signedness: signedness.unwrap_or(Signedness::Signed),
                kind: kind.unwrap_or(IntegralKind::Int),
            })),
            Self::Typedef(token) => Type::Typedef(token),
            Self::Typeof { unqual, expr } => Type::Typeof { unqual, expr },
            Self::TypeofTy { unqual, ty } => Type::TypeofTy { unqual, ty },
            Self::Struct(cst::Struct::Incomplete { name, kind }) =>
                Type::Struct(Struct::Incomplete { name, kind }),
            Self::Struct(cst::Struct::Complete { name, kind, members }) =>
                Type::Struct(Struct::Complete {
                    name,
                    kind,
                    members: sess.alloc_slice_fill_iter(
                        members.iter().map(|member| Member::parse(sess, member)),
                    ),
                }),
        }
    }
}

#[derive(Default)]
pub struct Qualifiers<'a> {
    pub const_qualifier: Option<TypeQualifier<'a>>,
    pub volatile_qualifier: Option<TypeQualifier<'a>>,
}

impl<'a> Qualifiers<'a> {
    pub fn parse(sess: &'a Session<'a>, type_qualifiers: &[TypeQualifier<'a>]) -> Self {
        let mut qualifiers = Self::default();
        for qualifier in type_qualifiers {
            qualifier.parse(sess, &mut qualifiers)
        }
        qualifiers
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct FunctionSpecifiers<'a> {
    pub inline: Option<cst::FunctionSpecifier<'a>>,
    pub noreturn: Option<cst::FunctionSpecifier<'a>>,
}

pub fn parse_declaration_specifiers<'a>(
    sess: &'a Session<'a>,
    specifiers: cst::DeclarationSpecifiers<'a>,
) -> DeclarationSpecifiers<'a> {
    let mut qualifiers = Qualifiers::default();
    let mut ty = ParsedSpecifiers::None;
    let mut storage_class = None;
    let mut function_specifiers = FunctionSpecifiers::default();
    for (i, specifier) in specifiers.0.iter().enumerate() {
        match specifier {
            cst::DeclarationSpecifier::StorageClass(class)
                if let StorageClassSpecifierKind::Extern
                | StorageClassSpecifierKind::Static
                | StorageClassSpecifierKind::Typedef = class.kind =>
                match &mut storage_class {
                    storage_class @ None => *storage_class = Some(*class),
                    Some(storage_class) => sess.emit(Diagnostic::DuplicateStorageClassSpecifier {
                        first: *storage_class,
                        at: *class,
                    }),
                },
            cst::DeclarationSpecifier::StorageClass(storage_class) => unimplemented_todo!(
                storage_class,
                "unimplemented storage class {:#?}",
                storage_class,
            ),
            cst::DeclarationSpecifier::TypeSpecifierQualifier(Specifier(specifier)) =>
                ty = specifier.parse(sess, specifiers, i, ty),
            cst::DeclarationSpecifier::TypeSpecifierQualifier(Qualifier(qualifier)) =>
                qualifier.parse(sess, &mut qualifiers),
            cst::DeclarationSpecifier::FunctionSpecifier(function_specifier) =>
                match function_specifier.kind {
                    cst::FunctionSpecifierKind::Inline =>
                        function_specifiers.inline = Some(*function_specifier),
                    cst::FunctionSpecifierKind::Noreturn =>
                        function_specifiers.noreturn = Some(*function_specifier),
                },
        }
    }

    let Qualifiers { const_qualifier, volatile_qualifier } = qualifiers;
    let ty = ty.into_type(sess, || {
        // TODO: implement `FromError` for `Type`
        let () = sess.emit(Diagnostic::DeclarationWithoutType { at: specifiers });
        // FIXME: don’t use implicit int here, an explicit “type error” type will be better
        Type::Arithmetic(Arithmetic::Integral(Integral {
            signedness: Signedness::Signed,
            kind: IntegralKind::Int,
        }))
    });
    DeclarationSpecifiers {
        storage_class,
        function_specifiers,
        ty: QualifiedType {
            is_const: const_qualifier.is_some(),
            is_volatile: volatile_qualifier.is_some(),
            ty,
            loc: specifiers.loc(),
        },
    }
}

pub fn reject_function_specifiers<'a>(
    sess: &Session<'a>,
    function_specifiers: &FunctionSpecifiers<'a>,
    declaration_loc: Loc<'a>,
    kind: &'a str,
) {
    let FunctionSpecifiers { inline, noreturn } = function_specifiers;
    for specifier in [inline, noreturn].into_iter().flatten() {
        sess.emit(Diagnostic::NonFunctionDeclaredWithFunctionSpecifier {
            at: *specifier,
            name: declaration_loc,
            kind,
        })
    }
}

pub(crate) fn from_parse_tree<'a>(
    sess: &'a Session<'a>,
    parse_tree: cst::TranslationUnit<'a>,
) -> TranslationUnit<'a> {
    let cst::TranslationUnit { filename, decls } = parse_tree;
    TranslationUnit {
        filename,
        decls: sess.alloc_slice_fill_iter(
            decls
                .iter()
                .map(|decl| ExternalDeclaration::from_parse_tree(sess, decl)),
        ),
    }
}
