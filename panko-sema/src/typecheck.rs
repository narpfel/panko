use std::cmp::Ordering;
use std::fmt;
use std::hash::Hash;
use std::num::IntErrorKind;
use std::str::Chars;

use ariadne::Color::Blue;
use ariadne::Color::Green;
use ariadne::Color::Magenta;
use ariadne::Color::Red;
use ariadne::Fmt as _;
use indexmap::IndexMap;
use itertools::EitherOrBoth;
use itertools::Itertools as _;
use panko_lex::Integer;
use panko_lex::IntegerSuffix;
use panko_lex::Loc;
use panko_lex::Token;
use panko_lex::TokenKind;
use panko_parser as cst;
use panko_parser::BinOp;
use panko_parser::BinOpKind;
use panko_parser::IncrementOpKind;
use panko_parser::LogicalOp;
use panko_parser::UnaryOp;
use panko_parser::UnaryOpKind;
use panko_parser::ast::Arithmetic;
use panko_parser::ast::ErrorExpr;
use panko_parser::ast::Integral;
use panko_parser::ast::IntegralKind;
use panko_parser::ast::Session;
use panko_parser::ast::Signedness;
use panko_report::Report;
use variant_types::IntoVariant as _;

use crate::scope;
use crate::scope::DesignatedInitialiser;
use crate::scope::Designation;
use crate::scope::Designator;
use crate::scope::GenericAssociation;
use crate::scope::Id;
use crate::scope::IncrementFixity;
use crate::scope::IsParameter;
use crate::scope::RefKind;
use crate::scope::StorageDuration;
use crate::ty;
use crate::ty::ArrayType;
use crate::ty::FunctionType;
use crate::ty::ParameterDeclaration;
use crate::ty::subobjects::AllowExplicit;
use crate::ty::subobjects::Subobject;
use crate::ty::subobjects::SubobjectIterator;
use crate::ty::subobjects::Subobjects;

mod as_sexpr;
#[cfg(test)]
mod tests;

// TODO: this should be in `panko_report`
trait Sliced {
    fn slice(&self) -> String;
}

impl<T> Sliced for T
where
    T: ToString,
{
    fn slice(&self) -> String {
        self.to_string()
    }
}

#[derive(Debug, Report)]
#[exit_code(1)]
enum Diagnostic<'a> {
    #[error("redeclaration of `{at}` with different type: `{original_ty}` vs. `{new_ty}`")]
    #[with(original_ty = previous_definition.ty, new_ty = at.ty)]
    #[diagnostics(
        previous_definition(colour = Blue, label = "previously declared here with type `{original_ty}`"),
        at(colour = Red, label = "new declaration with different type `{new_ty}`"),
    )]
    AlreadyDefinedWithDifferentType {
        at: Reference<'a>,
        previous_definition: Reference<'a>,
    },

    #[error("cannot declare variable `{at}` with incomplete type `{ty}`")]
    #[with(ty = at.ty)]
    #[diagnostics(at(colour = Red, label = "declared here"))]
    VariableWithIncompleteType { at: Reference<'a> },

    #[error("cannot declare function parameter `{at}` with incomplete type `{ty}`")]
    #[with(ty = at.ty)]
    #[diagnostics(at(colour = Red, label = "parameter declared here"))]
    ParameterWithIncompleteType {
        at: ParameterDeclaration<'a, !, ArrayLength<&'a TypedExpression<'a>>>,
    },

    #[error("array with incomplete element type `{at}`")]
    #[diagnostics(at(colour = Red, label = "array element types must be complete"))]
    ArrayWithIncompleteType { at: QualifiedType<'a> },

    #[error("arrays of functions are not allowed")]
    #[diagnostics(at(colour = Red, label = "element type is `{at}`"))]
    ArrayOfFunctions { at: QualifiedType<'a> },

    #[error("invalid function return type `{ty}`")]
    #[diagnostics(at(colour = Red, label = "declaration here"))]
    InvalidFunctionReturnType { at: Loc<'a>, ty: QualifiedType<'a> },

    // TODO: types should be printed in C syntax, not in their SExpr debug repr
    #[error("invalid {kind} conversion from `{from_ty}` to `{to_ty}`")]
    #[diagnostics(at(colour = Red, label = "this is of type `{from_ty}`, which cannot be {kind}ly converted to `{to_ty}`"))]
    #[with(from_ty = from_ty.fg(Blue), to_ty = to_ty.fg(Magenta))]
    InvalidConversion {
        at: TypedExpression<'a>,
        from_ty: Type<'a>,
        to_ty: Type<'a>,
        kind: ConversionKind,
    },

    #[error(
        "`{return_}` statement without value in non-`void` function `{name}` returning `{return_ty}`"
    )]
    #[diagnostics(
        at(colour = Red, label = "`{return_}` statement here"),
        function(colour = Blue, label = "function `{name}` declared here"),
    )]
    #[with(
        return_ = at.return_,
        name = function.reference,
        return_ty = function.return_ty(),
    )]
    ReturnWithoutValueInNonVoidFunction {
        at: scope::StatementTypes::Return<'a>,
        function: scope::FunctionDefinition<'a>,
    },

    #[error("cannot assign to `const` value `{decl}`")]
    #[diagnostics(
        at(colour = Red, label = "this value is `const`"),
        decl(colour = Blue, label = "note: `{decl}` declared here")
    )]
    AssignmentToConstName {
        at: &'a Expression<'a>,
        decl: Reference<'a>,
    },

    #[error("cannot assign to `const` expression")]
    #[diagnostics(
        at(colour = Red, label = "this value is `const`"),
    )]
    AssignmentToConst { at: Expression<'a> },

    #[error("cannot assign to this expression because it is not an lvalue")]
    #[diagnostics(at(colour = Red, label = "this expression is not an lvalue"))]
    AssignmentToNonLValue { at: &'a Expression<'a> },

    #[error("dereference of pointer to `void`")]
    #[diagnostics(at(colour = Red, label = "this expression has type `{ty}`"))]
    #[with(ty = at.ty.ty)]
    DerefOfVoidPtr { at: TypedExpression<'a> },

    #[error("invalid integer suffix `{suffix}`")]
    #[diagnostics(at(colour = Red, label = "invalid integer suffix"))]
    #[with(suffix = suffix.fg(Red))]
    InvalidIntegerSuffix { at: Token<'a>, suffix: &'a str },

    #[error("invalid application of `{op}` to {kind} `{ty}`")]
    #[diagnostics(at(colour = Red, label = "in this expression"), op(colour = Red))]
    #[with(ty = ty.fg(Blue))]
    InvalidSizeofOrAlignof {
        op: Token<'a>,
        at: scope::Expression<'a>,
        kind: &'static str,
        ty: QualifiedType<'a>,
    },

    #[error("cannot take address of this expression because it is {reason}")]
    #[diagnostics(at(colour = Red, label = "this expression is {reason}"))]
    #[with(reason = reason.fg(Red))]
    CannotTakeAddress {
        at: TypedExpression<'a>,
        reason: &'a str,
    },

    #[error("cannot dereference this expression with type `{ty}` (pointer or array type required)")]
    #[diagnostics(at(colour = Red, label = "in this expression"))]
    #[with(ty = ty.fg(Red))]
    CannotDeref {
        at: scope::Expression<'a>,
        ty: QualifiedType<'a>,
    },

    #[error(
        "cannot apply {operator_name} operator to expression with type `{ty}` ({expected_ty} type required)"
    )]
    #[diagnostics(at(colour = Red, label = "in this expression"))]
    #[with(
        ty = ty.fg(Red),
        operator_name = operator_name.fg(Red),
        expected_ty = expected_ty.fg(Blue),
    )]
    InvalidOperandForUnaryOperator {
        at: scope::Expression<'a>,
        ty: QualifiedType<'a>,
        operator_name: &'a str,
        expected_ty: &'a str,
    },

    #[error("duplicate `{at}` association in {generic} selection")]
    #[diagnostics(
        generic(colour = Green, label = "in this {generic} selection"),
        previous_default(colour = Blue, label = "first `{previous_default}` here"),
        at(colour = Red, label = "duplicate `{at}`"),
    )]
    GenericWithDuplicateDefault {
        at: Token<'a>,
        previous_default: Token<'a>,
        generic: Token<'a>,
    },

    #[error("duplicate association for type `{at}` in {generic} selection")]
    #[diagnostics(
        generic(colour = Green, label = "in this {generic} selection"),
        previous(colour = Blue, label = "previous association for `{previous}` here"),
        at(colour = Red, label = "this type is duplicated in this {generic} selection"),
    )]
    GenericWithDuplicateType {
        at: QualifiedType<'a>,
        previous: QualifiedType<'a>,
        generic: Token<'a>,
    },

    #[error("controlling expression does not match any type in this {generic} selection")]
    #[diagnostics(
        at(colour = Red, label = "controlling expression has type `{ty}`"),
        expr(colour = Blue, label = "in this {generic} selection"),
    )]
    #[with(ty = at.ty.ty, generic = "_Generic".fg(Blue))]
    GenericWithoutMatch {
        at: TypedExpression<'a>,
        expr: scope::Expression<'a>,
    },

    #[error(
        "the type of a {generic} association shall be a complete object type other than a variably modified type"
    )]
    #[diagnostics(
        generic(colour = Green, label = "in this {generic} selection"),
        at(colour = Red, label = "this type is not allowed in a {generic} association"),
    )]
    GenericWithInvalidType {
        at: QualifiedType<'a>,
        generic: Token<'a>,
    },

    #[error("subtraction between pointers of incompatible types `{lhs_ty}` and `{rhs_ty}`")]
    #[diagnostics(
        lhs(colour = Blue, label = "this is of type `{lhs_ty}`"),
        at(colour = Red),
        rhs(colour = Magenta, label = "this is of type `{rhs_ty}`"),
    )]
    #[with(lhs_ty = lhs.ty, rhs_ty = rhs.ty)]
    PtrDiffIncompatiblePointeeTypes {
        at: Token<'a>,
        lhs: TypedExpression<'a>,
        rhs: TypedExpression<'a>,
    },

    #[error("{kind} operator expects a scalar value as operand")]
    #[diagnostics(at(colour = Red, label = "this is of type `{ty}`"), expr(colour = Blue))]
    #[with(ty = at.ty, kind = kind.fg(Blue))]
    ScalarExpected {
        at: TypedExpression<'a>,
        expr: scope::Expression<'a>,
        kind: &'a str,
    },

    #[error("invalid operands to binary operator `{op_token}`")]
    #[diagnostics(
        lhs(colour = Blue, label = "this is of type `{lhs_ty}`"),
        // TODO: include message showing what was expected
        at(colour = Red),
        rhs(colour = Magenta, label = "this is of type `{rhs_ty}`"),
    )]
    #[with(op_token = at.token, lhs_ty = lhs.ty, rhs_ty = rhs.ty)]
    InvalidOperandsForBinaryOperator {
        at: BinOp<'a>,
        lhs: TypedExpression<'a>,
        rhs: TypedExpression<'a>,
    },

    #[error("integer literal too large")]
    #[diagnostics(at(colour = Red, label = "this literal does not fit any integer type"))]
    IntegerLiteralTooLarge { at: Token<'a> },

    #[error("functions must have a body, not an initialiser")]
    #[diagnostics(
        ty(colour = Blue, label = "in this function definition"),
        at(colour = Red, label = "functions must have a body (or remove this to declare the function)"),
    )]
    IllegalInitialiserForFunction {
        at: TypedExpression<'a>,
        ty: QualifiedType<'a>,
    },

    #[error("comparison of pointers to incompatible types")]
    #[diagnostics(
        lhs(colour = Blue, label = "this is of type `{lhs_ty}`"),
        at(colour = Red, label = "pointer comparisons are only allowed between pointers to compatible types"),
        rhs(colour = Magenta, label = "this is of type `{rhs_ty}`"),
    )]
    #[with(lhs_ty = lhs.ty.ty, rhs_ty = rhs.ty.ty)]
    IncompatibleTypesInPtrCmp {
        lhs: TypedExpression<'a>,
        at: Token<'a>,
        rhs: TypedExpression<'a>,
    },

    #[error("`{op}` can only be applied to arrays, not `{ty}`")]
    #[diagnostics(
        op(colour = Blue),
        at(colour = Red, label = "this expression is of type `{ty}`"),
    )]
    #[with(ty = at.ty)]
    InvalidLengthof {
        op: Token<'a>,
        at: TypedExpression<'a>,
    },

    #[error("`{op}` can only be applied to arrays, not `{at}`")]
    #[diagnostics(
        op(colour = Blue),
        at(colour = Red, label = "this type is `{at}`"),
    )]
    InvalidLengthofTy {
        op: Token<'a>,
        at: QualifiedType<'a>,
    },

    #[error(
        "`{op}` can only be applied to arrays of known length, but the operand has type `{ty}`"
    )]
    #[diagnostics(
        op(colour = Blue),
        at(colour = Red, label = "the type of this is `{ty}`, which has unknown length"),
    )]
    #[with(ty = at.ty)]
    LengthofOfArrayOfUnknownLength {
        op: Token<'a>,
        at: TypedExpression<'a>,
    },

    #[error("`{op}` can only be applied to arrays of known length")]
    #[diagnostics(
        op(colour = Blue),
        at(colour = Red, label = "`{at}` has unknown length"),
    )]
    LengthofOfArrayTyOfUnknownLength {
        op: Token<'a>,
        at: QualifiedType<'a>,
    },

    #[error(
        "argument count mismatch: too {determiner} arguments to function call (expected {at_least}{expected} but got {actual})"
    )]
    #[diagnostics(
        at(colour = Red, label = "this callee expects {expected} argument{maybe_plural_s}"),
    )]
    #[with(
        at_least = if *is_varargs { "at least " } else { "" },
        determiner = if expected > actual { "few" } else { "many" },
        maybe_plural_s = if *expected != 1 { "s" } else { "" },
        expected = expected.fg(Blue),
        actual = actual.fg(Red),
    )]
    ArityMismatch {
        at: TypedExpression<'a>,
        expected: usize,
        actual: usize,
        is_varargs: bool,
    },

    #[error("empty arrays are not allowed")]
    #[diagnostics(
        at(colour = Red, label = "this array is declared as empty"),
    )]
    EmptyArray { at: Reference<'a> },

    #[error("excess element in {kind} initialiser")]
    #[with(
        name = reference.name.fg(Blue),
        ty = reference.ty.fg(Blue),
        kind = iterator.kind(),
        help = match iterator {
            SubobjectIterator::Scalar { .. } => format!("`{name}`’s type `{ty}` is scalar"),
            SubobjectIterator::Array { ty, index, offset: _ } => {
                let ty = ty.fg(Blue);
                // trying to get the non-existing element increments the index, so we undo this
                // here
                let index = index.checked_sub(1).unwrap().fg(Red);
                format!("trying to initialise element at index {index} for `{ty}`")
            }
        },
    )]
    #[diagnostics(
        reference(colour = Blue, label = "while initialising this variable"),
        at(colour = Red, label = "{help}"),
    )]
    ExcessInitialiser {
        at: scope::Initialiser<'a>,
        reference: Reference<'a>,
        iterator: SubobjectIterator<'a>,
    },

    #[error("no such subobject while initialising object of type `{ty}`")]
    #[diagnostics(
        reference(colour = Blue, label = "while initialising this variable"),
        at(colour = Red, label = "no such subobject"),
    )]
    #[with(
        ty = match iterator {
            SubobjectIterator::Scalar { ty, .. } => ty,
            SubobjectIterator::Array { .. } => unreachable!(),
        }.fg(Blue),
    )]
    NoSuchSubobject {
        at: Designator<'a>,
        reference: Reference<'a>,
        iterator: SubobjectIterator<'a>,
    },

    #[error("empty character constant")]
    #[diagnostics(at(colour = Red, label = "this character constant is empty"))]
    EmptyCharConstant { at: Token<'a> },
}

#[derive(Debug, Clone, Copy)]
pub enum ArrayLength<Expression> {
    Constant(u64),
    Variable(Expression),
    Unknown,
}

impl<Expression> ArrayLength<Expression> {
    pub(crate) fn is_known(&self) -> bool {
        !matches!(self, Self::Unknown)
    }
}

impl<'a> TryFrom<&'a TypedExpression<'a>> for ArrayLength<&'a TypedExpression<'a>> {
    type Error = &'a TypedExpression<'a>;

    fn try_from(value: &'a TypedExpression<'a>) -> Result<Self, Self::Error> {
        // TODO: constexpr evaluate `value` here
        match value.expr {
            Expression::Integer { value, token: _ } => Ok(Self::Constant(value)),
            _ => Err(value),
        }
    }
}

impl<Expression> PartialEq for ArrayLength<Expression> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ArrayLength::Constant(self_length), ArrayLength::Constant(other_length)) =>
                self_length == other_length,
            (ArrayLength::Unknown, _) | (_, ArrayLength::Unknown) => true,
            _ => todo!("type compatibility involving variably-modified types not implemented"),
        }
    }
}

impl<Expression> Eq for ArrayLength<Expression> {}

impl<Expression> Hash for ArrayLength<Expression> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            ArrayLength::Constant(length) => length.hash(state),
            ArrayLength::Variable(_) =>
                todo!("type compatibility involving variably-modified types not implemented"),
            ArrayLength::Unknown =>
                unreachable!("arrays of unknown length are not allowed in `_Generic`"),
        }
    }
}

pub(crate) type Type<'a> = ty::Type<'a, !, ArrayLength<&'a TypedExpression<'a>>>;
pub(crate) type QualifiedType<'a> = ty::QualifiedType<'a, !, ArrayLength<&'a TypedExpression<'a>>>;

#[derive(Debug, Clone, Copy)]
pub struct TranslationUnit<'a> {
    pub(crate) decls: &'a [ExternalDeclaration<'a>],
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ExternalDeclaration<'a> {
    FunctionDefinition(FunctionDefinition<'a>),
    Declaration(Declaration<'a>),
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Declaration<'a> {
    pub(crate) reference: Reference<'a>,
    pub(crate) initialiser: Option<Initialiser<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Initialiser<'a> {
    Braced {
        subobject_initialisers: &'a [SubobjectInitialiser<'a>],
    },
    Expression(TypedExpression<'a>),
}

impl Initialiser<'_> {
    fn array_length(&self, element_ty: &Type) -> Option<u64> {
        match self {
            Self::Braced {
                subobject_initialisers:
                    [
                        SubobjectInitialiser {
                            subobject: _,
                            initialiser: TypedExpression { ty: _, expr: Expression::String(value) },
                        },
                    ],
            } => Some(value.len()),
            Self::Braced { subobject_initialisers } => Some(
                subobject_initialisers
                    .iter()
                    .map(|initialiser| initialiser.subobject.offset)
                    .max()
                    .map_or(0, |max_offset| {
                        let element_size = element_ty.size();
                        assert!(max_offset.is_multiple_of(element_size));
                        (max_offset / element_size).checked_add(1).unwrap()
                    }),
            ),
            Self::Expression(TypedExpression { ty: _, expr: Expression::String(value) }) =>
                Some(value.len()),
            Self::Expression(_) => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct SubobjectInitialiser<'a> {
    pub(crate) subobject: Subobject<'a>,
    pub(crate) initialiser: TypedExpression<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct FunctionDefinition<'a> {
    pub(crate) reference: Reference<'a>,
    pub(crate) params: ParamRefs<'a>,
    pub(crate) storage_class: Option<cst::StorageClassSpecifier<'a>>,
    pub(crate) inline: Option<cst::FunctionSpecifier<'a>>,
    pub(crate) noreturn: Option<cst::FunctionSpecifier<'a>>,
    pub(crate) is_varargs: bool,
    pub(crate) body: CompoundStatement<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct ParamRefs<'a>(pub(crate) &'a [Reference<'a>]);

#[derive(Debug, Clone, Copy)]
pub(crate) struct CompoundStatement<'a>(pub(crate) &'a [Statement<'a>]);

#[derive(Debug, Clone, Copy)]
pub(crate) enum Statement<'a> {
    Declaration(Declaration<'a>),
    Expression(Option<TypedExpression<'a>>),
    Compound(CompoundStatement<'a>),
    Return(Option<TypedExpression<'a>>),
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct TypedExpression<'a> {
    pub(crate) ty: QualifiedType<'a>,
    pub(crate) expr: Expression<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct StringLiteral<'a> {
    value: &'a str,
    loc: Loc<'a>,
}

impl<'a> StringLiteral<'a> {
    pub(crate) fn value(&self) -> &'a str {
        self.value
    }

    fn len(&self) -> u64 {
        u64::try_from(self.value.len()).unwrap()
    }

    fn loc(&self) -> Loc<'a> {
        self.loc
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Expression<'a> {
    Error(&'a dyn Report),
    Name(Reference<'a>),
    Integer {
        value: u64,
        token: Token<'a>,
    },
    String(StringLiteral<'a>),
    NoopTypeConversion(&'a TypedExpression<'a>),
    // TODO: `Truncate`, `SignExtend`, `ZeroExtend` and `VoidCast` lose location information when
    // representing an explicit cast. They should take an optional location (or better:
    // `TypedExpression` should have a `loc`).
    Truncate(&'a TypedExpression<'a>),
    SignExtend(&'a TypedExpression<'a>),
    ZeroExtend(&'a TypedExpression<'a>),
    VoidCast(&'a TypedExpression<'a>),
    Parenthesised {
        open_paren: Token<'a>,
        expr: &'a TypedExpression<'a>,
        close_paren: Token<'a>,
    },
    Assign {
        target: &'a TypedExpression<'a>,
        value: &'a TypedExpression<'a>,
    },
    IntegralBinOp {
        ty: Integral,
        lhs: &'a TypedExpression<'a>,
        op: BinOp<'a>,
        rhs: &'a TypedExpression<'a>,
    },
    PtrAdd {
        pointer: &'a TypedExpression<'a>,
        integral: &'a TypedExpression<'a>,
        pointee_size: u64,
        order: PtrAddOrder,
    },
    PtrSub {
        pointer: &'a TypedExpression<'a>,
        integral: &'a TypedExpression<'a>,
        pointee_size: u64,
    },
    PtrDiff {
        lhs: &'a TypedExpression<'a>,
        rhs: &'a TypedExpression<'a>,
        pointee_size: u64,
    },
    PtrCmp {
        lhs: &'a TypedExpression<'a>,
        kind: PtrCmpKind,
        rhs: &'a TypedExpression<'a>,
    },
    Addressof {
        ampersand: Token<'a>,
        operand: &'a TypedExpression<'a>,
    },
    Deref {
        star: Token<'a>,
        operand: &'a TypedExpression<'a>,
    },
    Call {
        callee: &'a TypedExpression<'a>,
        args: &'a [TypedExpression<'a>],
        is_varargs: bool,
        close_paren: Token<'a>,
    },
    Negate {
        minus: Token<'a>,
        operand: &'a TypedExpression<'a>,
    },
    Compl {
        compl: Token<'a>,
        operand: &'a TypedExpression<'a>,
    },
    Not {
        not: Token<'a>,
        operand: &'a TypedExpression<'a>,
    },
    Sizeof {
        sizeof: Token<'a>,
        operand: &'a TypedExpression<'a>,
        size: u64,
    },
    Lengthof {
        lengthof: Token<'a>,
        operand: &'a TypedExpression<'a>,
        length: u64,
    },
    SizeofTy {
        sizeof: Token<'a>,
        ty: QualifiedType<'a>,
        size: u64,
        close_paren: Token<'a>,
    },
    LengthofTy {
        lengthof: Token<'a>,
        ty: QualifiedType<'a>,
        length: u64,
        close_paren: Token<'a>,
    },
    Alignof {
        alignof: Token<'a>,
        ty: QualifiedType<'a>,
        align: u64,
        close_paren: Token<'a>,
    },
    Combine {
        first: &'a TypedExpression<'a>,
        second: &'a TypedExpression<'a>,
    },
    Logical {
        lhs: &'a TypedExpression<'a>,
        op: LogicalOp<'a>,
        rhs: &'a TypedExpression<'a>,
    },
    Conditional {
        condition: &'a TypedExpression<'a>,
        then: &'a TypedExpression<'a>,
        or_else: &'a TypedExpression<'a>,
    },
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Reference<'a> {
    // TODO: The location of `name` points to where this name was declared. This is unused for now,
    // but should be used in error messages to print e. g. “note: [...] was declared here:”.
    pub(crate) name: &'a str,
    pub(crate) loc: Loc<'a>,
    pub(crate) ty: QualifiedType<'a>,
    pub(crate) id: Id,
    pub(crate) usage_location: Loc<'a>,
    pub(crate) kind: RefKind,
    pub(crate) storage_duration: StorageDuration,
}

impl<'a> Reference<'a> {
    pub(crate) fn name(&self) -> &'a str {
        self.name
    }

    pub(crate) fn unique_name(&self) -> String {
        format!("{}~{}", self.name, self.id.0)
    }

    #[expect(
        clippy::misnamed_getters,
        reason = "`loc` should actually return the `usage_location` and not the `loc` where this reference was declared"
    )]
    pub(crate) fn loc(&self) -> Loc<'a> {
        self.usage_location
    }

    pub(crate) fn slice(&self) -> &'a str {
        self.name
    }

    pub(crate) fn kind(&self) -> RefKind {
        self.kind
    }

    fn at(&self, location: Loc<'a>) -> Self {
        Self { usage_location: location, ..*self }
    }

    pub(crate) fn at_decl(&self) -> Self {
        self.at(self.loc)
    }
}

impl<'a> TypedExpression<'a> {
    pub(crate) fn loc(&self) -> Loc<'a> {
        self.expr.loc()
    }

    fn is_modifiable_lvalue(&self) -> bool {
        self.is_lvalue() && self.is_modifiable()
    }

    fn is_lvalue(&self) -> bool {
        match self.expr {
            Expression::Error(_error) => false,
            Expression::Name(reference) =>
                reference.ty.ty.is_object() && !matches!(reference.ty.ty, Type::Void),
            Expression::Parenthesised { open_paren: _, expr, close_paren: _ } => expr.is_lvalue(),
            Expression::Deref { .. } => self.ty.ty.is_object() && !matches!(self.ty.ty, Type::Void),
            Expression::String(_) => true,
            Expression::Integer { .. }
            | Expression::NoopTypeConversion(_)
            | Expression::Truncate(_)
            | Expression::SignExtend(_)
            | Expression::ZeroExtend(_)
            | Expression::VoidCast(_)
            | Expression::Assign { .. }
            | Expression::IntegralBinOp { .. }
            | Expression::PtrAdd { .. }
            | Expression::PtrSub { .. }
            | Expression::PtrDiff { .. }
            | Expression::PtrCmp { .. }
            | Expression::Addressof { .. }
            | Expression::Call { .. }
            | Expression::Negate { .. }
            | Expression::Compl { .. }
            | Expression::Not { .. }
            | Expression::Sizeof { .. }
            | Expression::Lengthof { .. }
            | Expression::SizeofTy { .. }
            | Expression::LengthofTy { .. }
            | Expression::Alignof { .. } => false,
            Expression::Combine { first: _, second } => second.is_lvalue(),
            Expression::Logical { .. } | Expression::Conditional { .. } => false,
        }
    }

    fn is_modifiable(&self) -> bool {
        !self.ty.is_const
    }
}

impl<'a> Expression<'a> {
    fn loc(&self) -> Loc<'a> {
        match self {
            Expression::Error(error) => error.location(),
            Expression::Name(reference) => reference.loc(),
            Expression::Integer { value: _, token } => token.loc(),
            Expression::String(string) => string.loc(),
            Expression::NoopTypeConversion(inner)
            | Expression::Truncate(inner)
            | Expression::SignExtend(inner)
            | Expression::ZeroExtend(inner)
            | Expression::VoidCast(inner) => inner.loc(),
            Expression::Assign { target, value } => target.loc().until(value.loc()),
            Expression::Parenthesised { open_paren, expr: _, close_paren } =>
                open_paren.loc().until(close_paren.loc()),
            Expression::IntegralBinOp { ty: _, lhs, op: _, rhs } => lhs.loc().until(rhs.loc()),
            Expression::PtrAdd {
                pointer,
                integral,
                pointee_size: _,
                order,
            } => {
                let (lhs, rhs) = order.select(pointer, integral);
                lhs.loc().until(rhs.loc())
            }
            Expression::PtrSub { pointer, integral, pointee_size: _ } =>
                pointer.loc().until(integral.loc()),
            Expression::PtrDiff { lhs, rhs, pointee_size: _ } => lhs.loc().until(rhs.loc()),
            Expression::PtrCmp { lhs, kind: _, rhs } => lhs.loc().until(rhs.loc()),
            Expression::Addressof { ampersand, operand } => ampersand.loc().until(operand.loc()),
            Expression::Deref { star, operand } => star.loc().until(operand.loc()),
            Expression::Call {
                callee,
                args: _,
                is_varargs: _,
                close_paren,
            } => callee.loc().until(close_paren.loc()),
            Expression::Negate { minus, operand } => minus.loc().until(operand.loc()),
            Expression::Compl { compl, operand } => compl.loc().until(operand.loc()),
            Expression::Not { not, operand } => not.loc().until(operand.loc()),
            Expression::Sizeof { sizeof, operand, size: _ } => sizeof.loc().until(operand.loc()),
            Expression::Lengthof { lengthof, operand, length: _ } =>
                lengthof.loc().until(operand.loc()),
            Expression::SizeofTy { sizeof, ty: _, size: _, close_paren } =>
                sizeof.loc().until(close_paren.loc()),
            Expression::LengthofTy { lengthof, ty: _, length: _, close_paren } =>
                lengthof.loc().until(close_paren.loc()),
            Expression::Alignof { alignof, ty: _, align: _, close_paren } =>
                alignof.loc().until(close_paren.loc()),
            Expression::Combine { first, second } => first.loc().until(second.loc()),
            Expression::Logical { lhs, op: _, rhs } => lhs.loc().until(rhs.loc()),
            Expression::Conditional { condition, then: _, or_else } =>
                condition.loc().until(or_else.loc()),
        }
    }

    fn unwrap_parens(&self) -> &Self {
        match self {
            Expression::Parenthesised { open_paren: _, expr, close_paren: _ } =>
                expr.expr.unwrap_parens(),
            _ => self,
        }
    }
}

impl<'a> ErrorExpr<'a> for TypedExpression<'a> {
    fn from_error(error: &'a dyn Report) -> Self {
        Self {
            // TODO: this should by `Type::Error`
            ty: Type::Void.unqualified(),
            expr: Expression::from_error(error),
        }
    }
}

impl<'a> ErrorExpr<'a> for Expression<'a> {
    fn from_error(error: &'a dyn Report) -> Self {
        Self::Error(error)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ConversionKind {
    Implicit,
    Explicit,
}

impl fmt::Display for ConversionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Self::Implicit => "implicit",
            Self::Explicit => "explicit",
        };
        write!(f, "{s}")
    }
}

fn typeck_ty_with_initialiser<'a>(
    sess: &'a Session<'a>,
    ty: scope::QualifiedType<'a>,
    is_parameter: IsParameter,
    reference: Option<&scope::Reference<'a>>,
) -> QualifiedType<'a> {
    let scope::QualifiedType { is_const, is_volatile, ty, loc } = ty;
    let ty = match ty {
        ty::Type::Arithmetic(arithmetic) => Type::Arithmetic(arithmetic),
        ty::Type::Pointer(pointee) =>
            Type::Pointer(sess.alloc(typeck_ty(sess, *pointee, IsParameter::No))),
        ty::Type::Array(ArrayType { ty, length }) => {
            let ty = sess.alloc(typeck_ty(sess, *ty, IsParameter::No));
            if !ty.ty.is_complete() {
                // TODO: this should be `Type::Error`
                // TODO: this generates a new error for each *usage* of this ty, including usages
                // of variables with this ty.
                sess.emit(Diagnostic::ArrayWithIncompleteType { at: *ty })
            }
            if ty.ty.is_function() {
                // TODO: this should be `Type::Error`
                sess.emit(Diagnostic::ArrayOfFunctions { at: *ty })
            }
            let length = length
                .map(|length| {
                    ArrayLength::try_from(sess.alloc(typeck_expression(
                        sess,
                        length,
                        Context::Default,
                    )))
                    .unwrap_or_else(|_| todo!("variable length array"))
                })
                .unwrap_or(ArrayLength::Unknown);
            let length = if let Some(reference) = reference
                && let Some(initialiser) = reference.initialiser
                && let ArrayLength::Unknown = length
                && let reference = typeck_reference(sess, *reference, NeedsInitialiser::No)
                && let initialiser = typeck_initialiser(sess, initialiser, &reference)
                && let Initialiser::Braced { subobject_initialisers: _ }
                | Initialiser::Expression(TypedExpression {
                    ty: _,
                    expr: Expression::String(_),
                }) = initialiser
            {
                ArrayLength::Constant(initialiser.array_length(&ty.ty).unwrap())
            }
            else {
                length
            };
            match is_parameter {
                IsParameter::Yes => {
                    // TODO: use qualifiers specified in the array declarator
                    if matches!(length, ArrayLength::Variable(_)) {
                        todo!("unimplemented: variably-modified type as function parameter");
                    }
                    Type::Pointer(ty)
                }
                IsParameter::No => Type::Array(ArrayType { ty, length }),
            }
        }
        ty::Type::Function(FunctionType { params, return_type, is_varargs }) => {
            let return_type = sess.alloc(typeck_ty(sess, *return_type, IsParameter::No));
            match return_type.ty {
                Type::Arithmetic(_) | Type::Pointer(_) | Type::Void => (),
                Type::Array(_) | Type::Function(_) =>
                    sess.emit(Diagnostic::InvalidFunctionReturnType {
                        at: return_type.loc,
                        ty: *return_type,
                    }),
                Type::Typeof { expr, unqual: _ } => match expr {},
            }
            Type::Function(FunctionType {
                params: sess.alloc_slice_fill_iter(params.iter().map(
                    |&ParameterDeclaration { loc, ty, name }| {
                        let ty = typeck_ty(sess, ty, IsParameter::Yes);
                        let param = ParameterDeclaration { loc, ty, name };
                        if !ty.ty.is_complete() {
                            sess.emit(Diagnostic::ParameterWithIncompleteType { at: param })
                        }
                        param
                    },
                )),
                return_type,
                is_varargs,
            })
        }
        ty::Type::Void => Type::Void,
        ty::Type::Typeof { expr, unqual } => {
            let expr = typeck_expression(sess, expr, Context::Typeof);
            return if unqual {
                expr.ty.ty.unqualified()
            }
            else {
                QualifiedType {
                    is_const: is_const | expr.ty.is_const,
                    is_volatile: is_volatile | expr.ty.is_volatile,
                    ty: expr.ty.ty,
                    loc: expr.ty.loc,
                }
            };
        }
    };
    QualifiedType { is_const, is_volatile, ty, loc }
}

fn typeck_ty<'a>(
    sess: &'a Session<'a>,
    ty: scope::QualifiedType<'a>,
    is_parameter: IsParameter,
) -> QualifiedType<'a> {
    typeck_ty_with_initialiser(sess, ty, is_parameter, None)
}

enum NeedsInitialiser {
    No,
    Yes,
}

fn typeck_reference<'a>(
    sess: &'a Session<'a>,
    reference: scope::Reference<'a>,
    needs_initialiser: NeedsInitialiser,
) -> Reference<'a> {
    let scope::Reference {
        name,
        loc,
        ty,
        id,
        usage_location,
        kind,
        storage_duration,
        previous_definition,
        is_parameter,
        initialiser: _,
    } = reference;
    let reference = Reference {
        name,
        loc,
        // TODO: this should be replaced by a more ECS-ish approach to avoid re-typechecking the
        // initialiser for every usage of references to arrays with unknown length.
        ty: typeck_ty_with_initialiser(
            sess,
            ty,
            is_parameter,
            matches!(needs_initialiser, NeedsInitialiser::Yes).then_some(&reference),
        ),
        id,
        usage_location,
        kind,
        storage_duration,
    };
    let ty = match previous_definition {
        Some(previous_definition) => {
            // TODO: this is quadratic in the number of previous decls for this name
            // TODO: this will also emit quadratically many error messages
            let previous_definition =
                typeck_reference(sess, *previous_definition, needs_initialiser);
            reference
                .ty
                .composite_ty(sess.bump(), &previous_definition.ty)
                .unwrap_or_else(|| {
                    // TODO: use this error to generate a `Type::Error`
                    let () = sess.emit(Diagnostic::AlreadyDefinedWithDifferentType {
                        at: reference,
                        previous_definition,
                    });
                    reference.ty
                })
        }
        None => reference.ty,
    };
    Reference { ty, ..reference }
}

fn convert<'a>(
    sess: &'a Session<'a>,
    target: QualifiedType<'a>,
    expr: TypedExpression<'a>,
    kind: ConversionKind,
) -> TypedExpression<'a> {
    // TODO: forbid ptr <=> float
    // TODO: only allow nullptr => {bool, void, ptr<T>}
    // TODO: if target is nullptr_t, expr must be nullptr or a null pointer constant
    // TODO: check that target is a scalar type or void
    // TODO: check that expr_ty is a scalar type when target_ty != void
    let target_ty = target.ty;
    let expr_ty = expr.ty.ty;
    let extend_kind = match expr_ty {
        Type::Arithmetic(arithmetic) => match arithmetic.signedness() {
            Signedness::Signed => Expression::SignExtend,
            Signedness::Unsigned => Expression::ZeroExtend,
        },
        _ => Expression::ZeroExtend,
    };
    let convert = || {
        let cast = match target_ty.size().cmp(&expr_ty.size()) {
            Ordering::Less => Expression::Truncate,
            Ordering::Equal => Expression::NoopTypeConversion,
            Ordering::Greater => extend_kind,
        };
        cast(sess.alloc(expr))
    };

    let expr = match (target_ty, expr_ty) {
        (Type::Void, Type::Void) => return expr,
        (Type::Void, _) if kind == ConversionKind::Explicit =>
            Expression::VoidCast(sess.alloc(expr)),

        (Type::Arithmetic(_), Type::Arithmetic(_)) | (Type::Pointer(_), Type::Pointer(_)) =>
            if kind == ConversionKind::Implicit && target_ty == expr_ty {
                return expr;
            }
            else {
                convert()
            },
        // TODO: clang (but not gcc) allows implicitly converting `Type::Function(_)` to
        // `Type::Pointer(_)` (with a warning).
        // TODO: handle nullptr literals
        (Type::Function(_), _) =>
            sess.emit(Diagnostic::IllegalInitialiserForFunction { at: expr, ty: target }),

        (Type::Arithmetic(Arithmetic::Integral(_)), Type::Pointer(_))
        | (Type::Pointer(_), Type::Arithmetic(Arithmetic::Integral(_)))
            if kind == ConversionKind::Explicit =>
            convert(),

        _ => sess.emit(Diagnostic::InvalidConversion {
            at: expr,
            from_ty: expr_ty,
            to_ty: target_ty,
            kind,
        }),
    };
    TypedExpression { ty: target_ty.unqualified(), expr }
}

fn convert_as_if_by_assignment<'a>(
    sess: &'a Session<'a>,
    target: QualifiedType<'a>,
    expr: TypedExpression<'a>,
) -> TypedExpression<'a> {
    convert(sess, target, expr, ConversionKind::Implicit)
}

fn typeck_function_definition<'a>(
    sess: &'a Session<'a>,
    definition: &scope::FunctionDefinition<'a>,
) -> FunctionDefinition<'a> {
    let scope::FunctionDefinition {
        reference,
        params,
        storage_class,
        inline,
        noreturn,
        is_varargs,
        body,
    } = *definition;

    let params = ParamRefs(
        sess.alloc_slice_fill_iter(
            params
                .0
                .iter()
                .map(|&param| typeck_reference(sess, param, NeedsInitialiser::No)),
        ),
    );

    FunctionDefinition {
        reference: typeck_reference(sess, reference, NeedsInitialiser::No),
        params,
        storage_class,
        inline,
        noreturn,
        is_varargs,
        body: typeck_compound_statement(sess, &body, definition),
    }
}

fn typeck_initialiser_list<'a>(
    sess: &'a Session<'a>,
    reference: &Reference<'a>,
    subobject_initialisers: &mut IndexMap<u64, SubobjectInitialiser<'a>>,
    subobjects: &mut Subobjects<'a>,
    initialiser_list: &[DesignatedInitialiser<'a>],
    emit_nested_excess_initialiser_errors: bool,
) {
    if let [
        DesignatedInitialiser {
            designation: None,
            initialiser: scope::Initialiser::Expression(initialiser @ scope::Expression::String(_)),
        },
    ] = initialiser_list
        && let Some(subobject) = subobjects.parent()
    {
        subobject_initialisers.insert(
            subobject.offset,
            SubobjectInitialiser {
                subobject,
                initialiser: typeck_expression(
                    sess,
                    initialiser,
                    Context::ArrayInitialisationByString,
                ),
            },
        );
        return;
    }

    for DesignatedInitialiser { designation, initialiser } in initialiser_list {
        match designation {
            Some(Designation([])) => unreachable!(),
            Some(Designation([designator, rest @ ..])) => {
                fn apply_designator<'a>(
                    sess: &'a Session<'a>,
                    reference: &Reference<'a>,
                    subobjects: &mut Subobjects<'a>,
                    designator: &Designator<'a>,
                ) {
                    match designator {
                        Designator::Bracketed { open_bracket: _, index, close_bracket: _ } => {
                            let index = typeck_expression(sess, index, Context::Default);
                            // TODO: constexpr evaluate
                            let index = match index.expr {
                                Expression::Integer { value, token: _ } => value,
                                _ => todo!(),
                            };
                            match subobjects.goto_index(index) {
                                Ok(()) => (),
                                Err(iterator) => sess.emit(Diagnostic::NoSuchSubobject {
                                    at: *designator,
                                    reference: *reference,
                                    iterator,
                                }),
                            }
                        }
                        Designator::Identifier { .. } => todo!(),
                    }
                }

                while subobjects.try_leave_subobject(AllowExplicit::No) {}
                apply_designator(sess, reference, subobjects, designator);
                for designator in rest {
                    subobjects.enter_subobject_implicit().unwrap_or_else(|_| {
                        unreachable!("only reachable for nested braced initialisation")
                    });
                    apply_designator(sess, reference, subobjects, designator);
                }
            }
            None => (),
        }
        match initialiser {
            scope::Initialiser::Braced {
                open_brace: _,
                initialiser_list,
                close_brace: _,
            } => {
                let emit_nested_errors = match subobjects.enter_subobject() {
                    Ok(()) => true,
                    Err(iterator) => {
                        if emit_nested_excess_initialiser_errors {
                            // TODO: use this error
                            sess.emit(Diagnostic::ExcessInitialiser {
                                at: **initialiser,
                                reference: *reference,
                                iterator,
                            })
                        }
                        false
                    }
                };
                typeck_initialiser_list(
                    sess,
                    reference,
                    subobject_initialisers,
                    subobjects,
                    initialiser_list,
                    emit_nested_errors,
                );
                let left = subobjects.try_leave_subobject(AllowExplicit::Yes);
                assert!(left);
            }
            scope::Initialiser::Expression(expr @ scope::Expression::String(_))
                if let Some(subobject) = subobjects.current()
                    && subobject.ty.ty.is_array() =>
            {
                let _ = subobjects.next_scalar();
                let left = subobjects.try_leave_subobject(AllowExplicit::No);
                assert!(left);
                subobject_initialisers.insert(
                    subobject.offset,
                    SubobjectInitialiser {
                        subobject,
                        initialiser: typeck_expression(
                            sess,
                            expr,
                            Context::ArrayInitialisationByString,
                        ),
                    },
                );
            }
            scope::Initialiser::Expression(expr) => match subobjects.next_scalar() {
                Ok(subobject) => {
                    subobject_initialisers.insert(
                        subobject.offset,
                        SubobjectInitialiser {
                            subobject,
                            // TODO: this skips typechecking the initialiser in the `Err` case
                            // example:
                            //     int x = {1, (void)42};
                            // this should emit an excess initialiser error *and* a type error
                            initialiser: convert_as_if_by_assignment(
                                sess,
                                subobject.ty,
                                typeck_expression(sess, expr, Context::Default),
                            ),
                        },
                    );
                }
                Err(iterator) =>
                    if emit_nested_excess_initialiser_errors {
                        // TODO: use this error (implement `ErrorExpr` for `SubobjectInitialiser`)
                        sess.emit(Diagnostic::ExcessInitialiser {
                            at: **initialiser,
                            reference: *reference,
                            iterator,
                        })
                    },
            },
        }
    }
}

fn typeck_initialiser<'a>(
    sess: &'a Session<'a>,
    initialiser: &scope::Initialiser<'a>,
    reference: &Reference<'a>,
) -> Initialiser<'a> {
    match initialiser {
        scope::Initialiser::Braced {
            open_brace: _,
            initialiser_list,
            close_brace: _,
        } => {
            let subobject_initialisers = &mut IndexMap::new();
            typeck_initialiser_list(
                sess,
                reference,
                subobject_initialisers,
                &mut Subobjects::new(reference.ty),
                initialiser_list,
                true,
            );
            subobject_initialisers.sort_unstable_keys();
            Initialiser::Braced {
                subobject_initialisers: sess
                    .alloc_slice_fill_iter(subobject_initialisers.values().copied()),
            }
        }
        scope::Initialiser::Expression(initialiser) => {
            let initialiser = match initialiser {
                scope::Expression::String(_) if reference.ty.ty.is_array() =>
                    typeck_expression(sess, initialiser, Context::ArrayInitialisationByString),
                _ => convert_as_if_by_assignment(
                    sess,
                    reference.ty,
                    typeck_expression(sess, initialiser, Context::Default),
                ),
            };
            Initialiser::Expression(initialiser)
        }
    }
}

fn typeck_declaration<'a>(
    sess: &'a Session<'a>,
    declaration: &scope::Declaration<'a>,
) -> Declaration<'a> {
    let scope::Declaration { reference, initialiser } = *declaration;
    let reference = typeck_reference(sess, reference, NeedsInitialiser::Yes);
    let initialiser = try { typeck_initialiser(sess, initialiser?, &reference) };
    let reference =
        if matches!(reference.kind, RefKind::Definition) && !reference.ty.ty.is_complete() {
            if let ty @ QualifiedType {
                ty:
                    Type::Array(ArrayType {
                        ty: element_ty,
                        length: ArrayLength::Unknown,
                    }),
                ..
            } = reference.ty
                && let Some(
                    initialiser @ (Initialiser::Braced { subobject_initialisers: _ }
                    | Initialiser::Expression(TypedExpression {
                        ty: _,
                        expr: Expression::String(_),
                    })),
                ) = initialiser
            {
                Reference {
                    ty: QualifiedType {
                        ty: Type::Array(ArrayType {
                            ty: element_ty,
                            length: ArrayLength::Constant(
                                initialiser.array_length(&element_ty.ty).unwrap(),
                            ),
                        }),
                        ..ty
                    },
                    ..reference
                }
            }
            else {
                // TODO: use this error
                let () = sess.emit(Diagnostic::VariableWithIncompleteType { at: reference });
                reference
            }
        }
        else {
            reference
        };

    if let Type::Array(ArrayType { ty: _, length: ArrayLength::Constant(0) }) = reference.ty.ty {
        // TODO: use this error
        sess.emit(Diagnostic::EmptyArray { at: reference })
    }

    Declaration { reference, initialiser }
}

fn typeck_compound_statement<'a>(
    sess: &'a Session<'a>,
    stmt: &scope::CompoundStatement<'a>,
    function: &scope::FunctionDefinition<'a>,
) -> CompoundStatement<'a> {
    CompoundStatement(
        sess.alloc_slice_fill_iter(
            stmt.0
                .iter()
                .map(|stmt| typeck_statement(sess, stmt, function)),
        ),
    )
}

fn typeck_statement<'a>(
    sess: &'a Session<'a>,
    stmt: &scope::Statement<'a>,
    function: &scope::FunctionDefinition<'a>,
) -> Statement<'a> {
    match stmt {
        scope::Statement::Declaration(decl) =>
            Statement::Declaration(typeck_declaration(sess, decl)),
        scope::Statement::Expression(expr) =>
            Statement::Expression(try { typeck_expression(sess, expr.as_ref()?, Context::Default) }),
        scope::Statement::Compound(stmt) =>
            Statement::Compound(typeck_compound_statement(sess, stmt, function)),
        scope::Statement::Return { return_: _, expr } => {
            // TODO: why are we re-typechecking the return type for each return stmt?
            let return_ty = typeck_ty(sess, *function.return_ty(), IsParameter::No);
            let expr = expr
                .as_ref()
                .map(|expr| typeck_expression(sess, expr, Context::Default));
            let expr = match expr {
                Some(expr) => Some(convert_as_if_by_assignment(sess, return_ty, expr)),
                None =>
                    if !matches!(return_ty.ty, Type::Void) {
                        Some(sess.emit(Diagnostic::ReturnWithoutValueInNonVoidFunction {
                            at: stmt.into_variant(),
                            function: *function,
                        }))
                    }
                    else {
                        None
                    },
            };
            Statement::Return(expr)
        }
    }
}

type Comparator = impl Fn(&Arithmetic) -> (impl Ord + use<>);
const SIZE_WITH_UNSIGNED_AS_TIE_BREAKER: Comparator = |ty| (ty.conversion_rank(), ty.signedness());

fn integral_promote(ty: Arithmetic) -> Arithmetic {
    match ty {
        Arithmetic::Integral(Integral {
            signedness: _,
            kind: IntegralKind::PlainChar | IntegralKind::Char | IntegralKind::Short,
        }) => Arithmetic::Integral(Integral {
            signedness: Signedness::Signed,
            kind: IntegralKind::Int,
        }),
        ty => ty,
    }
}

fn perform_usual_arithmetic_conversions(lhs_ty: Arithmetic, rhs_ty: Arithmetic) -> Arithmetic {
    // TODO: handle floats
    // TODO: convert enumerations to their underlying types

    let lhs_ty = integral_promote(lhs_ty);
    let rhs_ty = integral_promote(rhs_ty);

    if lhs_ty == rhs_ty {
        lhs_ty
    }
    else if lhs_ty.signedness() == rhs_ty.signedness() {
        std::cmp::max_by_key(lhs_ty, rhs_ty, |ty| ty.conversion_rank())
    }
    else {
        let [smaller_ty, larger_ty] =
            std::cmp::minmax_by_key(lhs_ty, rhs_ty, SIZE_WITH_UNSIGNED_AS_TIE_BREAKER);

        if matches!(larger_ty.signedness(), Signedness::Unsigned) {
            larger_ty
        }
        else if larger_ty.size() > smaller_ty.size() {
            assert!(matches!(larger_ty.signedness(), Signedness::Signed));
            larger_ty
        }
        else {
            assert!(matches!(larger_ty.signedness(), Signedness::Signed));
            #[expect(irrefutable_let_patterns)]
            let Arithmetic::Integral(Integral { signedness: _, kind }) = larger_ty
            else {
                unreachable!()
            };

            Arithmetic::Integral(Integral { signedness: Signedness::Unsigned, kind })
        }
    }
}

fn typeck_binop<'a>(
    sess: &'a Session<'a>,
    lhs: TypedExpression<'a>,
    op: &BinOp<'a>,
    rhs: TypedExpression<'a>,
) -> TypedExpression<'a> {
    match (lhs.ty.ty, rhs.ty.ty) {
        (Type::Arithmetic(lhs_ty), Type::Arithmetic(rhs_ty)) =>
            typeck_arithmetic_binop(sess, *op, lhs, rhs, lhs_ty, rhs_ty),
        (Type::Arithmetic(Arithmetic::Integral(_)), Type::Pointer(pointee_ty))
            if matches!(op.kind, BinOpKind::Add) =>
            typeck_ptradd(sess, rhs, pointee_ty, lhs, PtrAddOrder::IntegralFirst),
        (Type::Pointer(pointee_ty), Type::Arithmetic(Arithmetic::Integral(_)))
            if matches!(op.kind, BinOpKind::Add) =>
            typeck_ptradd(sess, lhs, pointee_ty, rhs, PtrAddOrder::PtrFirst),
        (Type::Pointer(pointee_ty), Type::Arithmetic(Arithmetic::Integral(_)))
            if matches!(op.kind, BinOpKind::Subtract) =>
            typeck_ptrsub(sess, lhs, pointee_ty, rhs),
        (Type::Pointer(_), Type::Pointer(_))
            if matches!(
                op.kind,
                BinOpKind::Equal
                    | BinOpKind::NotEqual
                    | BinOpKind::Less
                    | BinOpKind::LessEqual
                    | BinOpKind::Greater
                    | BinOpKind::GreaterEqual
            ) =>
            typeck_ptrcmp(sess, lhs, *op, rhs),
        (Type::Pointer(lhs_pointee_ty), Type::Pointer(rhs_pointee_ty))
            if matches!(op.kind, BinOpKind::Subtract) =>
            typeck_ptrdiff(sess, op, lhs, lhs_pointee_ty, rhs, rhs_pointee_ty),
        _ => sess.emit(Diagnostic::InvalidOperandsForBinaryOperator { at: *op, lhs, rhs }),
    }
}

fn check_assignable<'a>(
    target: &'a TypedExpression<'a>,
    sess: &'a Session<'a>,
) -> &'a TypedExpression<'a> {
    if !target.is_modifiable_lvalue() {
        let error = if !target.is_lvalue() {
            sess.emit(Diagnostic::AssignmentToNonLValue { at: &target.expr })
        }
        else {
            assert!(!target.is_modifiable());
            match target.expr.unwrap_parens() {
                Expression::Name(reference) => sess.emit(Diagnostic::AssignmentToConstName {
                    at: &target.expr,
                    decl: reference.at_decl(),
                }),
                Expression::Deref { .. } =>
                    sess.emit(Diagnostic::AssignmentToConst { at: target.expr }),
                _ => todo!("for const struct members etc."),
            }
        };
        sess.alloc(TypedExpression { ty: target.ty, expr: error })
    }
    else {
        target
    }
}

fn typeck_assign<'a>(
    target: &'a TypedExpression<'a>,
    sess: &'a Session<'a>,
    typeck_value: impl FnOnce() -> TypedExpression<'a>,
) -> TypedExpression<'a> {
    let target = check_assignable(target, sess);
    let value = typeck_value();
    let value = sess.alloc(convert_as_if_by_assignment(sess, target.ty, value));
    TypedExpression {
        // TODO: `ty` is the type that `target` would have after lvalue conversion, so it
        // might be necessary to add a `NoopTypeConversion` here
        ty: target.ty,
        expr: Expression::Assign { target, value },
    }
}

fn typeck_arithmetic_binop<'a>(
    sess: &'a Session<'a>,
    op: BinOp<'a>,
    lhs: TypedExpression<'a>,
    rhs: TypedExpression<'a>,
    lhs_ty: Arithmetic,
    rhs_ty: Arithmetic,
) -> TypedExpression<'a> {
    let Arithmetic::Integral(integral_ty) = perform_usual_arithmetic_conversions(lhs_ty, rhs_ty);
    let common_ty = Type::Arithmetic(Arithmetic::Integral(integral_ty)).unqualified();
    let ty = match op.kind {
        BinOpKind::Multiply
        | BinOpKind::Divide
        | BinOpKind::Modulo
        | BinOpKind::Add
        | BinOpKind::Subtract
        | BinOpKind::BitAnd
        | BinOpKind::BitXor
        | BinOpKind::BitOr => common_ty,
        BinOpKind::LeftShift | BinOpKind::RightShift =>
            return typeck_integral_shift(sess, op, lhs, rhs, lhs_ty, rhs_ty),
        BinOpKind::Equal
        | BinOpKind::NotEqual
        | BinOpKind::Less
        | BinOpKind::LessEqual
        | BinOpKind::Greater
        | BinOpKind::GreaterEqual => Type::int().unqualified(),
    };
    let lhs = convert_as_if_by_assignment(sess, common_ty, lhs);
    let rhs = convert_as_if_by_assignment(sess, common_ty, rhs);
    TypedExpression {
        ty,
        expr: Expression::IntegralBinOp {
            ty: integral_ty,
            lhs: sess.alloc(lhs),
            op,
            rhs: sess.alloc(rhs),
        },
    }
}

fn typeck_integral_shift<'a>(
    sess: &'a Session<'a>,
    op: BinOp<'a>,
    lhs: TypedExpression<'a>,
    rhs: TypedExpression<'a>,
    lhs_ty: Arithmetic,
    rhs_ty: Arithmetic,
) -> TypedExpression<'a> {
    assert!(matches!(
        op.kind,
        BinOpKind::LeftShift | BinOpKind::RightShift,
    ));
    let lhs_ty @ Arithmetic::Integral(lhs_integral) = integral_promote(lhs_ty);
    let lhs_ty = Type::Arithmetic(lhs_ty).unqualified();
    let rhs_ty = Type::Arithmetic(integral_promote(rhs_ty)).unqualified();
    let lhs = convert_as_if_by_assignment(sess, lhs_ty, lhs);
    let rhs = convert_as_if_by_assignment(sess, rhs_ty, rhs);
    TypedExpression {
        ty: lhs_ty,
        expr: Expression::IntegralBinOp {
            ty: lhs_integral,
            lhs: sess.alloc(lhs),
            op,
            rhs: sess.alloc(rhs),
        },
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PtrAddOrder {
    PtrFirst,
    IntegralFirst,
}

impl PtrAddOrder {
    pub fn select<T>(self, pointer: T, integral: T) -> (T, T) {
        match self {
            Self::PtrFirst => (pointer, integral),
            Self::IntegralFirst => (integral, pointer),
        }
    }
}

fn typeck_ptradd<'a>(
    sess: &'a Session<'a>,
    pointer: TypedExpression<'a>,
    pointee_ty: &QualifiedType<'a>,
    integral: TypedExpression<'a>,
    order: PtrAddOrder,
) -> TypedExpression<'a> {
    assert!(matches!(pointer.ty.ty, Type::Pointer(_)));
    assert!(matches!(
        integral.ty.ty,
        Type::Arithmetic(Arithmetic::Integral(_)),
    ));
    let integral = convert_as_if_by_assignment(sess, Type::size_t().unqualified(), integral);
    TypedExpression {
        ty: pointer.ty.ty.unqualified(),
        expr: Expression::PtrAdd {
            pointer: sess.alloc(pointer),
            integral: sess.alloc(integral),
            pointee_size: pointee_ty.ty.size(),
            order,
        },
    }
}

fn typeck_ptrsub<'a>(
    sess: &'a Session<'a>,
    pointer: TypedExpression<'a>,
    pointee_ty: &QualifiedType<'a>,
    integral: TypedExpression<'a>,
) -> TypedExpression<'a> {
    assert!(matches!(pointer.ty.ty, Type::Pointer(_)));
    assert!(matches!(
        integral.ty.ty,
        Type::Arithmetic(Arithmetic::Integral(_)),
    ));
    let integral = convert_as_if_by_assignment(sess, Type::size_t().unqualified(), integral);
    TypedExpression {
        ty: pointer.ty.ty.unqualified(),
        expr: Expression::PtrSub {
            pointer: sess.alloc(pointer),
            integral: sess.alloc(integral),
            pointee_size: pointee_ty.ty.size(),
        },
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PtrCmpKind {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

impl PtrCmpKind {
    pub(crate) fn str(&self) -> &'static str {
        match self {
            PtrCmpKind::Equal => "ptr-equal",
            PtrCmpKind::NotEqual => "ptr-not-equal",
            PtrCmpKind::Less => "ptr-less",
            PtrCmpKind::LessEqual => "ptr-less-equal",
            PtrCmpKind::Greater => "ptr-greater",
            PtrCmpKind::GreaterEqual => "ptr-greater-equal",
        }
    }
}

fn typeck_ptrcmp<'a>(
    sess: &'a Session<'a>,
    lhs: TypedExpression<'a>,
    op: BinOp<'a>,
    rhs: TypedExpression<'a>,
) -> TypedExpression<'a> {
    // TODO: should check for type compatibility, not exact equality
    let expr = if lhs.ty.ty != rhs.ty.ty {
        sess.emit(Diagnostic::IncompatibleTypesInPtrCmp { at: op.token, lhs, rhs })
    }
    else {
        let kind = match op.kind {
            BinOpKind::Multiply
            | BinOpKind::Divide
            | BinOpKind::Modulo
            | BinOpKind::Add
            | BinOpKind::Subtract
            | BinOpKind::LeftShift
            | BinOpKind::RightShift
            | BinOpKind::BitAnd
            | BinOpKind::BitXor
            | BinOpKind::BitOr => unreachable!(),
            BinOpKind::Equal => PtrCmpKind::Equal,
            BinOpKind::NotEqual => PtrCmpKind::NotEqual,
            BinOpKind::Less => PtrCmpKind::Less,
            BinOpKind::LessEqual => PtrCmpKind::LessEqual,
            BinOpKind::Greater => PtrCmpKind::Greater,
            BinOpKind::GreaterEqual => PtrCmpKind::GreaterEqual,
        };
        Expression::PtrCmp {
            lhs: sess.alloc(lhs),
            kind,
            rhs: sess.alloc(rhs),
        }
    };
    TypedExpression { ty: Type::int().unqualified(), expr }
}

fn typeck_ptrdiff<'a>(
    sess: &'a Session<'a>,
    op: &BinOp<'a>,
    lhs: TypedExpression<'a>,
    lhs_pointee_ty: &QualifiedType<'a>,
    rhs: TypedExpression<'a>,
    rhs_pointee_ty: &QualifiedType<'a>,
) -> TypedExpression<'a> {
    let expr = if lhs_pointee_ty.ty != rhs_pointee_ty.ty {
        sess.emit(Diagnostic::PtrDiffIncompatiblePointeeTypes { at: op.token, lhs, rhs })
    }
    else {
        Expression::PtrDiff {
            lhs: sess.alloc(lhs),
            rhs: sess.alloc(rhs),
            pointee_size: lhs_pointee_ty.ty.size(),
        }
    };

    TypedExpression {
        ty: Type::ptrdiff_t().unqualified(),
        expr,
    }
}

fn check_ty_can_sizeof<'a>(
    sess: &'a Session<'a>,
    ty: &QualifiedType<'a>,
    at: &scope::Expression<'a>,
    op: &Token<'a>,
) -> QualifiedType<'a> {
    // TODO: the error cases should return `Type::Error`
    if !ty.ty.is_complete() {
        let () = sess.emit(Diagnostic::InvalidSizeofOrAlignof {
            op: *op,
            at: *at,
            kind: "incomplete type",
            ty: *ty,
        });
        Type::int().unqualified()
    }
    else if ty.ty.is_function() {
        let () = sess.emit(Diagnostic::InvalidSizeofOrAlignof {
            op: *op,
            at: *at,
            kind: "function type",
            ty: *ty,
        });
        Type::int().unqualified()
    }
    else {
        *ty
    }
}

fn desugar_postfix_increment<'a>(
    sess: &'a Session<'a>,
    op: BinOp<'a>,
    operand: &'a scope::Expression<'a>,
    reference: &scope::Reference<'a>,
    pointer: &scope::Reference<'a>,
    copy: &scope::Reference<'a>,
) -> scope::Expression<'a> {
    // `x++` is mostly equivalent to the following expression, so we use that as a desugaring:
    // __pointer = &x, __copy = *__pointer, *__pointer += 1, __copy

    // TODO: this desugaring has the same problem as for `CompoundAssign`: bitfields
    // are not supported.

    let token = op.token;

    let pointer = sess.alloc(scope::Expression::Name(*pointer));
    let copy = sess.alloc(scope::Expression::Name(*copy));

    scope::Expression::Comma {
        lhs: sess.alloc(scope::Expression::Comma {
            lhs: sess.alloc(scope::Expression::Comma {
                // pointer = &operand
                lhs: sess.alloc(scope::Expression::Assign {
                    target: pointer,
                    value: sess.alloc(scope::Expression::UnaryOp {
                        operator: UnaryOp { kind: UnaryOpKind::Addressof, token },
                        operand,
                    }),
                }),
                // copy = *pointer
                rhs: sess.alloc(scope::Expression::Assign {
                    target: copy,
                    value: sess.alloc(scope::Expression::UnaryOp {
                        operator: UnaryOp { kind: UnaryOpKind::Deref, token },
                        operand: pointer,
                    }),
                }),
            }),
            // *pointer <op>= 1
            rhs: sess.alloc(scope::Expression::CompoundAssign {
                target: sess.alloc(scope::Expression::UnaryOp {
                    operator: UnaryOp { kind: UnaryOpKind::Deref, token },
                    operand: pointer,
                }),
                target_temporary: *reference,
                op,
                value: sess.alloc(scope::Expression::Integer {
                    value: "1",
                    token: Token::synthesised(
                        TokenKind::Integer(panko_lex::Integer {
                            suffix: IntegerSuffix::None,
                            suffix_len: 0,
                            base: 10,
                            prefix_len: 0,
                        }),
                        token.loc(),
                    ),
                }),
            }),
        }),
        rhs: copy,
    }
}

gen fn parse_escape_sequences(mut chars: Chars<'_>) -> char {
    let char_from_codepoint = |codepoint| {
        char::from_u32(codepoint).unwrap_or_else(|| todo!("error: invalid escape sequence"))
    };

    while let Some(c) = chars.next() {
        match c {
            '\\' =>
                yield match chars.next() {
                    None => unreachable!(),
                    Some('\'') => '\'',
                    Some('"') => '"',
                    Some('?') => '?',
                    Some('\\') => '\\',
                    Some('a') => '\x07',
                    Some('b') => '\x08',
                    Some('f') => '\x0c',
                    Some('n') => '\n',
                    Some('r') => '\r',
                    Some('t') => '\t',
                    Some('v') => '\x0b',
                    Some(oct_digit @ '0'..='7') => {
                        let value = oct_digit.to_digit(8).unwrap();
                        match chars.next() {
                            None => char_from_codepoint(value),
                            Some(oct_digit @ '0'..='7') => {
                                let value = value * 8 + oct_digit.to_digit(8).unwrap();
                                match chars.next() {
                                    None => char_from_codepoint(value),
                                    Some(oct_digit @ '0'..='7') => {
                                        let value = value * 8 + oct_digit.to_digit(8).unwrap();
                                        char_from_codepoint(value)
                                    }
                                    Some(c) => {
                                        yield char_from_codepoint(value);
                                        c
                                    }
                                }
                            }
                            Some(c) => {
                                yield char_from_codepoint(value);
                                c
                            }
                        }
                    }
                    Some('x') => {
                        let s = chars.as_str();
                        let split_point =
                            s.find(|c: char| !c.is_ascii_hexdigit()).unwrap_or(s.len());
                        let (digits, rest) = s.split_at(split_point);
                        chars = rest.chars();
                        match u32::from_str_radix(digits, 16) {
                            Ok(codepoint) => char_from_codepoint(codepoint),
                            Err(_) => unreachable!(),
                        }
                    }
                    Some(_) => todo!("error: invalid escape sequence"),
                },
            c => yield c,
        }
    }
}

fn parse_char_literal(char: &Token) -> Option<u64> {
    Some(u64::from(
        // TODO: handle multichar character constants
        parse_escape_sequences(char.slice()[1..char.slice().len() - 1].chars()).next()?,
    ))
}

fn parse_string_literal<'a>(sess: &'a Session<'a>, tokens: &[Token<'a>]) -> StringLiteral<'a> {
    let value: String = tokens
        .iter()
        .flat_map(|token| parse_escape_sequences(token.slice()[1..token.slice().len() - 1].chars()))
        .chain(std::iter::once('\0'))
        .collect();
    StringLiteral {
        value: sess.alloc_str(&value),
        loc: tokens
            .first()
            .unwrap()
            .loc()
            .until(tokens.last().unwrap().loc()),
    }
}

#[derive(Debug, Clone, Copy)]
enum Context {
    Default,
    Addressof,
    Sizeof,
    Typeof,
    ArrayInitialisationByString,
}

fn typeck_expression<'a>(
    sess: &'a Session<'a>,
    expr: &scope::Expression<'a>,
    context: Context,
) -> TypedExpression<'a> {
    let expr = match expr {
        scope::Expression::Error(error) => TypedExpression::from_error(*error),
        scope::Expression::Name(reference) => {
            let reference = typeck_reference(sess, *reference, NeedsInitialiser::Yes);
            TypedExpression {
                ty: reference.ty,
                expr: Expression::Name(reference),
            }
        }
        scope::Expression::Integer { value, token } => {
            let TokenKind::Integer(Integer { suffix, suffix_len, base, prefix_len }) = token.kind
            else {
                unreachable!()
            };
            let number = &value[prefix_len..value.len() - suffix_len];
            let number: String = number.chars().filter(|&c| c != '\'').collect();
            let (signedness, kind) = match suffix {
                IntegerSuffix::None => (Signedness::Signed, IntegralKind::Int),
                IntegerSuffix::Unsigned => (Signedness::Unsigned, IntegralKind::Int),
                IntegerSuffix::UnsignedLong => (Signedness::Unsigned, IntegralKind::Long),
                IntegerSuffix::UnsignedLongLong => (Signedness::Unsigned, IntegralKind::LongLong),
                IntegerSuffix::Long => (Signedness::Signed, IntegralKind::Long),
                IntegerSuffix::LongLong => (Signedness::Signed, IntegralKind::LongLong),
                IntegerSuffix::BitInt => todo!("unimplemented: `_BitInt`"),
                IntegerSuffix::UnsignedBitInt => todo!("unimplemented: `unsigned _BitInt`"),
                IntegerSuffix::Invalid => {
                    // TODO: use the error
                    let () = sess.emit(Diagnostic::InvalidIntegerSuffix {
                        at: *token,
                        suffix: &value[value.len() - suffix_len..],
                    });
                    (Signedness::Signed, IntegralKind::Int)
                }
            };
            match u64::from_str_radix(&number, base) {
                Ok(parsed_value) => {
                    let integral_ty = grow_to_fit(signedness, kind, base, parsed_value);
                    TypedExpression {
                        ty: Type::Arithmetic(Arithmetic::Integral(integral_ty)).unqualified(),
                        expr: Expression::Integer { value: parsed_value, token: *token },
                    }
                }
                Err(error) => match error.kind() {
                    IntErrorKind::PosOverflow =>
                        sess.emit(Diagnostic::IntegerLiteralTooLarge { at: *token }),
                    _ => unreachable!(),
                },
            }
        }
        scope::Expression::CharConstant(char) => TypedExpression {
            ty: Type::int().unqualified(),
            expr: match parse_char_literal(char) {
                Some(value) => Expression::Integer { value, token: *char },
                None => sess.emit(Diagnostic::EmptyCharConstant { at: *char }),
            },
        },
        scope::Expression::String(tokens) => {
            let string = parse_string_literal(sess, tokens);
            TypedExpression {
                ty: Type::Array(ArrayType {
                    ty: sess.alloc(Type::char().unqualified()),
                    length: ArrayLength::Constant(string.len()),
                })
                .unqualified(),
                expr: Expression::String(string),
            }
        }
        scope::Expression::Parenthesised { open_paren, expr, close_paren } => {
            let expr = sess.alloc(typeck_expression(sess, expr, context));
            TypedExpression {
                ty: expr.ty,
                expr: Expression::Parenthesised {
                    open_paren: *open_paren,
                    expr,
                    close_paren: *close_paren,
                },
            }
        }
        scope::Expression::Assign { target, value } => {
            let target = sess.alloc(typeck_expression(sess, target, Context::Default));
            typeck_assign(target, sess, || {
                typeck_expression(sess, value, Context::Default)
            })
        }
        scope::Expression::CompoundAssign { target, target_temporary, op, value } => {
            let target = sess.alloc(typeck_expression(sess, target, Context::Default));
            let target = check_assignable(target, sess);
            let ty = target.ty;
            let target = sess.alloc(TypedExpression {
                ty: Type::Pointer(&target.ty).unqualified(),
                // TODO: `op.token` should not be included in the synthesised expr’s location
                // TODO: this will fail for bitfields
                expr: Expression::Addressof { ampersand: op.token, operand: target },
            });
            let target_temporary = typeck_reference(sess, *target_temporary, NeedsInitialiser::No);
            let target_addr = sess.alloc(TypedExpression {
                ty: target.ty,
                expr: Expression::Name(Reference { ty: target.ty, ..target_temporary }),
            });
            let deref_target_addr = sess.alloc(TypedExpression {
                ty,
                // TODO: `op.token` should not be included in the synthesised expr’s location
                expr: Expression::Deref { star: op.token, operand: target_addr },
            });
            let first = TypedExpression {
                ty: target.ty,
                expr: Expression::Assign { target: target_addr, value: target },
            };
            // TODO: when `target` is `const` this emits a duplicate “cannot assign to `const`”
            // error because we already check that `target` is assignable
            let second = typeck_assign(deref_target_addr, sess, || {
                let value = typeck_expression(sess, value, Context::Default);
                typeck_binop(sess, *deref_target_addr, op, value)
            });
            TypedExpression {
                // TODO: `ty` is the type that `target` would have after lvalue conversion, so it
                // might be necessary to add a `NoopTypeConversion` here
                ty,
                expr: Expression::Combine {
                    first: sess.alloc(first),
                    second: sess.alloc(second),
                },
            }
        }
        scope::Expression::BinOp { lhs, op, rhs } => {
            let lhs = typeck_expression(sess, lhs, Context::Default);
            let rhs = typeck_expression(sess, rhs, Context::Default);
            typeck_binop(sess, lhs, op, rhs)
        }
        scope::Expression::UnaryOp { operator, operand } => {
            let context = match operator.kind {
                UnaryOpKind::Addressof => Context::Addressof,
                UnaryOpKind::Sizeof => Context::Sizeof,
                UnaryOpKind::Lengthof => Context::Sizeof,
                _ => Context::Default,
            };
            let operand = typeck_expression(sess, operand, context);
            match operator.kind {
                UnaryOpKind::Addressof => {
                    let is_function_designator = operand.ty.ty.is_function();
                    // requirement “the result of a [] or unary * operator” is checked by
                    // `is_lvalue`.
                    let is_lvalue = operand.is_lvalue()
                        // TODO
                        /* && !operand.is_bitfield() && !operand.has_register_storage_class() */;
                    if !(is_function_designator || is_lvalue) {
                        // TODO: update error message for bitfields and `register` storage class
                        sess.emit(Diagnostic::CannotTakeAddress {
                            at: operand,
                            reason: "not an lvalue",
                        })
                    }
                    else {
                        TypedExpression {
                            ty: Type::Pointer(sess.alloc(operand.ty)).unqualified(),
                            expr: Expression::Addressof {
                                ampersand: operator.token,
                                operand: sess.alloc(operand),
                            },
                        }
                    }
                }
                UnaryOpKind::Deref => match operand.ty.ty {
                    Type::Pointer(pointee_ty) => {
                        if matches!(pointee_ty.ty, Type::Void) {
                            sess.emit(Diagnostic::DerefOfVoidPtr { at: operand })
                        }
                        TypedExpression {
                            ty: *pointee_ty,
                            expr: Expression::Deref {
                                star: operator.token,
                                operand: sess.alloc(operand),
                            },
                        }
                    }
                    _ => {
                        let expr = sess.emit(Diagnostic::CannotDeref { at: *expr, ty: operand.ty });
                        // TODO: should be `Type::Error`
                        TypedExpression { ty: operand.ty, expr }
                    }
                },
                UnaryOpKind::Plus => match operand.ty.ty {
                    Type::Arithmetic(arithmetic) => {
                        let result_ty =
                            Type::Arithmetic(integral_promote(arithmetic)).unqualified();
                        convert_as_if_by_assignment(sess, result_ty, operand)
                    }
                    _ => TypedExpression {
                        // TODO: should be `Type::Error`
                        ty: operand.ty,
                        expr: sess.emit(Diagnostic::InvalidOperandForUnaryOperator {
                            at: *expr,
                            ty: operand.ty,
                            operator_name: "unary plus",
                            expected_ty: "arithmetic",
                        }),
                    },
                },
                UnaryOpKind::Negate => match operand.ty.ty {
                    Type::Arithmetic(arithmetic) => {
                        let result_ty =
                            Type::Arithmetic(integral_promote(arithmetic)).unqualified();
                        let operand = convert_as_if_by_assignment(sess, result_ty, operand);
                        TypedExpression {
                            ty: result_ty,
                            expr: Expression::Negate {
                                minus: operator.token,
                                operand: sess.alloc(operand),
                            },
                        }
                    }
                    _ => TypedExpression {
                        // TODO: should be `Type::Error`
                        ty: operand.ty,
                        expr: sess.emit(Diagnostic::InvalidOperandForUnaryOperator {
                            at: *expr,
                            ty: operand.ty,
                            operator_name: "unary minus",
                            expected_ty: "arithmetic",
                        }),
                    },
                },
                UnaryOpKind::Compl => match operand.ty.ty {
                    Type::Arithmetic(Arithmetic::Integral(integral)) => {
                        let result_ty =
                            Type::Arithmetic(integral_promote(Arithmetic::Integral(integral)))
                                .unqualified();
                        let operand = convert_as_if_by_assignment(sess, result_ty, operand);
                        TypedExpression {
                            ty: result_ty,
                            expr: Expression::Compl {
                                compl: operator.token,
                                operand: sess.alloc(operand),
                            },
                        }
                    }
                    _ => TypedExpression {
                        // TODO: should be `Type::Error`
                        ty: operand.ty,
                        expr: sess.emit(Diagnostic::InvalidOperandForUnaryOperator {
                            at: *expr,
                            ty: operand.ty,
                            operator_name: "bitwise complement",
                            expected_ty: "integral",
                        }),
                    },
                },
                UnaryOpKind::Not =>
                    if operand.ty.ty.is_scalar() {
                        TypedExpression {
                            ty: Type::int().unqualified(),
                            expr: Expression::Not {
                                not: operator.token,
                                operand: sess.alloc(operand),
                            },
                        }
                    }
                    else {
                        sess.emit(Diagnostic::ScalarExpected {
                            at: operand,
                            expr: *expr,
                            kind: "unary not",
                        })
                    },
                UnaryOpKind::Sizeof => {
                    let ty = check_ty_can_sizeof(sess, &operand.ty, expr, &operator.token);
                    TypedExpression {
                        ty: Type::size_t().unqualified(),
                        expr: Expression::Sizeof {
                            sizeof: operator.token,
                            operand: sess.alloc(operand),
                            size: ty.ty.size(),
                        },
                    }
                }
                UnaryOpKind::Lengthof => {
                    let expr = match operand.ty.ty {
                        Type::Array(ArrayType { ty: _, length }) => match length {
                            ArrayLength::Constant(length) => Expression::Lengthof {
                                lengthof: operator.token,
                                operand: sess.alloc(operand),
                                length,
                            },
                            ArrayLength::Variable(_) =>
                                todo!("`_Lengthof` of a variably-modified type"),
                            ArrayLength::Unknown =>
                                sess.emit(Diagnostic::LengthofOfArrayOfUnknownLength {
                                    op: operator.token,
                                    at: operand,
                                }),
                        },
                        _ => sess
                            .emit(Diagnostic::InvalidLengthof { op: operator.token, at: operand }),
                    };
                    TypedExpression { ty: Type::size_t().unqualified(), expr }
                }
            }
        }
        scope::Expression::Call { callee, args, close_paren } => {
            let callee = typeck_expression(sess, callee, Context::Default);

            let Type::Pointer(&QualifiedType {
                is_const: _,
                is_volatile: _,
                ty: Type::Function(FunctionType { params, return_type, is_varargs }),
                loc: _,
            }) = callee.ty.ty
            else {
                todo!("type error: uncallable");
            };

            let has_arity_mismatch = if is_varargs {
                params.len() > args.len()
            }
            else {
                params.len() != args.len()
            };
            if has_arity_mismatch {
                return sess.emit(Diagnostic::ArityMismatch {
                    at: callee,
                    expected: params.len(),
                    actual: args.len(),
                    is_varargs,
                });
            }

            let default_argument_promote = |arg: TypedExpression<'a>| match arg.ty.ty {
                Type::Arithmetic(ty @ Arithmetic::Integral(_)) =>
                    Type::Arithmetic(integral_promote(ty)),
                ty => ty,
            };

            let args = args
                .iter()
                .zip_longest(params)
                .map(|arg_param| match arg_param {
                    EitherOrBoth::Both(arg, param) => {
                        let arg = typeck_expression(sess, arg, Context::Default);
                        convert_as_if_by_assignment(sess, param.ty, arg)
                    }
                    EitherOrBoth::Left(arg) => {
                        let arg = typeck_expression(sess, arg, Context::Default);
                        let ty = default_argument_promote(arg).unqualified();
                        convert_as_if_by_assignment(sess, ty, arg)
                    }
                    EitherOrBoth::Right(_param) => unreachable!(),
                });

            TypedExpression {
                ty: *return_type,
                expr: Expression::Call {
                    callee: sess.alloc(callee),
                    args: sess.alloc_slice_fill_iter(args),
                    is_varargs,
                    close_paren: *close_paren,
                },
            }
        }
        scope::Expression::Sizeof { sizeof, ty, close_paren } => {
            let ty = typeck_ty(sess, *ty, IsParameter::No);
            let ty = check_ty_can_sizeof(sess, &ty, expr, sizeof);
            TypedExpression {
                ty: Type::size_t().unqualified(),
                expr: Expression::SizeofTy {
                    sizeof: *sizeof,
                    ty,
                    size: ty.ty.size(),
                    close_paren: *close_paren,
                },
            }
        }
        scope::Expression::Lengthof { lengthof, ty, close_paren } => {
            let ty = typeck_ty(sess, *ty, IsParameter::No);
            let expr = match ty.ty {
                Type::Array(ArrayType { ty: _, length }) => match length {
                    ArrayLength::Constant(length) => Expression::LengthofTy {
                        lengthof: *lengthof,
                        ty,
                        length,
                        close_paren: *close_paren,
                    },
                    ArrayLength::Variable(_) => todo!("_Lengthof of a variably-modified type"),
                    ArrayLength::Unknown =>
                        sess.emit(Diagnostic::LengthofOfArrayTyOfUnknownLength {
                            op: *lengthof,
                            at: ty,
                        }),
                },
                _ => sess.emit(Diagnostic::InvalidLengthofTy { op: *lengthof, at: ty }),
            };
            TypedExpression { ty: Type::size_t().unqualified(), expr }
        }
        scope::Expression::Alignof { alignof, ty, close_paren } => {
            let ty = typeck_ty(sess, *ty, IsParameter::No);
            let ty = check_ty_can_sizeof(sess, &ty, expr, alignof);
            TypedExpression {
                ty: Type::size_t().unqualified(),
                expr: Expression::Alignof {
                    alignof: *alignof,
                    ty,
                    align: ty.ty.align(),
                    close_paren: *close_paren,
                },
            }
        }
        scope::Expression::Cast { open_paren: _, ty, expr } => {
            let expr = typeck_expression(sess, expr, Context::Default);
            let ty = typeck_ty(sess, ty.ty.unqualified(), IsParameter::No);
            convert(sess, ty, expr, ConversionKind::Explicit)
        }
        scope::Expression::Subscript { lhs, rhs, close_bracket } => typeck_expression(
            sess,
            &scope::Expression::UnaryOp {
                operator: UnaryOp {
                    kind: UnaryOpKind::Deref,
                    // TODO: Is this cheating?
                    token: *close_bracket,
                },
                operand: sess.alloc(scope::Expression::BinOp {
                    lhs,
                    op: BinOp {
                        kind: BinOpKind::Add,
                        // TODO: Also cheating?
                        token: *close_bracket,
                    },
                    rhs,
                }),
            },
            context,
        ),
        scope::Expression::Generic {
            generic,
            selector,
            assocs,
            close_paren: _,
        } => {
            let selector = typeck_expression(sess, selector, Context::Default);
            let selector_ty = selector.ty.ty.unqualified();
            let default = assocs
                .0
                .iter()
                .filter_map(|assoc| match assoc {
                    GenericAssociation::Ty { ty: _, expr: _ } => None,
                    GenericAssociation::Default { default, expr } => Some((default, expr)),
                })
                .at_most_one()
                .unwrap_or_else(|mut duplicate_defaults| {
                    let first = duplicate_defaults.next();
                    for (default, _) in duplicate_defaults {
                        sess.emit(Diagnostic::GenericWithDuplicateDefault {
                            at: *default,
                            previous_default: *first.unwrap().0,
                            generic: *generic,
                        })
                    }
                    first
                })
                .map(|(_default_token, expr)| expr);

            let assocs = assocs
                .0
                .iter()
                .filter_map(|assoc| match assoc {
                    GenericAssociation::Ty { ty, expr } => {
                        let ty = typeck_ty(sess, *ty, IsParameter::No);
                        // TODO: `ty` shall not be a variably modified type.
                        if !(ty.ty.is_object() && ty.ty.is_complete()) {
                            sess.emit(Diagnostic::GenericWithInvalidType {
                                at: ty,
                                generic: *generic,
                            })
                        }
                        Some((ty, expr))
                    }
                    GenericAssociation::Default { default: _, expr: _ } => None,
                })
                .into_grouping_map_by(|(ty, _expr)| *ty)
                .reduce(|first, _ty, duplicate| {
                    // intentionally ignored because we can continue translation with first
                    // duplicate
                    let () = sess.emit(Diagnostic::GenericWithDuplicateType {
                        at: duplicate.0,
                        previous: first.0,
                        generic: *generic,
                    });
                    first
                });

            assocs
                .get(&selector_ty)
                .map(|(_ty, expr)| expr)
                .or(default.as_ref())
                .map(|expr| typeck_expression(sess, expr, context))
                .unwrap_or_else(|| {
                    let expr =
                        sess.emit(Diagnostic::GenericWithoutMatch { at: selector, expr: *expr });
                    TypedExpression { ty: Type::int().unqualified(), expr }
                })
        }
        scope::Expression::Logical { lhs, op, rhs } => {
            let check_scalar = |operand: TypedExpression<'a>| {
                if operand.ty.ty.is_scalar() {
                    operand
                }
                else {
                    let kind = sess.alloc_str(&format!("logical {}", op.str()));
                    sess.emit(Diagnostic::ScalarExpected { at: operand, expr: *expr, kind })
                }
            };
            let lhs = check_scalar(typeck_expression(sess, lhs, Context::Default));
            let rhs = check_scalar(typeck_expression(sess, rhs, Context::Default));
            TypedExpression {
                ty: Type::int().unqualified(),
                expr: Expression::Logical {
                    lhs: sess.alloc(lhs),
                    op: *op,
                    rhs: sess.alloc(rhs),
                },
            }
        }
        scope::Expression::Conditional { condition, then, or_else } => {
            let condition = typeck_expression(sess, condition, Context::Default);
            let condition = if condition.ty.ty.is_scalar() {
                condition
            }
            else {
                sess.emit(Diagnostic::ScalarExpected {
                    at: condition,
                    expr: *expr,
                    kind: "ternary",
                })
            };
            let then = typeck_expression(sess, then, Context::Default);
            let or_else = typeck_expression(sess, or_else, Context::Default);
            // TODO: some rules are unimplemented
            let result_ty = match (then.ty.ty, or_else.ty.ty) {
                (Type::Arithmetic(then_ty), Type::Arithmetic(or_else_ty)) =>
                    Type::Arithmetic(perform_usual_arithmetic_conversions(then_ty, or_else_ty)),
                (Type::Void, Type::Void) => Type::Void,
                (Type::Pointer(then_pointee), Type::Pointer(or_else_pointee))
                    if then_pointee.ty == or_else_pointee.ty
                        || then_pointee.ty == Type::Void
                        || or_else_pointee.ty == Type::Void =>
                    Type::Pointer(sess.alloc(QualifiedType {
                        is_const: then_pointee.is_const | or_else_pointee.is_const,
                        is_volatile: then_pointee.is_volatile | or_else_pointee.is_volatile,
                        ty: if then_pointee.ty == Type::Void || or_else_pointee.ty == Type::Void {
                            Type::Void
                        }
                        else {
                            then_pointee.ty
                        },
                        loc: Loc::synthesised(),
                    })),
                (Type::Pointer(_), Type::Pointer(_)) => todo!(
                    "type error: pointer type mismatch: `{}` != `{}`",
                    then.ty.ty,
                    or_else.ty.ty,
                ),
                (Type::Array(_), _) | (_, Type::Array(_)) => unreachable!(),
                (Type::Function(_), _) | (_, Type::Function(_)) => unreachable!(),
                (Type::Arithmetic(_), Type::Pointer(_) | Type::Void)
                | (Type::Pointer(_), Type::Arithmetic(_) | Type::Void)
                | (Type::Void, _) => todo!(
                    "type error: incompatible operand types for conditional expression: `{}` != `{}`",
                    then.ty.ty,
                    or_else.ty.ty,
                ),
            };
            let result_ty = result_ty.unqualified();
            TypedExpression {
                ty: result_ty,
                expr: Expression::Conditional {
                    condition: sess.alloc(condition),
                    then: sess.alloc(convert_as_if_by_assignment(sess, result_ty, then)),
                    or_else: sess.alloc(convert_as_if_by_assignment(sess, result_ty, or_else)),
                },
            }
        }
        scope::Expression::Comma { lhs, rhs } => {
            let lhs = typeck_expression(sess, lhs, Context::Default);
            let rhs = typeck_expression(sess, rhs, Context::Default);
            TypedExpression {
                ty: rhs.ty,
                expr: Expression::Combine {
                    first: sess.alloc(lhs),
                    second: sess.alloc(rhs),
                },
            }
        }
        scope::Expression::Increment { operator, operand, fixity, reference } => {
            let kind = match operator.kind() {
                IncrementOpKind::Increment => BinOpKind::Add,
                IncrementOpKind::Decrement => BinOpKind::Subtract,
            };
            let op = BinOp { kind, token: operator.token() };
            let expr = match fixity {
                IncrementFixity::Prefix => scope::Expression::CompoundAssign {
                    target: operand,
                    target_temporary: *reference,
                    op,
                    value: sess.alloc(scope::Expression::Integer {
                        value: "1",
                        token: Token::synthesised(
                            TokenKind::Integer(panko_lex::Integer {
                                suffix: IntegerSuffix::None,
                                suffix_len: 0,
                                base: 10,
                                prefix_len: 0,
                            }),
                            operator.token().loc(),
                        ),
                    }),
                },
                IncrementFixity::Postfix { pointer, copy } =>
                    desugar_postfix_increment(sess, op, operand, reference, pointer, copy),
            };
            typeck_expression(sess, &expr, Context::Default)
        }
    };

    match context {
        Context::ArrayInitialisationByString if let Expression::String(_) = expr.expr => expr,
        Context::Default | Context::ArrayInitialisationByString => match expr.ty.ty {
            ty @ Type::Function(_) => TypedExpression {
                ty: Type::Pointer(sess.alloc(ty.unqualified())).unqualified(),
                expr: Expression::Addressof {
                    ampersand: Token::synthesised(panko_lex::TokenKind::And, expr.loc()),
                    operand: sess.alloc(expr),
                },
            },
            ty @ Type::Array(array_type) => TypedExpression {
                ty: Type::Pointer(array_type.ty).unqualified(),
                expr: Expression::NoopTypeConversion(sess.alloc(TypedExpression {
                    ty: Type::Pointer(sess.alloc(ty.unqualified())).unqualified(),
                    expr: Expression::Addressof {
                        ampersand: Token::synthesised(TokenKind::And, expr.loc()),
                        operand: sess.alloc(expr),
                    },
                })),
            },
            _ => expr,
        },
        Context::Sizeof | Context::Addressof | Context::Typeof => expr,
    }
}

fn grow_to_fit(signedness: Signedness, kind: IntegralKind, base: u32, value: u64) -> Integral {
    const POSSIBLE_TYS: [Integral; 6] = [
        Integral {
            signedness: Signedness::Signed,
            kind: IntegralKind::Int,
        },
        Integral {
            signedness: Signedness::Unsigned,
            kind: IntegralKind::Int,
        },
        Integral {
            signedness: Signedness::Signed,
            kind: IntegralKind::Long,
        },
        Integral {
            signedness: Signedness::Unsigned,
            kind: IntegralKind::Long,
        },
        Integral {
            signedness: Signedness::Signed,
            kind: IntegralKind::LongLong,
        },
        Integral {
            signedness: Signedness::Unsigned,
            kind: IntegralKind::LongLong,
        },
    ];
    POSSIBLE_TYS
        .into_iter()
        .filter(|ty| base != 10 || ty.signedness == signedness)
        .filter(|ty| ty.kind >= kind)
        .find(|ty| ty.can_represent(value))
        .unwrap_or_else(|| todo!("emit error: integer constant cannot be represented"))
}

pub fn resolve_types<'a>(
    sess: &'a Session<'a>,
    translation_unit: scope::TranslationUnit<'a>,
) -> TranslationUnit<'a> {
    TranslationUnit {
        decls: sess.alloc_slice_fill_iter(translation_unit.decls.iter().map(|decl| match decl {
            scope::ExternalDeclaration::FunctionDefinition(def) =>
                ExternalDeclaration::FunctionDefinition(typeck_function_definition(sess, def)),
            scope::ExternalDeclaration::Declaration(decl) =>
                ExternalDeclaration::Declaration(typeck_declaration(sess, decl)),
        })),
    }
}
