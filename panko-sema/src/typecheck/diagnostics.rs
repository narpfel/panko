use ariadne::Color::Blue;
use ariadne::Color::Green;
use ariadne::Color::Magenta;
use ariadne::Color::Red;
use ariadne::Fmt as _;
use panko_lex::EncodingPrefix;
use panko_lex::Loc;
use panko_lex::Token;
use panko_parser as cst;
use panko_parser::BinOp;
use panko_report::Report;
use panko_report::Sliced as _;

use super::ConversionKind;
use super::Expression;
use super::QualifiedType;
use super::Reference;
use super::Type;
use super::Typeck;
use super::TypedExpression;
use crate::ItertoolsExt as _;
use crate::scope;
use crate::scope::Designator;
use crate::ty::ArrayType;
use crate::ty::ParameterDeclaration;
use crate::ty::Struct;
use crate::ty::subobjects::SubobjectIterator;

#[derive(Debug, Report)]
#[exit_code(1)]
pub(super) enum Diagnostic<'a> {
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
        at: ParameterDeclaration<'a, Typeck>,
    },

    #[error("array with incomplete element type `{at}`")]
    #[diagnostics(at(colour = Red, label = "array element types must be complete"))]
    ArrayWithIncompleteType { at: QualifiedType<'a> },

    #[error("arrays of functions are not allowed")]
    #[diagnostics(at(colour = Red, label = "element type is `{at}`"))]
    ArrayOfFunctions { at: QualifiedType<'a> },

    #[error("invalid function return type `{at}`")]
    #[diagnostics(at(colour = Red, label = "declaration here"))]
    InvalidFunctionReturnType { at: QualifiedType<'a> },

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

    #[error("invalid non-scalar cast from `{from_ty}` to `{target_ty}`")]
    #[diagnostics(
        at(colour = Blue, label = "this is of type `{from_ty}`"),
        target_ty(colour = Red, label = "type `{target_ty}` is not a scalar or `void`"),
    )]
    #[with(from_ty = from_ty.fg(Blue))]
    NonScalarCast {
        at: TypedExpression<'a>,
        from_ty: Type<'a>,
        target_ty: QualifiedType<'a>,
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

    #[error("dereference of pointer to incomplete type `{pointee_ty}`")]
    #[diagnostics(at(colour = Red, label = "this expression has type `{ty}`"))]
    #[with(ty = at.ty.ty, pointee_ty = pointee_ty.fg(Red))]
    DerefOfPointerToIncompleteType {
        at: TypedExpression<'a>,
        pointee_ty: QualifiedType<'a>,
    },

    #[error("invalid application of `{op}` to {kind} `{ty}`")]
    #[diagnostics(
        op(colour = Red),
        at(colour = Red, label = "in this expression"),
    )]
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

    #[error("cannot dereference this expression of type `{ty}` (pointer or array type required)")]
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

    #[error(
        "invalid pointer arithmetic `{op_token}` on a pointer to incomplete type `{pointee_ty}`"
    )]
    #[diagnostics(
        lhs(colour = Blue, label = "this is of type `{lhs_ty}`"),
        at(colour = Red),
        rhs(colour = Magenta, label = "this is of type `{rhs_ty}`"),
    )]
    #[with(op_token = at.token, lhs_ty = lhs.ty, rhs_ty = rhs.ty, pointee_ty = pointee_ty.fg(Red))]
    PointerArithmeticWithIncompletePointee {
        at: BinOp<'a>,
        pointee_ty: QualifiedType<'a>,
        lhs: TypedExpression<'a>,
        rhs: TypedExpression<'a>,
    },

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
        at(colour = Red, label = "this callee expects {at_least}{expected} argument{maybe_plural_s}"),
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

    #[error(
        "value of type `{ty}` is not callable because it is not a function or function pointer"
    )]
    #[diagnostics(at(colour = Red, label = "this is not callable because it is of type `{ty}`"))]
    #[with(ty = at.ty)]
    Uncallable { at: TypedExpression<'a> },

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
                let index = index.strict_sub(1).fg(Red);
                format!("trying to initialise element at index {index} for `{ty}`")
            }
            SubobjectIterator::Struct { ty, index, offset: _ } => {
                let ty = Type::Struct(Struct::Complete(*ty)).fg(Blue);
                // trying to get the non-existing element increments the index, so we undo this
                // here
                let index = index.strict_sub(1).fg(Red);
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
            SubobjectIterator::Scalar { ty, .. } => *ty,
            SubobjectIterator::Array { .. } => unreachable!(),
            SubobjectIterator::Struct { ty, .. } => Type::Struct(Struct::Complete(*ty)),
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

    #[error("{kind} character constant may not contain multiple characters")]
    #[diagnostics(at(colour = Red, label = "this character constant contains {len} characters"))]
    #[with(
        kind = prefix.encoding(),
        len = len.fg(Red),
    )]
    UnicodeCharConstantWithMoreThanOneCharacter {
        at: Token<'a>,
        prefix: EncodingPrefix,
        len: usize,
    },

    #[error(
        "{kind} character constant contains character that is not encodable in a single {kind} code unit"
    )]
    #[diagnostics(
        at(
            colour = Red,
            label = "`{char}` (U+{codepoint:X}) is encoded as {len} code units in {kind}",
        ),
    )]
    #[with(
        kind = prefix.encoding(),
        codepoint = u32::from(*char),
        char = char.fg(Red),
        len = len.fg(Red),
    )]
    UnicodeCharLiteralNotEncodableInSingleCodeUnit {
        at: Token<'a>,
        char: char,
        len: usize,
        prefix: EncodingPrefix,
    },

    #[error("array of inappropriate type `{actual_ty}` initialised by string literal")]
    #[diagnostics(
        actual_ty(colour = Red, label = "this is an array of `{element_ty}`"),
        at(
            colour = Red,
            label = "this is a {kind} string literal, which can only initialise arrays of {expected_tys}",
        ),
    )]
    #[with(
        expected_tys = expected_tys.iter().map(|ty| format!("`{}`", ty.fg(Blue))).join_end(", ", " or "),
        element_ty = actual_ty.ty.ty,
    )]
    ArrayInitialisationByStringLiteralTypeMismatch {
        at: scope::Expression<'a>,
        kind: &'a str,
        expected_tys: &'a [Type<'a>],
        actual_ty: ArrayType<'a, Typeck>,
    },

    #[error("cannot initialise variable of {kind} type `{ty}`")]
    #[diagnostics(
        ty(colour = Blue, label = "`{ty}` is {article}{kind}{specifier}"),
        at(colour = Red, label = "cannot initialise variable of {kind} type"),
        reference(colour = Magenta, label = "while defining this variable"),
    )]
    #[with(
        ty = reference.ty,
        (article, kind, specifier) = match ty.ty {
            ty if !ty.is_complete() => ("", "incomplete", ""),
            Type::Function(_) => ("a ", "function", " type"),
            _ => unreachable!(),
        },
    )]
    BracedInitOfInvalidType {
        at: scope::Initialiser<'a>,
        reference: Reference<'a>,
    },

    #[error("incompatible operand types in conditional expression{note}")]
    #[diagnostics(
        at(colour = Blue),
        then(colour = Red, label = "this is of type `{then_ty}`"),
        or_else(colour = Magenta, label = "this is of type `{or_else_ty}`"),
    )]
    #[with(
        then_ty = then.ty.ty,
        or_else_ty = or_else.ty.ty,
        note = match note {
            Some(note) => format!(" {note}"),
            None => String::from(""),
        },
    )]
    ConditionalExprOperandTypesIncompatible {
        at: Token<'a>,
        note: Option<&'a str>,
        then: TypedExpression<'a>,
        or_else: TypedExpression<'a>,
    },

    #[error("redeclaration of type alias `{name}` with different type")]
    #[with(
        ty = at.ty,
        name = at.name,
    )]
    #[diagnostics(
        previously_declared_as(
            colour = Blue,
            label = "previously declared as `{previously_declared_as}` here",
        ),
        at(colour = Red, label = "redeclared as different type `{ty}` here"),
    )]
    TypedefRedeclared {
        at: scope::Typedef<'a>,
        previously_declared_as: scope::QualifiedType<'a>,
    },

    #[error("function declared at block scope cannot have a storage class other than `extern`")]
    #[diagnostics(at(colour = Red))]
    BlockScopeFunctionWithInvalidStorageClass { at: Loc<'a> },

    #[error("`{name}` redeclared as {new_linkage} but was originally declared as {old_linkage}")]
    #[diagnostics(
        previous_definition(colour = Blue, label = "previously declared here as {old_linkage}"),
        at(colour = Red, label = "{new_linkage} declaration here"),
    )]
    #[with(
        name = previous_definition.name,
        old_linkage = previous_definition.linkage_staticness_as_str(),
        new_linkage = at.linkage_staticness_as_str(),
    )]
    RedeclaredWithDifferentLinkage {
        at: Reference<'a>,
        previous_definition: Reference<'a>,
    },

    #[error("control flow returned from function `{function}` declared `{noreturn}`")]
    #[diagnostics(
        at(colour = Red, label = "this `{at}` statement was executed"),
        noreturn(colour = Magenta, label = "declared as `{noreturn}` here"),
        function(colour = Blue, label = "in function `{function}`")
    )]
    ReturnFromNoreturnFunction {
        at: Token<'a>,
        noreturn: cst::FunctionSpecifier<'a>,
        function: scope::Reference<'a>,
    },
}
