use std::bstr::ByteStr;
use std::num::IntErrorKind;
use std::str::Chars;

use itertools::Either;
use itertools::Itertools as _;
use panko_lex::EncodingPrefix;
use panko_lex::Integer;
use panko_lex::IntegerSuffix;
use panko_lex::Loc;
use panko_lex::Token;
use panko_lex::TokenKind;
use panko_parser::IntegerLiteralDiagnostic;
use panko_parser::ast::Arithmetic;
use panko_parser::ast::Integral;
use panko_parser::ast::IntegralKind;
use panko_parser::ast::Session;
use panko_parser::ast::Signedness;

use super::ArrayLength;
use super::ArrayType;
use super::Diagnostic;
use super::Expression;
use super::Type;
use super::TypedExpression;
use crate::fake_trait_impls::HashEqIgnored;

#[derive(Debug, Clone, Copy)]
pub(crate) struct StringLiteral<'a> {
    value: &'a ByteStr,
    loc: Loc<'a>,
}

impl<'a> StringLiteral<'a> {
    pub(crate) fn value(&self) -> &'a ByteStr {
        self.value
    }

    pub(super) fn len(&self) -> u64 {
        u64::try_from(self.value.len()).unwrap()
    }

    pub(super) fn loc(&self) -> Loc<'a> {
        self.loc
    }
}

#[derive(Debug, Clone, Copy)]
enum Char {
    Codepoint(char),
    // TODO: this hardcodes the assumption that `wchar_t` will be at most 32 bit long
    EscapeSequence(u32),
}

impl Char {
    fn unwrap_codepoint(self) -> char {
        match self {
            Char::Codepoint(c) => c,
            Char::EscapeSequence(_) => panic!(),
        }
    }

    fn len_utf8(self) -> usize {
        match self {
            Char::Codepoint(c) => c.len_utf8(),
            Char::EscapeSequence(_) => 1,
        }
    }

    fn len_utf16(self) -> usize {
        match self {
            Char::Codepoint(c) => c.len_utf16(),
            Char::EscapeSequence(_) => 1,
        }
    }

    fn encode_utf8(self) -> impl Iterator<Item = u8> {
        match self {
            Char::Codepoint(c) => {
                let mut storage = [0; char::MAX_LEN_UTF8];
                let encoded = c.encode_utf8(&mut storage);
                let len = encoded.len();
                Either::Left(storage.into_iter().take(len))
            }
            Char::EscapeSequence(value) =>
                #[expect(
                    clippy::as_conversions,
                    reason = "\
                        this should truncate; the value is checked to be in range for \
                        non-prefixed string literals in `parse_escape_sequences`\
                    "
                )]
                Either::Right(std::iter::once(value as u8)),
        }
    }
}

impl From<Char> for u64 {
    fn from(value: Char) -> Self {
        match value {
            Char::Codepoint(c) => Self::from(c),
            Char::EscapeSequence(value) => Self::from(value),
        }
    }
}

gen fn parse_escape_sequences(mut chars: Chars<'_>, encoding_prefix: EncodingPrefix) -> Char {
    let char_from_escape_sequence = move |value| {
        let escape_sequence_ty = Type::escape_sequence_ty(encoding_prefix);
        let codepoint = if escape_sequence_ty.can_represent(value) {
            match encoding_prefix {
                EncodingPrefix::None => i32::from(
                    u8::try_from(value)
                        .unwrap_or_else(|_| unreachable!())
                        .cast_signed(),
                )
                .cast_unsigned(),
                EncodingPrefix::Utf8
                | EncodingPrefix::Utf16
                | EncodingPrefix::Utf32
                | EncodingPrefix::Wchar => value,
            }
        }
        else {
            todo!(
                "error message: escape sequence value {value} out of range for type `{escape_sequence_ty}` of {encoding_prefix:?}-prefixed literal",
            );
        };
        Char::EscapeSequence(codepoint)
    };

    while let Some(c) = chars.next() {
        match c {
            '\\' =>
                yield match chars.next() {
                    None => unreachable!(),
                    Some('\'') => Char::Codepoint('\''),
                    Some('"') => Char::Codepoint('"'),
                    Some('?') => Char::Codepoint('?'),
                    Some('\\') => Char::Codepoint('\\'),
                    Some('a') => Char::Codepoint('\x07'),
                    Some('b') => Char::Codepoint('\x08'),
                    Some('f') => Char::Codepoint('\x0c'),
                    Some('n') => Char::Codepoint('\n'),
                    Some('r') => Char::Codepoint('\r'),
                    Some('t') => Char::Codepoint('\t'),
                    Some('v') => Char::Codepoint('\x0b'),
                    Some(oct_digit @ '0'..='7') => {
                        let value = oct_digit.to_digit(8).unwrap();
                        match chars.next() {
                            None => char_from_escape_sequence(value),
                            Some(oct_digit @ '0'..='7') => {
                                let value = value * 8 + oct_digit.to_digit(8).unwrap();
                                match chars.next() {
                                    None => char_from_escape_sequence(value),
                                    Some(oct_digit @ '0'..='7') => {
                                        let value = value * 8 + oct_digit.to_digit(8).unwrap();
                                        char_from_escape_sequence(value)
                                    }
                                    Some(c) => {
                                        yield char_from_escape_sequence(value);
                                        Char::Codepoint(c)
                                    }
                                }
                            }
                            Some(c) => {
                                yield char_from_escape_sequence(value);
                                Char::Codepoint(c)
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
                            Ok(codepoint) => char_from_escape_sequence(codepoint),
                            Err(_) => unreachable!(),
                        }
                    }
                    Some(_) => todo!("error: invalid escape sequence"),
                },
            c => yield Char::Codepoint(c),
        }
    }
}

pub(super) fn parse_char_literal<'a>(
    sess: &'a Session<'a>,
    char: &Token<'a>,
) -> TypedExpression<'a> {
    let TokenKind::CharConstant(encoding_prefix) = char.kind
    else {
        unreachable!()
    };
    let ty = Type::char_constant_ty(encoding_prefix);
    // TODO: check that escape sequence values are in range
    let values = parse_escape_sequences(
        char.slice()[encoding_prefix.len() + 1..char.slice().len() - 1].chars(),
        encoding_prefix,
    )
    .collect_vec();

    let expr = if let EncodingPrefix::Utf8 | EncodingPrefix::Utf16 | EncodingPrefix::Utf32 =
        encoding_prefix
        && values.len() > 1
    {
        sess.emit(Diagnostic::UnicodeCharConstantWithMoreThanOneCharacter {
            at: *char,
            prefix: encoding_prefix,
            len: values.len(),
        })
    }
    else {
        match values.first() {
            Some(value) => match encoding_prefix {
                EncodingPrefix::Utf8 if value.len_utf8() > 1 =>
                    sess.emit(Diagnostic::UnicodeCharLiteralNotEncodableInSingleCodeUnit {
                        at: *char,
                        char: value.unwrap_codepoint(),
                        len: value.len_utf8(),
                        prefix: encoding_prefix,
                    }),
                EncodingPrefix::Utf16 if value.len_utf16() > 1 =>
                    sess.emit(Diagnostic::UnicodeCharLiteralNotEncodableInSingleCodeUnit {
                        at: *char,
                        char: value.unwrap_codepoint(),
                        len: value.len_utf16(),
                        prefix: encoding_prefix,
                    }),
                // TODO: handle multichar character constants
                _ => Expression::Integer { value: u64::from(*value), token: *char },
            },
            None => sess.emit(Diagnostic::EmptyCharConstant { at: *char }),
        }
    };

    TypedExpression { ty: ty.unqualified(), expr }
}

fn parse_string_literal<'a>(sess: &'a Session<'a>, tokens: &[Token<'a>]) -> StringLiteral<'a> {
    let value: Vec<u8> = tokens
        .iter()
        .flat_map(|token| {
            parse_escape_sequences(
                token.slice()[1..token.slice().len() - 1].chars(),
                EncodingPrefix::None,
            )
        })
        .chain(std::iter::once(Char::Codepoint('\0')))
        .flat_map(|c| c.encode_utf8())
        .collect();
    StringLiteral {
        value: ByteStr::new(sess.alloc_slice_copy(&value)),
        loc: tokens
            .first()
            .unwrap()
            .loc()
            .until(tokens.last().unwrap().loc()),
    }
}

pub(super) fn typeck_string_literal<'a>(
    sess: &'a Session<'a>,
    tokens: &[Token<'a>],
) -> TypedExpression<'a> {
    let string = parse_string_literal(sess, tokens);
    TypedExpression {
        ty: Type::Array(ArrayType {
            ty: sess.alloc(Type::char().unqualified()),
            length: ArrayLength::Constant(string.len()),
            loc: HashEqIgnored(string.loc()),
        })
        .unqualified(),
        expr: Expression::String(string),
    }
}

pub(super) fn typeck_bool_literal<'a>(token: &Token<'a>) -> TypedExpression<'a> {
    let value = match token.kind {
        TokenKind::False => 0,
        TokenKind::True => 1,
        _ => unreachable!(),
    };
    TypedExpression {
        ty: Type::bool().unqualified(),
        expr: Expression::Integer { value, token: *token },
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

pub(super) fn typeck_integer_literal<'a>(
    sess: &'a Session<'a>,
    token: &Token<'a>,
    int: Integer,
    value: &str,
) -> TypedExpression<'a> {
    let Integer { suffix, suffix_len, base, prefix_len } = int;
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
            let () = sess.emit(IntegerLiteralDiagnostic::InvalidSuffix { at: *token });
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
                sess.emit(IntegerLiteralDiagnostic::TooLarge { at: *token }),
            _ => unreachable!(),
        },
    }
}
