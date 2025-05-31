use panko_lex::Error;
use panko_lex::Token;
use panko_lex::TokenKind;

use crate::ast::Session;

pub type TokenIter<'a> = impl Iterator<Item = Result<Token<'a>, Error<'a>>>;

#[define_opaque(TokenIter)]
pub fn preprocess<'a>(_sess: &'a Session<'a>, tokens: panko_lex::TokenIter<'a>) -> TokenIter<'a> {
    tokens.filter(|token| !token.is_ok_and(|token| token.kind == TokenKind::Newline))
}
