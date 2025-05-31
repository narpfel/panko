use std::iter::Peekable;

use panko_lex::Error;
use panko_lex::Token;
use panko_lex::TokenKind;

use crate::ast::Session;

pub type TokenIter<'a> = impl Iterator<Item = Result<Token<'a>, Error<'a>>>;

type UnpreprocessedTokens<'a> = Peekable<panko_lex::TokenIter<'a>>;

struct Preprocessor<'a> {
    #[expect(unused)]
    sess: &'a Session<'a>,
    tokens: UnpreprocessedTokens<'a>,
    previous_was_newline: bool,
}

impl<'a> Preprocessor<'a> {
    #[define_opaque(TokenIter)]
    fn run(mut self) -> TokenIter<'a> {
        gen move {
            while let Some(token) = self.tokens.next() {
                match token {
                    Ok(token) if token.kind == TokenKind::Newline =>
                        self.previous_was_newline = true,
                    Ok(token) if token.kind == TokenKind::Hash =>
                        if self.previous_was_newline {
                            self.parse_directive();
                        }
                        else {
                            todo!(
                                "error: preprocessor directive does not start at beginning of line",
                            )
                        },
                    _ => {
                        self.previous_was_newline = false;
                        yield token;
                    }
                }
            }
        }
    }

    fn peek(&mut self) -> Option<Result<&Token<'a>, &Error<'a>>> {
        try { self.tokens.peek()?.as_ref() }
    }

    fn parse_directive(&mut self) {
        match self.peek() {
            Some(Ok(token)) if token.kind == TokenKind::Newline => {
                // eat newline token => null directive
                self.tokens.next();
            }
            token => todo!("error: unimplemented preprocessor directive starting in {token:?}"),
        }
    }
}

pub fn preprocess<'a>(sess: &'a Session<'a>, tokens: panko_lex::TokenIter<'a>) -> TokenIter<'a> {
    Preprocessor {
        sess,
        tokens: tokens.peekable(),
        previous_was_newline: true,
    }
    .run()
}
