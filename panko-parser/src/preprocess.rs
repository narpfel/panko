use std::collections::HashMap;
use std::iter::Peekable;

use itertools::Itertools as _;
use panko_lex::Error;
use panko_lex::Token;
use panko_lex::TokenKind;

use crate::ast::Session;

pub type TokenIter<'a> = impl Iterator<Item = Result<Token<'a>, Error<'a>>>;

type UnpreprocessedTokens<'a> = Peekable<panko_lex::TokenIter<'a>>;

fn is_identifier(token: &Token) -> bool {
    // TODO: it’s UB to `#define` a keyword, so we can just treat that as a compile time error
    token.kind == TokenKind::Identifier
}

#[derive(Debug)]
struct Macro<'a> {
    replacement: &'a [Token<'a>],
}

struct Preprocessor<'a> {
    sess: &'a Session<'a>,
    tokens: UnpreprocessedTokens<'a>,
    previous_was_newline: bool,
    macros: HashMap<&'a str, Macro<'a>>,
    expander: Expander<'a>,
}

#[derive(Debug, Default)]
struct Expander<'a> {
    todo: Vec<&'a [Token<'a>]>,
}

impl<'a> Expander<'a> {
    fn is_empty(&self) -> bool {
        self.todo.is_empty()
    }

    fn push(&mut self, tokens: &'a [Token<'a>]) {
        self.todo.push(tokens);
    }

    fn next(&mut self, macros: &HashMap<&'a str, Macro<'a>>) -> Option<Token<'a>> {
        loop {
            let tokens = loop {
                let tokens = self.todo.pop()?;
                if !tokens.is_empty() {
                    break tokens;
                }
            };

            let (token, tokens) = tokens
                .split_first()
                .expect("the loop above only ends on nonempty slices");
            self.todo.push(tokens);

            if let Some(r#macro) = macros.get(token.slice()) {
                self.todo.push(r#macro.replacement);
            }
            else {
                return Some(*token);
            }
        }
    }
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
                    Ok(token) if let Some(r#macro) = self.macros.get(token.slice()) => {
                        self.previous_was_newline = false;
                        assert!(self.expander.is_empty());
                        self.expander.push(r#macro.replacement);
                        while let Some(token) = self.expander.next(&self.macros) {
                            yield Ok(token);
                        }
                    }
                    token => {
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
            Some(Ok(token)) if token.slice() == "define" => {
                self.parse_define();
            }
            token => todo!("error: unimplemented preprocessor directive starting in {token:?}"),
        }
    }

    fn parse_define(&mut self) {
        // eat `define`
        self.tokens.next();
        match self.tokens.next() {
            Some(Ok(name)) if is_identifier(&name) => {
                let replacement = self
                    .tokens
                    .by_ref()
                    .take_while(|token| {
                        !matches!(token, Ok(Token { kind: TokenKind::Newline, .. }))
                    })
                    .map(|token| {
                        token.unwrap_or_else(|err| {
                            todo!("what happens when there is a lexer error here? {err:?}")
                        })
                    })
                    .collect_vec();
                let replacement = self.sess.alloc_slice_copy(&replacement);
                self.macros.insert(name.slice(), Macro { replacement });
            }
            Some(Ok(name)) => todo!("error message: trying to `#define` non-identifier {name:?}"),
            _ => todo!("error message"),
        }
    }
}

pub fn preprocess<'a>(sess: &'a Session<'a>, tokens: panko_lex::TokenIter<'a>) -> TokenIter<'a> {
    Preprocessor {
        sess,
        tokens: tokens.peekable(),
        previous_was_newline: true,
        macros: HashMap::default(),
        expander: Expander::default(),
    }
    .run()
}
