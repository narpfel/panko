use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::Peekable;

use panko_lex::Error;
use panko_lex::Loc;
use panko_lex::Token;
use panko_lex::TokenKind;

use crate::ast::Session;
use crate::preprocess::diagnostics::Diagnostic;

mod diagnostics;

pub type TokenIter<'a> = impl Iterator<Item = Result<Token<'a>, Error<'a>>>;

type UnpreprocessedTokens<'a> = Peekable<panko_lex::TokenIter<'a>>;

fn is_identifier(token: &Token) -> bool {
    token.is_identifier() || token.is_keyword()
}

fn tokens_loc<'a>(tokens: &[Token<'a>]) -> Loc<'a> {
    match tokens {
        [] => unreachable!(),
        [token] => token.loc(),
        [first, .., last] => first.loc().until(last.loc()),
    }
}

#[derive(Debug, Clone, Copy)]
struct Macro<'a> {
    name: &'a str,
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
    hidden: HashSet<&'a str>,
    todo: Vec<(&'a str, &'a [Token<'a>])>,
}

impl<'a> Expander<'a> {
    fn is_empty(&self) -> bool {
        self.todo.is_empty()
    }

    fn push(&mut self, Macro { name, replacement }: Macro<'a>) {
        self.todo.push((name, replacement));
        assert!(self.hidden.insert(name));
    }

    fn next(&mut self, macros: &HashMap<&'a str, Macro<'a>>) -> Option<Token<'a>> {
        loop {
            let token = loop {
                let (name, tokens) = self.todo.last_mut()?;
                match tokens.split_off_first() {
                    Some(token) => break token,
                    None => {
                        self.hidden.remove(name);
                        self.todo.pop();
                    }
                }
            };

            if let Some(&r#macro) = macros.get(token.slice())
                && !self.hidden.contains(r#macro.name)
            {
                self.push(r#macro);
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
                            let line = self.eat_until_newline();
                            match &line[..] {
                                [] => self
                                    .sess
                                    .emit(Diagnostic::HashOutsideDirective { at: token }),
                                [directive, line @ ..] if is_identifier(directive) =>
                                    self.sess.emit(
                                        Diagnostic::NamedDirectiveDoesNotStartAtBeginningOfLine {
                                            at: token,
                                            directive: *directive,
                                            line: tokens_loc(line),
                                        },
                                    ),
                                line => self.sess.emit(
                                    Diagnostic::DirectiveDoesNotStartAtBeginningOfLine {
                                        at: token,
                                        line: tokens_loc(line),
                                    },
                                ),
                            }
                        },
                    Ok(token) if let Some(&r#macro) = self.macros.get(token.slice()) => {
                        self.previous_was_newline = false;
                        assert!(self.expander.is_empty());
                        self.expander.push(r#macro);
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
            Some(Ok(token)) if token.slice() == "undef" => {
                self.parse_undef();
            }
            token => todo!("error: unimplemented preprocessor directive starting in {token:?}"),
        }
    }

    fn parse_define(&mut self) {
        // eat `define`
        self.tokens.next();
        let line = self.eat_until_newline();
        match &line[..] {
            [] => todo!("error: empty `#define` directive"),
            [name, replacement @ ..] if is_identifier(name) => {
                let replacement = self.sess.alloc_slice_copy(replacement);
                self.macros
                    .insert(name.slice(), Macro { name: name.slice(), replacement });
            }
            [name, rest @ ..] =>
                todo!("error message: trying to `#define` non-identifier {name:?} with {rest:#?}"),
        }
    }

    fn parse_undef(&mut self) {
        // eat `undef`
        self.tokens.next();
        let line = self.eat_until_newline();
        match &line[..] {
            [] => todo!("error message: empty `#undef` directive"),
            [name] if is_identifier(name) => {
                self.macros.remove(name.slice());
            }
            [name] => todo!("error message: trying to `#undef` non-identifier {name:?}"),
            [name, rest @ ..] =>
                todo!("error message: extraneous tokens in `#undef` of {name:?}: {rest:#?}"),
        }
    }

    fn eat_until_newline(&mut self) -> Vec<Token<'a>> {
        self.tokens
            .by_ref()
            .take_while(|token| !matches!(token, Ok(Token { kind: TokenKind::Newline, .. })))
            .map(|token| {
                token.unwrap_or_else(|err| {
                    todo!("what happens when there is a lexer error here? {err:?}")
                })
            })
            .collect()
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
