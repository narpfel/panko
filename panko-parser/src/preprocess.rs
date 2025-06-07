use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::Peekable;

use indexmap::IndexSet;
use itertools::Itertools as _;
use panko_lex::Loc;
use panko_lex::Token;
use panko_lex::TokenKind;

use crate::ast::Session;
use crate::preprocess::diagnostics::Diagnostic;

mod diagnostics;

pub type TokenIter<'a> = impl Iterator<Item = Token<'a>>;

type UnpreprocessedTokens<'a> = Peekable<panko_lex::TokenIter<'a>>;

fn is_identifier(token: &Token) -> bool {
    token.is_identifier() || token.is_keyword()
}

fn is_lparen(previous: &Token, token: &Token) -> bool {
    token.kind == TokenKind::LParen && previous.loc().end() == token.loc().start()
}

fn tokens_loc<'a>(tokens: &[Token<'a>]) -> Loc<'a> {
    match tokens {
        [] => unreachable!(),
        [token] => token.loc(),
        [first, .., last] => first.loc().until(last.loc()),
    }
}

#[derive(Debug, Clone, Copy)]
enum Replacement<'a> {
    Literal(#[expect(dead_code)] Token<'a>),
    Parameter(#[expect(dead_code)] usize),
}

#[derive(Debug, Clone, Copy)]
enum Macro<'a> {
    Object {
        name: &'a str,
        replacement: &'a [Token<'a>],
    },
    Function {
        #[expect(dead_code)]
        name: &'a str,
        #[expect(dead_code)]
        parameter_count: usize,
        #[expect(dead_code)]
        is_varargs: bool,
        #[expect(dead_code)]
        replacement: &'a [Replacement<'a>],
    },
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

    fn push(&mut self, r#macro: Macro<'a>) {
        match r#macro {
            Macro::Object { name, replacement } => {
                self.todo.push((name, replacement));
                assert!(self.hidden.insert(name));
            }
            Macro::Function { .. } => todo!("expansion of function-like macros"),
        }
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

            let name = token.slice();
            if let Some(&r#macro) = macros.get(name)
                && !self.hidden.contains(name)
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
                    token if token.kind == TokenKind::Newline => {
                        self.previous_was_newline = true;
                        yield token;
                    }
                    token if token.kind == TokenKind::Hash =>
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
                    token if let Some(&r#macro) = self.macros.get(token.slice()) => {
                        self.previous_was_newline = false;
                        assert!(self.expander.is_empty());
                        self.expander.push(r#macro);
                        while let Some(token) = self.expander.next(&self.macros) {
                            yield token;
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

    fn peek(&mut self) -> Option<&Token<'a>> {
        self.tokens.peek()
    }

    fn parse_directive(&mut self) {
        match self.peek() {
            None => {
                // null directive at end of file without newline (technically UB)
            }
            Some(token) if token.kind == TokenKind::Newline => {
                // null directive
            }
            Some(token) if token.slice() == "define" => {
                self.parse_define();
            }
            Some(token) if token.slice() == "undef" => {
                self.parse_undef();
            }
            Some(token) =>
                todo!("error: unimplemented preprocessor directive starting in {token:?}"),
        }
    }

    fn parse_define(&mut self) {
        // eat `define`
        self.tokens.next();
        let line = self.eat_until_newline();
        match &line[..] {
            [] => todo!("error: empty `#define` directive"),
            [name_tok, line @ ..] if is_identifier(name_tok) => {
                let line = self.sess.alloc_slice_copy(line);
                let name = name_tok.slice();
                let r#macro = if line.first().is_some_and(|token| is_lparen(name_tok, token)) {
                    parse_function_like_define(self.sess, name, line)
                }
                else {
                    Macro::Object { name, replacement: line }
                };
                // TODO: check for redefinition
                self.macros.insert(name, r#macro);
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
        self.previous_was_newline = true;
        self.tokens
            .by_ref()
            .peeking_take_while(|token| token.kind != TokenKind::Newline)
            .collect()
    }
}

// TODO: proper error type to differentiate between eof and unexpected token
fn eat<'a>(tokens: &mut &'a [Token<'a>], kind: TokenKind) -> Result<&'a Token<'a>, ()> {
    let token = tokens.first().ok_or(())?;
    if token.kind == kind {
        tokens.split_off_first();
        Ok(token)
    }
    else {
        Err(())
    }
}

fn parse_function_like_replacement<'a>(
    parameters: &IndexSet<&str>,
    _is_varargs: bool,
    tokens: &'a [Token<'a>],
) -> Vec<Replacement<'a>> {
    tokens
        .iter()
        .map(|token| {
            if let Some(param_index) = parameters.get_index_of(token.slice()) {
                Replacement::Parameter(param_index)
            }
            else {
                Replacement::Literal(*token)
            }
        })
        .collect()
}

fn parse_function_like_define<'a>(
    sess: &'a Session<'a>,
    name: &'a str,
    mut tokens: &'a [Token<'a>],
) -> Macro<'a> {
    assert!(eat(&mut tokens, TokenKind::LParen).is_ok());

    let mut parameters = IndexSet::default();
    loop {
        match tokens.first() {
            Some(token) if matches!(token.kind, TokenKind::RParen | TokenKind::Ellipsis) => break,
            Some(token) => {
                tokens.split_off_first();
                if is_identifier(token) {
                    assert!(
                        parameters.insert(token.slice()),
                        "TODO: error: duplicate parameter name",
                    )
                }
                else {
                    todo!("error: non-identifier in function-like macro parameter list: {token:?}")
                }
            }
            None => todo!("error: unexpected end of function-like macro parameter list"),
        }

        if eat(&mut tokens, TokenKind::Comma).is_err() {
            break;
        }
    }

    let is_varargs = eat(&mut tokens, TokenKind::Ellipsis).is_ok();
    if let Err(()) = eat(&mut tokens, TokenKind::RParen) {
        todo!("error: expected rparen")
    }

    let replacement = parse_function_like_replacement(&parameters, is_varargs, tokens);

    Macro::Function {
        name,
        parameter_count: parameters.len(),
        is_varargs,
        replacement: sess.alloc_slice_copy(&replacement),
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

pub fn print_preprocessed_source(tokens: TokenIter) {
    tokens.fold(None::<Token>, |last, token| {
        if let Some(last) = last {
            if last.kind == TokenKind::Newline {
                let indent = token.loc().src()[last.loc().end()..]
                    .find(|c: char| !c.is_whitespace())
                    .unwrap_or(0);
                print!("{:indent$}", "");
            }
            else if last.loc().end() != token.loc().start() {
                print!(" ");
            }
        }
        print!("{}", token.slice());
        Some(token)
    });
}
