use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::Peekable;
use std::iter::from_fn;

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
    // TODO: this should check that both source files are the same (so that two tokens `#include`d
    // from different files such that their locations match are not recognised as `lparen`).
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
    Literal(Token<'a>),
    Parameter(usize),
}

#[derive(Debug, Clone, Copy)]
enum Macro<'a> {
    Object {
        name: &'a str,
        replacement: &'a [Token<'a>],
    },
    Function {
        name: &'a str,
        parameter_count: usize,
        is_varargs: bool,
        replacement: &'a [Replacement<'a>],
    },
}

impl<'a> Macro<'a> {
    fn expand(
        &self,
        sess: &'a Session<'a>,
        tokens: &mut Peekable<impl Iterator<Item = Token<'a>>>,
        macro_token: &Token<'a>,
    ) -> Option<Expanding<'a>> {
        match self {
            Self::Object { name, replacement } => Some(Expanding::Object { name, replacement }),
            &Self::Function {
                name,
                parameter_count,
                is_varargs,
                replacement,
            } => match tokens.next_if(|token| token.kind == TokenKind::LParen) {
                Some(_) => {
                    let arguments = parse_macro_arguments(sess, tokens);
                    if arguments.len() != parameter_count {
                        sess.emit(Diagnostic::ArityMismatch {
                            at: *macro_token,
                            expected: parameter_count,
                            actual: arguments.len(),
                            is_varargs,
                        })
                    }
                    Some(Expanding::Function { name, arguments, replacement })
                }
                None => None,
            },
        }
    }
}

struct Preprocessor<'a> {
    sess: &'a Session<'a>,
    tokens: UnpreprocessedTokens<'a>,
    previous_was_newline: bool,
    macros: HashMap<&'a str, Macro<'a>>,
    expander: Expander<'a>,
}

#[derive(Debug)]
enum Expanding<'a> {
    Object {
        name: &'a str,
        replacement: &'a [Token<'a>],
    },
    Function {
        name: &'a str,
        arguments: Vec<&'a [Token<'a>]>,
        replacement: &'a [Replacement<'a>],
    },
    Argument {
        function_name: &'a str,
        tokens: &'a [Token<'a>],
    },
    Token(Option<Token<'a>>),
}

impl<'a> Expanding<'a> {
    fn name(&self) -> Option<&'a str> {
        match self {
            Expanding::Object { name, .. } | Expanding::Function { name, .. } => Some(name),
            Expanding::Argument { function_name: _, tokens: _ } => None,
            Expanding::Token(_) => None,
        }
    }

    fn next(&mut self) -> Expanded<'a> {
        match self {
            Self::Object { name: _, replacement } => replacement.split_off_first().into(),
            Self::Function { name, arguments, replacement } =>
                match replacement.split_off_first() {
                    Some(Replacement::Literal(token)) => Expanded::Token(*token),
                    Some(Replacement::Parameter(index)) => Expanded::Argument {
                        function_name: name,
                        tokens: arguments.get(*index).copied().unwrap_or_default(),
                    },
                    None => Expanded::Done,
                },
            Self::Argument { function_name: _, tokens } => match tokens.split_off_first() {
                Some(&token) => Expanded::Token(token),
                None => Expanded::Done,
            },
            Self::Token(token) => token.take().map_or(Expanded::Done, Expanded::Token),
        }
    }
}

enum Expanded<'a> {
    Token(Token<'a>),
    Argument {
        function_name: &'a str,
        tokens: &'a [Token<'a>],
    },
    Done,
}

impl<'a> From<Option<&Token<'a>>> for Expanded<'a> {
    fn from(value: Option<&Token<'a>>) -> Self {
        value.copied().map_or(Expanded::Done, Expanded::Token)
    }
}

#[derive(Debug)]
struct Expander<'a> {
    sess: &'a Session<'a>,
    hidden: HashSet<&'a str>,
    todo: Vec<Expanding<'a>>,
}

impl<'a> Expander<'a> {
    fn is_empty(&self) -> bool {
        self.todo.is_empty()
    }

    fn push(&mut self, expanding: Expanding<'a>) {
        match expanding {
            Expanding::Argument { function_name, tokens: _ } =>
                assert!(self.hidden.remove(function_name)),
            _ if let Some(name) = expanding.name() => assert!(self.hidden.insert(name)),
            _ => (),
        }
        self.todo.push(expanding);
    }

    fn next(&mut self, macros: &HashMap<&'a str, Macro<'a>>) -> Option<Token<'a>> {
        loop {
            let token = loop {
                let expanding = self.todo.last_mut()?;
                match expanding.next() {
                    Expanded::Token(token) => break token,
                    Expanded::Argument { function_name, tokens } =>
                        self.push(Expanding::Argument { function_name, tokens }),
                    Expanded::Done => {
                        match expanding {
                            Expanding::Argument { function_name, tokens: _ } =>
                                assert!(self.hidden.insert(function_name)),
                            _ if let Some(name) = expanding.name() =>
                                assert!(self.hidden.remove(name)),
                            _ => (),
                        }
                        self.todo.pop();
                    }
                }
            };

            let name = token.slice();
            match macros.get(name) {
                Some(r#macro) if !self.hidden.contains(name) => {
                    let sess = self.sess;
                    let tokens = &mut from_fn(|| self.next(macros)).peekable();
                    match r#macro.expand(sess, tokens, &token) {
                        Some(expanding) => self.push(expanding),
                        None => {
                            let maybe_peeked = tokens.next();
                            self.push(Expanding::Token(maybe_peeked));
                            return Some(token);
                        }
                    }
                }
                _ => return Some(token),
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
                        match r#macro.expand(self.sess, self.tokens.by_ref(), &token) {
                            Some(expanding) => {
                                self.expander.push(expanding);
                                while let Some(token) = self.expander.next(&self.macros) {
                                    yield token;
                                }
                            }
                            None => yield token,
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
        .map(|token| match parameters.get_index_of(token.slice()) {
            Some(param_index) => Replacement::Parameter(param_index),
            None => Replacement::Literal(*token),
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

fn eat_until_in_balanced_parens<'a>(
    tokens: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    predicate: impl Fn(&Token<'a>) -> bool,
) -> Vec<Token<'a>> {
    let mut nesting_level = 0_usize;
    tokens
        .peeking_take_while(|token| match token.kind {
            TokenKind::LParen => {
                nesting_level += 1;
                true
            }
            TokenKind::RParen => match nesting_level.checked_sub(1) {
                Some(value) => {
                    nesting_level = value;
                    true
                }
                None => false,
            },
            _ => nesting_level != 0 || !predicate(token),
        })
        .collect()
}

fn parse_macro_arguments<'a>(
    sess: &'a Session<'a>,
    tokens: &mut Peekable<impl Iterator<Item = Token<'a>>>,
) -> Vec<&'a [Token<'a>]> {
    let token = tokens.peek();
    if token.is_none_or(|token| token.kind == TokenKind::RParen) {
        tokens.next();
        return Vec::new();
    }

    let mut arguments = Vec::new();

    loop {
        arguments.push(
            sess.alloc_slice_copy(&eat_until_in_balanced_parens(tokens, |token| {
                matches!(token.kind, TokenKind::RParen | TokenKind::Comma)
            })),
        );

        match tokens.peek() {
            Some(token) if token.kind == TokenKind::RParen => break,
            Some(token) if token.kind == TokenKind::Comma => tokens.next(),
            Some(_) => unreachable!(),
            None => break,
        };
    }

    match tokens.next() {
        Some(token) if token.kind == TokenKind::RParen => (),
        Some(_) => unreachable!(),
        None => todo!("error: missing rparen in function-like macro invocation"),
    }

    arguments
}

pub fn preprocess<'a>(sess: &'a Session<'a>, tokens: panko_lex::TokenIter<'a>) -> TokenIter<'a> {
    Preprocessor {
        sess,
        tokens: tokens.peekable(),
        previous_was_newline: true,
        macros: HashMap::default(),
        expander: Expander {
            sess,
            hidden: HashSet::default(),
            todo: Vec::default(),
        },
    }
    .run()
}

pub fn print_preprocessed_source(tokens: TokenIter) {
    tokens.fold(None::<Token>, |last, token| {
        if let Some(last) = last
            && token.kind != TokenKind::Newline
        {
            if last.kind == TokenKind::Newline {
                let indent = token.loc().src()[last.loc().end()..]
                    .find(|c: char| !c.is_whitespace())
                    .unwrap_or(0);
                print!("{:indent$}", "");
            }
            // TODO: this should check that both source files are the same (so that two tokens
            // `#include`d from different files such that their locations match are not recognised
            // as adjacent).
            // TODO: this does not take into account that `last` could have come from a macro
            // expansion. The preprocessor should track the original location of each token for
            // this and for error messages.
            else if last.loc().end() != token.loc().start() {
                print!(" ");
            }
        }
        print!("{}", token.slice());
        Some(token)
    });
}
