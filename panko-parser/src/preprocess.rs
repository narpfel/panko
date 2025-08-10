use std::borrow::Cow;
use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::ffi::OsStr;
use std::fmt::Display;
use std::io::stdout;
use std::iter::Peekable;
use std::iter::from_fn;
use std::ops::Not as _;
use std::path::Path;

use indexmap::IndexSet;
use itertools::Itertools as _;
use itertools::PeekingNext;
use panko_lex::Bump;
use panko_lex::Integer;
use panko_lex::IntegerSuffix;
use panko_lex::Loc;
use panko_lex::Token;
use panko_lex::TokenKind;

use crate::ast::Session;
use crate::handle_parse_error;
use crate::nonempty;
use crate::preprocess::diagnostics::Diagnostic;
use crate::preprocess::diagnostics::MaybeError;
use crate::preprocess::eval::eval;
pub use crate::preprocess::include_paths::IncludePaths;

mod diagnostics;
mod eval;
mod include_paths;

const IF_DIRECTIVE_INTRODUCERS: [&str; 3] = ["if", "ifdef", "ifndef"];

pub type TokenIter<'a> = impl Iterator<Item = Token<'a>>;

type UnclosedIfs<'a> = Vec<Loc<'a>>;

struct UnpreprocessedTokens<'a> {
    previous_was_newline: bool,
    tokens: Peekable<panko_lex::TokenIter<'a>>,
    unclosed_ifs: UnclosedIfs<'a>,
}

impl<'a> UnpreprocessedTokens<'a> {
    fn new(tokens: panko_lex::TokenIter<'a>) -> Self {
        Self {
            previous_was_newline: true,
            tokens: tokens.peekable(),
            unclosed_ifs: UnclosedIfs::default(),
        }
    }

    fn push_if(&mut self, loc: Loc<'a>) {
        self.unclosed_ifs.push(loc)
    }

    fn pop_if(&mut self) -> Option<Loc<'a>> {
        self.unclosed_ifs.pop()
    }

    fn next_full(&mut self) -> Option<(bool, Token<'a>)> {
        let previous_was_newline = self.previous_was_newline;
        let token = self.tokens.next()?;
        self.previous_was_newline = token.kind == TokenKind::Newline;
        Some((previous_was_newline, token))
    }

    fn until_newline(&mut self) -> impl Iterator<Item = <Self as Iterator>::Item> {
        self.peeking_take_while(|token| token.kind != TokenKind::Newline)
    }
}

impl<'a> Iterator for UnpreprocessedTokens<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        try { self.next_full()?.1 }
    }
}

impl<'a> PeekingNext for UnpreprocessedTokens<'a> {
    fn peeking_next<F>(&mut self, accept: F) -> Option<Self::Item>
    where
        Self: Sized,
        F: FnOnce(&Self::Item) -> bool,
    {
        match accept(self.tokens.peek()?) {
            true => self.next(),
            false => None,
        }
    }
}

trait PeekableIterator: PeekingNext {
    fn peek(&mut self) -> Option<&Self::Item>;
}

impl PeekableIterator for UnpreprocessedTokens<'_> {
    fn peek(&mut self) -> Option<&Self::Item> {
        self.tokens.peek()
    }
}

impl<T> PeekableIterator for Peekable<T>
where
    T: Iterator,
{
    fn peek(&mut self) -> Option<&Self::Item> {
        Peekable::peek(self)
    }
}

trait Eat<'a>: PeekableIterator<Item = Token<'a>> {
    fn eat(&mut self, kind: TokenKind) -> Result<Token<'a>, Token<'a>> {
        self.eat_if(|token| token.kind == kind)
    }

    fn eat_if(
        &mut self,
        predicate: impl FnOnce(&Token<'a>) -> bool,
    ) -> Result<Token<'a>, Token<'a>> {
        match self.peek().copied() {
            Some(token) if predicate(&token) => Ok(self.next().unwrap()),
            Some(token) => Err(token),
            None => todo!("error message: UB: source file does not end in trailing newline"),
        }
    }

    fn eat_until_newline(&mut self) -> Vec<Token<'a>>
    where
        Self: Sized,
    {
        self.peeking_take_while(|token| token.kind != TokenKind::Newline)
            .collect()
    }
}

impl<'a, T> Eat<'a> for T where T: PeekableIterator<Item = Token<'a>> {}

fn alloc_path(bump: &Bump, path: impl AsRef<Path>) -> &Path {
    let bytes = bump.alloc_slice_copy(path.as_ref().as_os_str().as_encoded_bytes());
    Path::new(unsafe { OsStr::from_encoded_bytes_unchecked(bytes) })
}

fn is_identifier(token: &Token) -> bool {
    token.is_identifier() || token.is_keyword()
}

fn is_lparen(previous: &Token, token: &Token) -> bool {
    // TODO: this should check that both source files are the same (so that two tokens `#include`d
    // from different files such that their locations match are not recognised as `lparen`).
    token.kind == TokenKind::LParen && previous.loc().end() == token.loc().start()
}

pub(crate) fn tokens_loc<'a>(tokens: &[Token<'a>]) -> Loc<'a> {
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
    VaOpt(&'a [Replacement<'a>]),
    VaArgs,
    Stringise(usize),
    Concat {
        lhs: &'a Replacement<'a>,
        hash_hash: Token<'a>,
        rhs: &'a Replacement<'a>,
    },
}

impl<'a> Replacement<'a> {
    fn expand(&self, call: FunctionCall<'a>) -> Expanded<'a> {
        match self {
            Self::Literal(token) => Expanded::Token(*token),
            Self::Parameter(index) => Expanded::Argument(Argument {
                call,
                replacement: call.get(*index),
                update_hideset: true,
            }),
            Self::VaOpt(replacement) => Expanded::VaOpt(VaOpt { call, replacement }),
            Self::VaArgs => Expanded::Argument(Argument {
                call,
                replacement: call.get_va_args(),
                update_hideset: false,
            }),
            Self::Stringise(index) =>
                Expanded::Stringise(Stringise { call, replacement: call.get(*index) }),
            Self::Concat { lhs, hash_hash, rhs } =>
                Expanded::Concat(Concat { call, lhs, hash_hash: *hash_hash, rhs }),
        }
    }
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
    Line,
    File,
}

impl<'a> Macro<'a> {
    fn expand(
        &self,
        sess: &'a Session<'a>,
        tokens: &mut impl PeekingNext<Item = Token<'a>>,
        macro_token: &Token<'a>,
    ) -> Option<Expanding<'a>> {
        match self {
            Self::Object { name, replacement } => Some(Expanding::Object { name, replacement }),
            &Self::Function {
                name,
                parameter_count,
                is_varargs,
                replacement,
            } => match tokens.peeking_next(|token| token.kind == TokenKind::LParen) {
                Some(_) => {
                    let arguments =
                        parse_macro_arguments(sess, macro_token, parameter_count, tokens);
                    let has_arity_mismatch = match () {
                        () if arguments.is_empty() && parameter_count == 1 => false,
                        () if is_varargs => parameter_count > arguments.len(),
                        () => parameter_count != arguments.len(),
                    };
                    if has_arity_mismatch {
                        sess.emit(Diagnostic::ArityMismatch {
                            at: *macro_token,
                            expected: parameter_count,
                            actual: arguments.len(),
                            is_varargs,
                        })
                    }
                    let call = FunctionCall { name, parameter_count, arguments };
                    Some(Expanding::Function { call, replacement })
                }
                None => None,
            },
            // TODO: use original location, not the location of the `__LINE__`/`__FILE__` token
            // (wrong when generated by a macro)
            Self::Line => Some(Expanding::Token(Some(Token::from_str(
                sess.bump,
                TokenKind::Integer(Integer {
                    suffix: IntegerSuffix::None,
                    suffix_len: 0,
                    base: 10,
                    prefix_len: 0,
                }),
                sess.alloc_str(&macro_token.loc().line().to_string()),
            )))),
            Self::File => Some(Expanding::Token(Some(Token::from_str(
                sess.bump,
                TokenKind::String,
                sess.alloc_str(&format!("\"{}\"", macro_token.loc().file().display())),
            )))),
        }
    }
}

struct Preprocessor<'a> {
    sess: &'a Session<'a>,
    tokens: nonempty::Vec<UnpreprocessedTokens<'a>>,
    macros: HashMap<&'a str, Macro<'a>>,
    expander: Expander<'a>,
    include_paths: IncludePaths,
}

#[derive(Debug, Clone, Copy)]
struct FunctionCall<'a> {
    name: &'a str,
    parameter_count: usize,
    arguments: &'a [&'a [Replacement<'a>]],
}

impl<'a> FunctionCall<'a> {
    fn get(&self, param_index: usize) -> &'a [Replacement<'a>] {
        self.arguments.get(param_index).copied().unwrap_or_default()
    }

    fn get_va_args(&self) -> &'a [Replacement<'a>] {
        self.get(self.parameter_count)
    }
}

#[derive(Debug, Clone, Copy)]
struct Argument<'a> {
    call: FunctionCall<'a>,
    replacement: &'a [Replacement<'a>],
    update_hideset: bool,
}

#[derive(Debug, Clone)]
enum Expanding<'a> {
    Object {
        name: &'a str,
        replacement: &'a [Token<'a>],
    },
    Function {
        call: FunctionCall<'a>,
        replacement: &'a [Replacement<'a>],
    },
    Argument(Argument<'a>),
    Token(Option<Token<'a>>),
    Tokens(VecDeque<Token<'a>>),
    Wrapped(Option<Expanded<'a>>),
}

impl<'a> Expanding<'a> {
    fn name(&self) -> Option<&'a str> {
        match self {
            Expanding::Object { name, .. } => Some(name),
            Expanding::Function { call, replacement: _ } => Some(call.name),
            Expanding::Argument(_) => None,
            Expanding::Token(_) => None,
            Expanding::Tokens(_) => None,
            Expanding::Wrapped(_) => None,
        }
    }

    fn next(&mut self) -> Expanded<'a> {
        match self {
            Self::Object { name: _, replacement } => replacement.split_off_first().into(),
            Self::Function { call, replacement } => match replacement.split_off_first() {
                Some(replacement) => replacement.expand(*call),
                None => Expanded::Done,
            },
            Self::Argument(argument) => match argument.replacement.split_off_first() {
                Some(replacement) => replacement.expand(argument.call),
                None => Expanded::Done,
            },
            Self::Token(token) => token.take().into(),
            Self::Tokens(tokens) => tokens.pop_front().into(),
            Self::Wrapped(expanded) => expanded.take().unwrap_or(Expanded::Done),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct VaOpt<'a> {
    call: FunctionCall<'a>,
    replacement: &'a [Replacement<'a>],
}

#[derive(Debug, Clone, Copy)]
struct Stringise<'a> {
    call: FunctionCall<'a>,
    replacement: &'a [Replacement<'a>],
}

#[derive(Debug, Clone, Copy)]
struct Concat<'a> {
    call: FunctionCall<'a>,
    lhs: &'a Replacement<'a>,
    hash_hash: Token<'a>,
    rhs: &'a Replacement<'a>,
}

#[derive(Debug, Clone, Copy)]
enum Expanded<'a> {
    Token(Token<'a>),
    Argument(Argument<'a>),
    VaOpt(VaOpt<'a>),
    Stringise(Stringise<'a>),
    Concat(Concat<'a>),
    Done,
}

impl<'a> From<Option<Token<'a>>> for Expanded<'a> {
    fn from(value: Option<Token<'a>>) -> Self {
        value.map_or(Expanded::Done, Expanded::Token)
    }
}

impl<'a> From<Option<&Token<'a>>> for Expanded<'a> {
    fn from(value: Option<&Token<'a>>) -> Self {
        value.copied().into()
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

    gen fn expand_macros(
        &mut self,
        macros: &HashMap<&'a str, Macro<'a>>,
        tokens: impl Iterator<Item = Token<'a>>,
    ) -> Token<'a> {
        let mut tokens = tokens.peekable();
        while let Some(token) = tokens.next() {
            match token {
                token if let Some(&r#macro) = macros.get(token.slice()) => {
                    assert!(self.is_empty());
                    match r#macro.expand(self.sess, tokens.by_ref(), &token) {
                        Some(expanding) => {
                            self.push(expanding);
                            while let Some(token) = self.next(macros) {
                                yield token;
                            }
                        }
                        None => yield token,
                    }
                }
                token => {
                    yield token;
                }
            }
        }
    }

    fn push(&mut self, expanding: Expanding<'a>) {
        match expanding {
            Expanding::Argument(Argument { call, update_hideset, .. }) =>
                if update_hideset {
                    assert!(self.hidden.remove(call.name))
                },
            _ if let Some(name) = expanding.name() => assert!(self.hidden.insert(name)),
            _ => (),
        }
        self.todo.push(expanding);
    }

    fn done(&mut self, expanding: &Expanding<'a>) {
        match expanding {
            Expanding::Argument(Argument { call, update_hideset, .. }) =>
                if *update_hideset {
                    assert!(self.hidden.insert(call.name))
                },
            _ if let Some(name) = expanding.name() => assert!(self.hidden.remove(name)),
            _ => (),
        }
        self.todo.pop();
    }

    fn expand_va_opt(&mut self, macros: &HashMap<&'a str, Macro<'a>>, va_opt: VaOpt<'a>) {
        let VaOpt { call, replacement } = va_opt;
        let depth = self.todo.len();
        let fake_va_args = Expanding::Argument(Argument {
            call,
            replacement: call.get_va_args(),
            update_hideset: false,
        });
        self.push(fake_va_args.clone());
        if self.next_until(macros, depth).is_some() {
            self.done(&fake_va_args);
            self.push(Expanding::Argument(Argument {
                call,
                replacement,
                update_hideset: false,
            }));
        }
    }

    fn stringise(
        &mut self,
        macros: &HashMap<&'a str, Macro<'a>>,
        stringise: Stringise<'a>,
    ) -> Token<'a> {
        let Stringise { call, replacement } = stringise;
        let argument = Expanding::Argument(Argument { call, replacement, update_hideset: true });
        let depth = self.todo.len();
        self.push(argument);
        let mut result = vec![b'"'];

        struct TokenDisplayer<'a>(Token<'a>);

        impl Display for TokenDisplayer<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let Self(token) = self;
                match token.kind {
                    TokenKind::String | TokenKind::CharConstant(_) =>
                        token.slice().chars().try_for_each(|c| match c {
                            '"' | '\\' => write!(f, "\\{c}"),
                            _ => write!(f, "{c}"),
                        }),
                    _ => write!(f, "{}", token.slice()),
                }
            }
        }

        write_preprocessed_tokens(
            &mut result,
            from_fn(|| self.next_until(macros, depth)),
            TokenDisplayer,
        )
        .unwrap();

        result.push(b'"');
        Token::from_str(
            self.sess.bump,
            TokenKind::String,
            self.sess.alloc_str(str::from_utf8(&result).unwrap()),
        )
    }

    fn paste(&mut self, macros: &HashMap<&'a str, Macro<'a>>, concat: Concat<'a>) {
        let Concat { call, lhs, hash_hash, rhs } = concat;
        let bump = self.sess.bump();

        let lhs = lhs.expand(call);
        let depth = self.todo.len();
        self.push(Expanding::Wrapped(Some(lhs)));
        let mut tokens = from_fn(|| self.next_until(macros, depth)).collect_vec();

        let rhs = rhs.expand(call);
        let depth = self.todo.len();
        self.push(Expanding::Wrapped(Some(rhs)));

        let lhs = tokens.pop();
        let rhs = self.next_until(macros, depth);

        let src = format!(
            "{}{}",
            lhs.map_or("", |t| t.slice()),
            rhs.map_or("", |t| t.slice()),
        );

        if !src.is_empty() {
            let pasted_tokens =
                panko_lex::lex(bump, Path::new("<scratch area>"), &src).collect_vec();
            match pasted_tokens[..] {
                [token] => tokens.push(token),
                _ => {
                    tokens.extend(pasted_tokens);
                    self.sess.emit(Diagnostic::InvalidPaste {
                        at: lhs
                            .unwrap_or(hash_hash)
                            .loc()
                            .until(rhs.unwrap_or(hash_hash).loc()),
                        lhs: lhs.map_or("", |t| t.slice()),
                        rhs: rhs.map_or("", |t| t.slice()),
                        result: self.sess.alloc_str(&src),
                    })
                }
            };
        }
        tokens.extend(from_fn(|| self.next_until(macros, depth)));
        self.push(Expanding::Tokens(VecDeque::from(tokens)));
    }

    fn next_until(
        &mut self,
        macros: &HashMap<&'a str, Macro<'a>>,
        depth: usize,
    ) -> Option<Token<'a>> {
        loop {
            let token = loop {
                let expanding = self.todo.get_mut(depth..)?.last_mut()?;
                match expanding.next() {
                    Expanded::Token(token) => break token,
                    Expanded::Argument(argument) => self.push(Expanding::Argument(argument)),
                    Expanded::VaOpt(va_opt) => self.expand_va_opt(macros, va_opt),
                    Expanded::Stringise(stringise) => break self.stringise(macros, stringise),
                    Expanded::Concat(concat) => self.paste(macros, concat),
                    Expanded::Done => {
                        let expanding = expanding.clone();
                        self.done(&expanding)
                    }
                }
            };

            let name = token.slice();
            match macros.get(name) {
                Some(r#macro) if !self.hidden.contains(name) => {
                    let sess = self.sess;
                    let tokens = &mut from_fn(|| self.next_until(macros, depth)).peekable();
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

    fn next(&mut self, macros: &HashMap<&'a str, Macro<'a>>) -> Option<Token<'a>> {
        self.next_until(macros, 0)
    }
}

impl<'a> Preprocessor<'a> {
    fn next(&mut self) -> Option<(bool, Token<'a>)> {
        self.tokens.last_mut().next_full()
    }

    fn next_token(&mut self) -> Option<Token<'a>> {
        self.tokens.last_mut().next()
    }

    #[define_opaque(TokenIter)]
    fn run(mut self) -> TokenIter<'a> {
        gen move {
            loop {
                while let Some((previous_was_newline, token)) = self.next() {
                    match token {
                        token if token.kind == TokenKind::Hash =>
                            if previous_was_newline {
                                self.parse_directive(&token);
                            }
                            else {
                                let line = self.eat_until_newline();
                                let diagnostic = match &line[..] {
                                    [] => Diagnostic::HashOutsideDirective { at: token },
                                    [directive, line @ ..] if is_identifier(directive) =>
                                        Diagnostic::NamedDirectiveDoesNotStartAtBeginningOfLine {
                                            at: token,
                                            directive: *directive,
                                            line: tokens_loc(line),
                                        },
                                    line => Diagnostic::DirectiveDoesNotStartAtBeginningOfLine {
                                        at: token,
                                        line: tokens_loc(line),
                                    },
                                };
                                self.sess.emit(diagnostic)
                            },
                        token if let Some(&r#macro) = self.macros.get(token.slice()) => {
                            assert!(self.expander.is_empty());
                            match r#macro.expand(self.sess, self.tokens.last_mut(), &token) {
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
                            yield token;
                        }
                    }
                }
                emit_unterminated_if_errors(self.sess, &self.tokens.last_mut().unclosed_ifs);
                if self.tokens.pop().is_none() {
                    break;
                }
            }
        }
    }

    fn peek(&mut self) -> Option<&Token<'a>> {
        self.tokens.last_mut().tokens.peek()
    }

    fn parse_directive(&mut self, hash: &Token<'a>) {
        match self.peek() {
            None => {
                // null directive at end of file without newline (technically UB)
            }
            Some(token) if token.kind == TokenKind::Newline => {
                // null directive
            }
            Some(token) if token.slice() == "define" => {
                self.parse_define(hash);
            }
            Some(token) if token.slice() == "undef" => {
                self.parse_undef(hash);
            }
            Some(token) if token.slice() == "if" => {
                let r#if = self.next_token().unwrap();
                self.eval_if(hash.loc().until(r#if.loc()));
            }
            Some(token) if token.slice() == "ifdef" => {
                let ifdef = self.next_token().unwrap();
                self.eval_ifdef(hash, &ifdef);
            }
            Some(token) if token.slice() == "ifndef" => {
                let ifndef = self.next_token().unwrap();
                self.eval_ifndef(hash, &ifndef);
            }
            Some(&token) if ["elif", "elifdef", "elifndef", "else"].contains(&token.slice()) => {
                // eat token
                let _ = self.next_token().unwrap();
                if self.tokens.last_mut().unclosed_ifs.is_empty() {
                    self.sess
                        .emit(Diagnostic::UnmatchedElif { at: hash.loc().until(token.loc()) })
                }
                if token.slice() == "else" {
                    self.parse_else(hash, &token);
                }
                self.skip_to_endif();
            }
            Some(&token) if token.slice() == "endif" => {
                self.next();
                self.parse_endif(hash, &token, true);
            }
            Some(token) if token.slice() == "include" => {
                self.eval_include(hash);
            }
            Some(token) =>
                todo!("error: unimplemented preprocessor directive starting in {token:?}"),
        }
    }

    fn parse_define(&mut self, hash: &Token<'a>) {
        let define = self.next_token().unwrap();
        let define_loc = || hash.loc().until(define.loc());
        let line = self.eat_until_newline();
        match &line[..] {
            [] => self.sess.emit(Diagnostic::EmptyDefine { at: define_loc() }),
            [name_tok, line @ ..] if is_identifier(name_tok) => {
                let line = self.sess.alloc_slice_copy(line);
                // TODO: check `name` for `__VA_OPT__` and `__VA_ARGS__`
                let name = name_tok.slice();
                let r#macro = if line.first().is_some_and(|token| is_lparen(name_tok, token)) {
                    parse_function_like_define(self.sess, name_tok, line)
                }
                else {
                    // TODO: check for `__VA_OPT__` and `__VA_ARGS__` here
                    Macro::Object { name, replacement: line }
                };
                // TODO: check for redefinition
                self.macros.insert(name, r#macro);
            }
            [non_identifier, ..] => self.sess.emit(Diagnostic::DefineOfNonIdentifier {
                at: *non_identifier,
                define: define_loc(),
            }),
        }
    }

    fn parse_undef(&mut self, hash: &Token<'a>) {
        let undef = self.next_token().unwrap();
        let undef_loc = || hash.loc().until(undef.loc());
        let line = self.eat_until_newline();
        match &line[..] {
            [] => self.sess.emit(Diagnostic::EmptyUndef { at: undef_loc() }),
            [name] if is_identifier(name) => {
                self.macros.remove(name.slice());
            }
            [non_identifier] => self.sess.emit(Diagnostic::DefineOfNonIdentifier {
                at: *non_identifier,
                define: undef_loc(),
            }),
            [name, rest @ ..] => self.sess.emit(Diagnostic::ExtraneousTokensInUndef {
                at: undef_loc(),
                name: *name,
                tokens: tokens_loc(rest),
            }),
        }
    }

    fn eat_until_newline(&mut self) -> Vec<Token<'a>> {
        self.tokens.last_mut().eat_until_newline()
    }

    fn require_no_trailing_tokens(&mut self, at: impl FnOnce() -> Loc<'a>) {
        let rest_of_line = self.eat_until_newline();
        if !rest_of_line.is_empty() {
            self.sess.emit(Diagnostic::ExtraneousTokens {
                at: at(),
                tokens: tokens_loc(&rest_of_line),
            })
        }
    }

    fn parse_else(&mut self, hash: &Token<'a>, r#else: &Token<'a>) {
        self.require_no_trailing_tokens(|| hash.loc().until(r#else.loc()))
    }

    fn skip_to_else(&mut self) {
        let mut nested_conditionals = UnclosedIfs::default();
        while let Some((previous_was_newline, token)) = self.next() {
            if previous_was_newline && token.kind == TokenKind::Hash {
                let hash = &token;
                match self.next_token() {
                    Some(token) if IF_DIRECTIVE_INTRODUCERS.contains(&token.slice()) =>
                        nested_conditionals.push(hash.loc().until(token.loc())),
                    Some(token) if token.slice() == "endif" =>
                        if nested_conditionals.pop().is_none() {
                            return self.parse_endif(hash, &token, false);
                        },
                    Some(token) if token.slice() == "elif" && nested_conditionals.is_empty() => {
                        self.tokens.last_mut().pop_if().unwrap();
                        return self.eval_if(hash.loc().until(token.loc()));
                    }
                    Some(token) if token.slice() == "elifdef" && nested_conditionals.is_empty() => {
                        self.tokens.last_mut().pop_if().unwrap();
                        return self.eval_ifdef(hash, &token);
                    }
                    Some(token)
                        if token.slice() == "elifndef" && nested_conditionals.is_empty() =>
                    {
                        self.tokens.last_mut().pop_if().unwrap();
                        return self.eval_ifndef(hash, &token);
                    }
                    Some(token) if token.slice() == "else" && nested_conditionals.is_empty() =>
                        return self.parse_else(hash, &token),
                    Some(_) => (),
                    None => break,
                }
            }
        }
        emit_unterminated_if_errors(self.sess, &nested_conditionals)
    }

    fn parse_endif(&mut self, hash: &Token<'a>, endif: &Token<'a>, maybe_unmatched: bool) {
        let endif_loc = || hash.loc().until(endif.loc());
        if self.tokens.last_mut().pop_if().is_none() && maybe_unmatched {
            self.sess
                .emit(Diagnostic::UnmatchedElif { at: endif_loc() })
        }
        self.require_no_trailing_tokens(endif_loc)
    }

    fn skip_to_endif(&mut self) {
        let mut nested_conditionals = UnclosedIfs::default();
        while let Some((previous_was_newline, token)) = self.next() {
            if previous_was_newline && token.kind == TokenKind::Hash {
                let hash = &token;
                match self.next_token() {
                    Some(token) if IF_DIRECTIVE_INTRODUCERS.contains(&token.slice()) =>
                        nested_conditionals.push(hash.loc().until(token.loc())),
                    Some(token) if token.slice() == "endif" =>
                        if nested_conditionals.pop().is_none() {
                            return self.parse_endif(hash, &token, false);
                        },
                    Some(_) => (),
                    None => break,
                }
            }
        }
        emit_unterminated_if_errors(self.sess, &nested_conditionals)
    }

    fn parse_condition(&mut self) -> bool {
        let sess = self.sess;
        let tokens = &mut self.tokens.last_mut().until_newline().peekable();
        let tokens = gen {
            while let Some(token) = tokens.next() {
                // TODO: if there’s a syntax error in the `defined` expression this will leave
                // behind some tokens that will probably lead to further errors in the
                // expression parser
                match token.slice() == "defined" {
                    true =>
                        if let Some(macro_name) = parse_defined(sess, tokens, &token).value() {
                            let is_defined = is_macro_defined(&self.macros, macro_name);
                            yield zero_or_one_from_bool(sess, is_defined)
                        },
                    false => yield token,
                }
            }
        };
        let tokens = self.expander.expand_macros(&self.macros, tokens);
        let tokens = tokens.map(|token| match is_identifier(&token) {
            true => zero_or_one_from_bool(sess, token.slice() == "true"),
            false => token,
        });
        let typedef_names = RefCell::default();
        let is_in_typedef = Cell::default();
        let parser = crate::grammar::ConstantExpressionParser::new();
        let expr = parser
            .parse(sess, &typedef_names, &is_in_typedef, tokens)
            .unwrap_or_else(handle_parse_error(sess));
        eval(sess, &expr).into_bool().unwrap_or_else(|reports| {
            for report in reports {
                self.sess.emit(report)
            }
            false
        })
    }

    fn eval_if(&mut self, loc: Loc<'a>) {
        let condition = self.parse_condition();
        let _ = self.tokens.last_mut().eat(TokenKind::Newline);
        self.eval_if_condition(loc, condition);
    }

    fn parse_ifdef(&mut self, hash: &Token<'a>, ifdef: &Token<'a>) -> MaybeError<bool> {
        let result = match self.tokens.last_mut().eat_if(is_identifier) {
            Ok(ident) => MaybeError::new(is_macro_defined(&self.macros, ident.slice())),
            Err(token) => self
                .sess
                .emit(Diagnostic::UnexpectedTokenInDefinedExpression {
                    at: token,
                    defined: *ifdef,
                    expectation: "an identifier",
                }),
        };
        let extraneous = self.eat_until_newline();
        let extraneous = extraneous
            .get(if result.is_some() { 0 } else { 1 }..)
            .unwrap_or_default();
        if !extraneous.is_empty() {
            self.sess.emit(Diagnostic::ExtraneousTokens {
                at: hash.loc().until(ifdef.loc()),
                tokens: tokens_loc(extraneous),
            })
        }
        result
    }

    fn eval_ifdef(&mut self, hash: &Token<'a>, ifdef: &Token<'a>) {
        let condition = self.parse_ifdef(hash, ifdef);
        self.eval_if_condition(
            hash.loc().until(ifdef.loc()),
            condition.value().unwrap_or(false),
        );
    }

    fn eval_ifndef(&mut self, hash: &Token<'a>, ifndef: &Token<'a>) {
        let condition = self.parse_ifdef(hash, ifndef);
        self.eval_if_condition(
            hash.loc().until(ifndef.loc()),
            condition.value().map(bool::not).unwrap_or(false),
        );
    }

    fn eval_if_condition(&mut self, loc: Loc<'a>, condition: bool) {
        self.tokens.last_mut().push_if(loc);
        if !condition {
            self.skip_to_else();
        }
    }

    fn eval_include(&mut self, hash: &Token<'a>) {
        let include = self.next_token().unwrap();
        let include_loc = || hash.loc().until(include.loc());
        let tokens = self.tokens.last_mut().until_newline();
        let tokens: Vec<_> = self.expander.expand_macros(&self.macros, tokens).collect();
        let (original_name, (included_filename, src), loc) = match &tokens[..] {
            [] =>
                return self
                    .sess
                    .emit(Diagnostic::IncludeDoesNotIncludeAnything { at: include_loc() }),
            [token] if token.kind == TokenKind::String => {
                let string = token.slice();
                let from_filename = include.loc().file();
                let filename = &string[1..string.len() - 1];
                if filename.is_empty() {
                    return self.sess.emit(Diagnostic::EmptyFilenameInInclude {
                        at: token.loc(),
                        include: include_loc(),
                    });
                }
                let included_file = self
                    .include_paths
                    .lookup_quoted(from_filename, Path::new(filename));
                (Cow::Borrowed(filename), included_file, token.loc())
            }
            [less, rest @ .., greater]
                if less.kind == TokenKind::Less && greater.kind == TokenKind::Greater =>
            {
                if rest.is_empty() {
                    return self.sess.emit(Diagnostic::EmptyFilenameInInclude {
                        at: less.loc().until(greater.loc()),
                        include: include_loc(),
                    });
                }
                let loc = tokens_loc(rest);
                let mut filename = Vec::new();
                write_preprocessed_tokens(&mut filename, rest.iter().copied(), |token| {
                    token.slice()
                })
                .unwrap();
                let filename = String::from_utf8(filename).unwrap();
                let included_file = self.include_paths.lookup_bracketed(&filename);
                (Cow::Owned(filename), included_file, loc)
            }
            tokens => todo!("error message: invalid `#include` directive with tokens {tokens:#?}"),
        };

        let filename = alloc_path(self.sess.bump, &included_filename);
        match src {
            Ok(src) => self.tokens.push(UnpreprocessedTokens::new(panko_lex::lex(
                self.sess.bump(),
                filename,
                &src,
            ))),
            Err(err) => self.sess.emit(Diagnostic::CouldNotReadIncludeFile {
                at: loc,
                include: include_loc(),
                error: self.sess.alloc_str(&err.to_string()),
                filename: self.sess.alloc_str(&original_name),
            }),
        }
    }
}

fn is_macro_defined(macros: &HashMap<&str, Macro>, macro_name: &str) -> bool {
    ["__has_include", "__has_embed", "__has_c_attribute"].contains(&macro_name)
        || macros.contains_key(macro_name)
}

fn parse_defined<'a>(
    sess: &Session<'a>,
    tokens: &mut impl Eat<'a>,
    defined: &Token<'a>,
) -> MaybeError<&'a str> {
    let unexpected = |at, expectation| {
        sess.emit(Diagnostic::UnexpectedTokenInDefinedExpression {
            at,
            defined: *defined,
            expectation,
        })
    };

    let is_parenthesised = tokens.eat(TokenKind::LParen).is_ok();

    match tokens.eat_if(is_identifier) {
        Ok(_) if is_parenthesised && let Err(token) = tokens.eat(TokenKind::RParen) =>
            unexpected(token, "a closing parenthesis"),
        Ok(ident) => MaybeError::new(ident.slice()),
        Err(token) => unexpected(token, "an identifier"),
    }
}

// TODO: it’s too easy to forget to call this
fn emit_unterminated_if_errors<'a>(sess: &Session<'a>, unclosed_ifs: &UnclosedIfs<'a>) {
    for loc in unclosed_ifs {
        sess.emit(Diagnostic::UnterminatedIf { at: *loc })
    }
}

fn zero_or_one_from_bool<'a>(sess: &Session<'a>, b: bool) -> Token<'a> {
    // TODO: this does not track source locations and probably should not allocate
    Token::from_str(
        sess.bump,
        TokenKind::Integer(Integer {
            suffix: IntegerSuffix::None,
            suffix_len: 0,
            base: 10,
            prefix_len: 0,
        }),
        if b { "1" } else { "0" },
    )
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

fn parse_va_opt<'a>(
    sess: &'a Session<'a>,
    parameters: &IndexSet<&str>,
    is_varargs: bool,
    va_opt: &Token<'a>,
    tokens: &mut &'a [Token<'a>],
) -> Result<Replacement<'a>, ()> {
    eat(tokens, TokenKind::LParen)?;
    let iter = &mut tokens.iter().copied().peekable();
    let va_opt_tokens_count = eat_until_in_balanced_parens(iter, |token| {
        if token.slice() == "__VA_OPT__" {
            sess.emit(Diagnostic::NestedVaOpt { at: *token, va_opt: *va_opt })
        }
        token.kind == TokenKind::RParen
    })
    .count();
    let va_opt_tokens = tokens.split_off(..va_opt_tokens_count).unwrap();
    let va_opt_tokens =
        parse_function_like_replacement(sess, parameters, is_varargs, va_opt_tokens);
    eat(tokens, TokenKind::RParen)?;
    Ok(Replacement::VaOpt(sess.alloc_slice_copy(&va_opt_tokens)))
}

fn parse_replacement<'a>(
    sess: &'a Session<'a>,
    parameters: &IndexSet<&str>,
    is_varargs: bool,
    tokens: &mut &'a [Token<'a>],
) -> Option<Replacement<'a>> {
    let token = tokens.split_off_first()?;
    let replacement = match token.kind {
        TokenKind::HashHash => {
            let () =
                sess.emit(Diagnostic::PasteAtBeginningOfMacro { at: *token, kind: "beginning" });
            parse_replacement(sess, parameters, is_varargs, tokens)?
        }
        TokenKind::Hash => match tokens.split_off_first() {
            Some(token) if let Some(index) = parameters.get_index_of(token.slice()) =>
                Replacement::Stringise(index),
            Some(_) => todo!("error: not a parameter (but allow `__VA_OPT__` and `__VA_ARGS__`)"),
            None => {
                let () = sess.emit(Diagnostic::StringiseAtEndOfMacro { at: *token });
                return None;
            }
        },
        _ => match token.slice() {
            "__VA_OPT__" => {
                if !is_varargs {
                    sess.emit(Diagnostic::VaArgsOrVaOptOutsideOfVariadicMacro { at: *token })
                }
                match parse_va_opt(sess, parameters, is_varargs, token, tokens) {
                    Ok(va_opt) => va_opt,
                    Err(()) => todo!("error message: error while parsing `__VA_OPT__`"),
                }
            }
            "__VA_ARGS__" => {
                if !is_varargs {
                    sess.emit(Diagnostic::VaArgsOrVaOptOutsideOfVariadicMacro { at: *token })
                }
                Replacement::VaArgs
            }
            _ => parse_token_as_maybe_parameter(parameters, token),
        },
    };
    if let Some(lookahead) = tokens.first()
        && lookahead.kind == TokenKind::HashHash
    {
        let hash_hash = tokens.split_off_first().unwrap();
        match parse_replacement(sess, parameters, is_varargs, tokens) {
            Some(rhs) => Some(Replacement::Concat {
                lhs: sess.alloc(replacement),
                hash_hash: *hash_hash,
                rhs: sess.alloc(rhs),
            }),
            None => {
                let () =
                    sess.emit(Diagnostic::PasteAtBeginningOfMacro { at: *hash_hash, kind: "end" });
                Some(replacement)
            }
        }
    }
    else {
        Some(replacement)
    }
}

fn parse_function_like_replacement<'a>(
    sess: &'a Session<'a>,
    parameters: &IndexSet<&str>,
    is_varargs: bool,
    mut tokens: &'a [Token<'a>],
) -> Vec<Replacement<'a>> {
    from_fn(|| parse_replacement(sess, parameters, is_varargs, &mut tokens)).collect()
}

fn parse_token_as_maybe_parameter<'a>(
    parameters: &IndexSet<&str>,
    token: &Token<'a>,
) -> Replacement<'a> {
    // TODO: check for `__VA_OPT__` and `__VA_ARGS__` here
    match parameters.get_index_of(token.slice()) {
        Some(param_index) => Replacement::Parameter(param_index),
        None => Replacement::Literal(*token),
    }
}

fn parse_function_like_define<'a>(
    sess: &'a Session<'a>,
    name: &Token<'a>,
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
                    if !parameters.insert(token.slice()) {
                        sess.emit(Diagnostic::DuplicateMacroParamName { at: *token })
                    }
                }
                else {
                    sess.emit(Diagnostic::NonIdentifierParameter { at: *token })
                }
            }
            None => break,
        }

        if eat(&mut tokens, TokenKind::Comma).is_err() {
            break;
        }
    }

    let is_varargs = eat(&mut tokens, TokenKind::Ellipsis).is_ok();
    // TODO: differentiate between `#define M(a, b` and `#define M(a b)` (that case should not emit
    // a missing rparen error)
    if let Err(()) = eat(&mut tokens, TokenKind::RParen) {
        sess.emit(Diagnostic::MissingRParenInMacroInvocation { at: *name, kind: "definition" })
    }

    let replacement = parse_function_like_replacement(sess, &parameters, is_varargs, tokens);

    Macro::Function {
        name: name.slice(),
        parameter_count: parameters.len(),
        is_varargs,
        replacement: sess.alloc_slice_copy(&replacement),
    }
}

fn eat_until_in_balanced_parens<'a>(
    tokens: &mut impl PeekingNext<Item = Token<'a>>,
    predicate: impl Fn(&Token<'a>) -> bool,
) -> impl Iterator<Item = Token<'a>> {
    let mut nesting_level = 0_usize;
    tokens.peeking_take_while(move |token| match token.kind {
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
}

fn eat_newlines<'a>(tokens: &mut impl PeekingNext<Item = Token<'a>>) {
    #[expect(clippy::redundant_pattern_matching)]
    while let Some(_) = tokens.peeking_next(|token| token.kind == TokenKind::Newline) {}
}

fn parse_macro_arguments<'a>(
    sess: &'a Session<'a>,
    macro_name: &Token<'a>,
    parameter_count: usize,
    tokens: &mut impl PeekingNext<Item = Token<'a>>,
) -> &'a [&'a [Replacement<'a>]] {
    eat_newlines(tokens);
    #[expect(clippy::redundant_pattern_matching)]
    if let Some(_) = tokens.peeking_next(|token| token.kind == TokenKind::RParen) {
        return &[];
    }

    let mut arguments = Vec::new();

    loop {
        let is_varargs_argument = arguments.len() == parameter_count;
        let argument = eat_until_in_balanced_parens(tokens, |token| match is_varargs_argument {
            true => matches!(token.kind, TokenKind::RParen),
            false => matches!(token.kind, TokenKind::RParen | TokenKind::Comma),
        });
        arguments.push(sess.alloc_slice_copy(&argument.map(Replacement::Literal).collect_vec()));

        match tokens.next() {
            Some(token) if token.kind == TokenKind::RParen => break,
            Some(token) if token.kind == TokenKind::Comma => (),
            Some(_) => unreachable!(),
            None => {
                let () = sess.emit(Diagnostic::MissingRParenInMacroInvocation {
                    at: *macro_name,
                    kind: "invocation",
                });
                let is_newline = |replacement| {
                    matches!(
                        replacement,
                        &Replacement::Literal(token) if token.kind == TokenKind::Newline,
                    )
                };
                let is_first_argument = arguments.len() == 1;
                arguments.pop_if(|argument| is_first_argument && argument.iter().all(is_newline));
                break;
            }
        };
    }

    sess.alloc_slice_copy(&arguments)
}

pub fn preprocess<'a>(
    sess: &'a Session<'a>,
    tokens: panko_lex::TokenIter<'a>,
    include_paths: IncludePaths,
) -> TokenIter<'a> {
    Preprocessor {
        sess,
        tokens: nonempty::Vec::new(UnpreprocessedTokens::new(tokens)),
        macros: HashMap::from_iter([("__LINE__", Macro::Line), ("__FILE__", Macro::File)]),
        expander: Expander {
            sess,
            hidden: HashSet::default(),
            todo: Vec::default(),
        },
        include_paths,
    }
    .run()
    // TODO: check for `__VA_OPT__` and `__VA_ARGS__` here
}

fn write_preprocessed_tokens<'a, D: Display>(
    mut w: impl std::io::Write,
    mut tokens: impl Iterator<Item = Token<'a>>,
    stringify_token: impl Fn(Token<'a>) -> D,
) -> std::io::Result<()> {
    tokens
        .try_fold(None::<Token>, |last, token| {
            if let Some(last) = last
                && token.kind != TokenKind::Newline
            {
                if last.kind == TokenKind::Newline {
                    // TODO: this breaks indentation if the first token on a line is from a
                    // different file than the last token on the last line
                    let indent = token
                        .loc()
                        .src()
                        .get(last.loc().end()..)
                        .unwrap_or_default()
                        .find(|c: char| !c.is_whitespace())
                        .unwrap_or(0);
                    write!(w, "{:indent$}", "")?;
                }
                // TODO: this should check that both source files are the same (so that two tokens
                // `#include`d from different files such that their locations match are not
                // recognised as adjacent).
                // TODO: this does not take into account that `last` could have come from a macro
                // expansion. The preprocessor should track the original location of each token for
                // this and for error messages.
                else if last.loc().end() != token.loc().start() {
                    write!(w, " ")?;
                }
            }
            write!(w, "{}", stringify_token(token))?;
            Ok(Some(token))
        })
        .map(|_| ())
}

pub fn print_preprocessed_source(tokens: TokenIter) {
    write_preprocessed_tokens(stdout().lock(), tokens, |token| token.slice()).unwrap()
}
