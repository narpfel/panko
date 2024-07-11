use std::path::Path;

use bumpalo::Bump;

fn main() {
    let bump = &Bump::new();
    let tokens = panko_lex::lex(
        bump,
        Path::new("main.c"),
        &std::fs::read_to_string("main.c").unwrap(),
    );
    let translation_unit = panko_parser::parse(bump, tokens).unwrap();
    println!("{}", translation_unit.as_sexpr());
}
