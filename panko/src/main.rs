use std::path::Path;

use bumpalo::Bump;
use panko_parser::sexpr_builder::AsSExpr as _;

fn main() {
    let bump = &Bump::new();
    let tokens = panko_lex::lex(
        bump,
        Path::new("main.c"),
        &std::fs::read_to_string("main.c").unwrap(),
    );
    let translation_unit = panko_parser::parse(bump, tokens).unwrap();
    let translation_unit = panko_sema::resolve_names(bump, translation_unit);
    println!("{}", translation_unit.as_sexpr());
}
