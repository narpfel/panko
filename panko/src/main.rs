use std::path::PathBuf;

use bumpalo::Bump;
use clap::Parser;
use panko_parser::sexpr_builder::AsSExpr as _;
use panko_parser::Report as _;

#[derive(Debug, Parser)]
struct Args {
    filename: PathBuf,
}

fn main() {
    let args = Args::parse();
    let bump = &Bump::new();
    let tokens = panko_lex::lex(
        bump,
        &args.filename,
        &std::fs::read_to_string(&args.filename).unwrap(),
    );
    let session = &panko_parser::ast::Session::new(bump);
    let translation_unit = panko_parser::parse(session, tokens).unwrap();
    if !session.diagnostics().is_empty() {
        for (i, diagnostic) in session.diagnostics().iter().enumerate() {
            if i != 0 {
                eprintln!();
            }
            diagnostic.print();
        }

        todo!("not all diagnostics are fatal");
    }
    let translation_unit = panko_sema::resolve_names(session, translation_unit);
    println!("{}", translation_unit.as_sexpr());
}
