use std::path::PathBuf;

use bumpalo::Bump;
use clap::Parser;
use panko_parser::sexpr_builder::AsSExpr as _;

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
    let translation_unit = match panko_parser::parse(session, tokens) {
        Ok(translation_unit) => translation_unit,
        Err(err) => {
            err.print();
            std::process::exit(err.exit_code());
        }
    };
    let translation_unit = panko_sema::resolve_names(session, translation_unit);
    if !session.diagnostics().is_empty() {
        for (i, diagnostic) in session.diagnostics().iter().enumerate() {
            if i != 0 {
                eprintln!();
            }
            diagnostic.print();
        }
        let exit_code = session
            .diagnostics()
            .iter()
            .map(|diagnostic| diagnostic.exit_code())
            .max()
            .unwrap();
        // TODO: not all diagnostics are fatal
        std::process::exit(exit_code);
    }
    println!("{}", translation_unit.as_sexpr());
}
