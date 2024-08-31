#![feature(exit_status_error)]

use std::fs::File;
use std::io::Write as _;
use std::path::PathBuf;
use std::process::Command;
use std::process::ExitCode;

use bumpalo::Bump;
use clap::Parser;
use clap::ValueEnum;
use panko_parser::sexpr_builder::AsSExpr as _;

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum Step {
    Scopes,
    Typeck,
    Codegen,
    Assemble,
    Link,
}

#[derive(Debug, Parser)]
struct Args {
    filename: PathBuf,
    #[arg(long)]
    stop_after: Option<Step>,
    #[arg(long)]
    print: Vec<Step>,
    #[arg(short, long)]
    output_filename: Option<PathBuf>,
}

fn main_impl() -> Result<(), ExitCode> {
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
            return Err(ExitCode::from(err.exit_code()));
        }
    };
    let translation_unit = panko_sema::resolve_names(session, translation_unit);
    session.handle_diagnostics()?;

    if args.print.contains(&Step::Scopes) {
        println!("{}", translation_unit.as_sexpr());
    }
    if let Some(Step::Scopes) = args.stop_after {
        return Ok(());
    }

    let translation_unit = panko_sema::resolve_types(session, translation_unit);
    session.handle_diagnostics()?;

    if args.print.contains(&Step::Typeck) {
        println!("{}", translation_unit.as_sexpr());
    }
    if let Some(Step::Typeck) = args.stop_after {
        return Ok(());
    }

    let code = panko_codegen::emit(translation_unit);

    if args.print.contains(&Step::Codegen) {
        println!("{code}");
    }
    if let Some(Step::Codegen) = args.stop_after {
        return Ok(());
    }

    let output_filename = args
        .output_filename
        .unwrap_or_else(|| args.filename.with_extension("S"));

    File::create(&output_filename)
        .unwrap()
        .write_all(code.as_bytes())
        .unwrap();

    let object_filename = output_filename.with_extension("o");
    Command::new("as")
        .arg(&output_filename)
        .arg("-o")
        .arg(&object_filename)
        .status()
        .unwrap()
        .exit_ok()
        .unwrap();

    if args.print.contains(&Step::Assemble) {
        Command::new("objdump")
            .arg("-d")
            .arg("-Mintel")
            .arg(&object_filename)
            .status()
            .unwrap()
            .exit_ok()
            .unwrap();
    }
    if let Some(Step::Codegen) = args.stop_after {
        return Ok(());
    }

    let executable_filename = object_filename.with_extension("");
    Command::new("cc")
        .arg(&object_filename)
        .arg("-o")
        .arg(&executable_filename)
        .status()
        .unwrap()
        .exit_ok()
        .unwrap();

    if args.print.contains(&Step::Link) {
        Command::new("objdump")
            .arg("-d")
            .arg("-Mintel")
            .arg(&executable_filename)
            .status()
            .unwrap()
            .exit_ok()
            .unwrap();
    }
    if let Some(Step::Link) = args.stop_after {
        return Ok(());
    }

    Ok(())
}

fn main() -> ExitCode {
    match main_impl() {
        Ok(()) => ExitCode::SUCCESS,
        Err(exit_code) => exit_code,
    }
}
