#![feature(exit_status_error)]
#![feature(unqualified_local_imports)]

use std::cell::Cell;
use std::cell::RefCell;
use std::env;
use std::fs::File;
use std::io::Write as _;
use std::path::PathBuf;
use std::process::Command;

use bumpalo::Bump;
use clap::Parser;
use clap::ValueEnum;
use color_eyre::Result;
use color_eyre::eyre::Context;
use panko_parser::sexpr_builder::AsSExpr as _;
use yansi::Condition;

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum Step {
    Ast,
    Scopes,
    Typeck,
    Layout,
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
    /// emit debug info
    #[arg(short('g'), long)]
    debug: bool,
    /// panic whenever a diagnostic is emitted
    #[arg(long)]
    treat_error_as_bug: bool,
}

fn main() -> Result<()> {
    color_eyre::install()?;

    let enable_colours = if env::var_os("NO_COLOR").is_some() {
        false
    }
    else if env::var_os("CLICOLOR_FORCE").is_some() {
        true
    }
    else {
        Condition::stdouterr_are_tty()
    };
    yansi::whenever(Condition::cached(enable_colours));

    let args = Args::parse();
    let bump = &Bump::new();
    let typedef_names = RefCell::default();
    let is_in_typedef = Cell::new(false);
    let tokens = panko_lex::lex(
        bump,
        &args.filename,
        &std::fs::read_to_string(&args.filename)
            .with_context(|| format!("could not open source file `{}`", args.filename.display()))?,
        &typedef_names,
    );
    let session = &panko_parser::ast::Session::new(bump, args.treat_error_as_bug);

    let tokens = panko_parser::preprocess(session, tokens);

    let translation_unit =
        match panko_parser::parse(session, &typedef_names, &is_in_typedef, tokens) {
            Ok(translation_unit) => translation_unit,
            Err(err) => {
                err.print();
                std::process::exit(err.exit_code().into());
            }
        };

    if args.print.contains(&Step::Ast) {
        println!("{}", translation_unit.as_sexpr());
    }
    if let Some(Step::Ast) = args.stop_after {
        return Ok(());
    }

    let translation_unit = panko_sema::resolve_names(session, translation_unit);
    session.handle_diagnostics();

    if args.print.contains(&Step::Scopes) {
        println!("{}", translation_unit.as_sexpr());
    }
    if let Some(Step::Scopes) = args.stop_after {
        return Ok(());
    }

    let translation_unit = panko_sema::resolve_types(session, translation_unit);
    session.handle_diagnostics();

    if args.print.contains(&Step::Typeck) {
        println!("{}", translation_unit.as_sexpr());
    }
    if let Some(Step::Typeck) = args.stop_after {
        return Ok(());
    }

    let translation_unit = panko_sema::layout(bump, translation_unit);

    if args.print.contains(&Step::Layout) {
        println!("{}", translation_unit.as_sexpr());
    }
    if let Some(Step::Layout) = args.stop_after {
        return Ok(());
    }

    let code = panko_codegen::emit(translation_unit, args.debug);

    if args.print.contains(&Step::Codegen) {
        println!("{}{}", code.0, code.1);
    }
    if let Some(Step::Codegen) = args.stop_after {
        return Ok(());
    }

    let output_filename = args
        .output_filename
        .unwrap_or_else(|| args.filename.with_extension("S"));

    write!(
        File::create(&output_filename).wrap_err_with(|| {
            format!(
                "could not create output file `{}`",
                output_filename.display(),
            )
        })?,
        "{}{}",
        code.0,
        code.1,
    )
    .wrap_err_with(|| {
        format!(
            "could not write to output file `{}`",
            output_filename.display(),
        )
    })?;

    let object_filename = output_filename.with_extension("o");
    Command::new("as")
        .arg(&output_filename)
        .arg("-o")
        .arg(&object_filename)
        .status()
        .wrap_err_with(|| "could not execute assembler `as`")?
        .exit_ok()
        .wrap_err_with(|| "assembler `as` failed")?;

    if args.print.contains(&Step::Assemble) {
        Command::new("objdump")
            .arg("-d")
            .arg("-Mintel")
            .arg(&object_filename)
            .status()?
            .exit_ok()?;
    }
    if let Some(Step::Assemble) = args.stop_after {
        return Ok(());
    }

    let executable_filename = object_filename.with_extension("");
    Command::new("cc")
        .arg(&object_filename)
        .arg("-o")
        .arg(&executable_filename)
        .status()
        .wrap_err_with(|| "could not execute linker `cc`")?
        .exit_ok()
        .wrap_err_with(|| "linker `cc` failed")?;

    if args.print.contains(&Step::Link) {
        Command::new("objdump")
            .arg("-d")
            .arg("-Mintel")
            .arg(&executable_filename)
            .status()?
            .exit_ok()?;
    }
    if let Some(Step::Link) = args.stop_after {
        return Ok(());
    }

    Ok(())
}
