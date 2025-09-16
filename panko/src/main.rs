#![feature(exit_status_error)]
#![feature(unqualified_local_imports)]

use std::cell::Cell;
use std::cell::RefCell;
use std::env;
use std::fs::File;
use std::io::Write as _;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

use clap::Parser;
use clap::ValueEnum;
use color_eyre::Result;
use color_eyre::eyre::Context;
use panko_lex::Bump;
use panko_lex::TokenKind;
use panko_parser::sexpr_builder::AsSExpr as _;
use yansi::Condition;

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum Step {
    Preprocess,
    Ast,
    Scopes,
    Typeck,
    Layout,
    Codegen,
    Assemble,
    Link,
}

#[derive(Debug, Clone, Parser)]
struct IncludePaths {
    #[arg(long = "iquote")]
    quoted: Vec<PathBuf>,
    #[arg(short = 'I', long = "isystem")]
    bracketed: Vec<PathBuf>,
}

impl From<IncludePaths> for panko_parser::preprocess::IncludePaths {
    fn from(paths: IncludePaths) -> Self {
        let IncludePaths { quoted, bracketed } = paths;
        Self::new(quoted, bracketed)
    }
}

#[derive(Debug, Parser)]
struct Args {
    #[arg(required = true)]
    filenames: Vec<PathBuf>,
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
    #[clap(flatten)]
    include_paths: IncludePaths,
}

struct CompileArgs {
    stop_after: Option<Step>,
    print: Vec<Step>,
    debug: bool,
    treat_error_as_bug: bool,
    include_paths: IncludePaths,
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

    let stop_after = match args.filenames.len() == 1 {
        true => args.stop_after,
        false => args.stop_after.or(Some(Step::Assemble)),
    };
    let compile_args = CompileArgs {
        stop_after,
        print: args.print,
        debug: args.debug,
        treat_error_as_bug: args.treat_error_as_bug,
        include_paths: args.include_paths,
    };

    let object_filenames: Result<Vec<Result<_, ()>>> = args
        .filenames
        .iter()
        .map(|filename| {
            compile(
                filename,
                args.output_filename
                    .as_ref()
                    .map_or(Path::new(""), |path| {
                        path.parent().expect("TODO: is this unreachable?")
                    })
                    .join(filename.file_name().expect("TODO: is this unreachable?"))
                    .with_extension("o"),
                &compile_args,
            )
        })
        .collect();

    if let Some(Step::Assemble) = args.stop_after {
        return Ok(());
    }

    let object_filenames: Vec<_> = match object_filenames?.into_iter().collect() {
        Ok(object_filenames) => object_filenames,
        Err(()) => return Ok(()),
    };

    let executable_filename = args
        .output_filename
        .unwrap_or_else(|| match &args.filenames[..] {
            [] => unreachable!(),
            [filename] => Path::new(filename.file_name().expect("TODO: is this unreachable?"))
                .with_extension(""),
            [_, _, ..] => PathBuf::from("a.out"),
        });

    link(&executable_filename, &object_filenames, &compile_args.print)?;

    if let Some(Step::Link) = args.stop_after {
        return Ok(());
    }

    Ok(())
}

fn compile(
    filename: &Path,
    object_filename: PathBuf,
    args: &CompileArgs,
) -> Result<Result<PathBuf, ()>> {
    let bump = &Bump::new();
    let typedef_names = RefCell::default();
    let is_in_typedef = Cell::new(false);
    let tokens = panko_lex::lex(
        bump,
        filename,
        &std::fs::read_to_string(filename)
            .with_context(|| format!("could not open source file `{}`", filename.display()))?,
    );
    let session = &panko_parser::ast::Session::new(bump, args.treat_error_as_bug);

    let tokens = panko_parser::preprocess(session, tokens, args.include_paths.clone().into());

    // TODO: diagnostics emitted by the preprocessor are not handled when stopping here
    if args.print.contains(&Step::Preprocess) {
        panko_parser::preprocess::print_preprocessed_source(tokens);
        return Ok(Err(()));
    }
    if let Some(Step::Preprocess) = args.stop_after {
        return Ok(Err(()));
    }

    let tokens = tokens.filter(|token| token.kind != TokenKind::Newline);

    let tokens = panko_lex::apply_lexer_hack(tokens, &typedef_names);

    let translation_unit =
        panko_parser::parse(session, filename, &typedef_names, &is_in_typedef, tokens);
    let translation_unit = match translation_unit {
        Some(translation_unit) => translation_unit,
        None => {
            session.handle_diagnostics();
            unreachable!()
        }
    };

    if args.print.contains(&Step::Ast) {
        println!("{}", translation_unit.as_sexpr());
    }
    if let Some(Step::Ast) = args.stop_after {
        return Ok(Err(()));
    }

    let translation_unit = panko_sema::resolve_names(session, translation_unit);
    session.handle_diagnostics();

    if args.print.contains(&Step::Scopes) {
        println!("{}", translation_unit.as_sexpr());
    }
    if let Some(Step::Scopes) = args.stop_after {
        return Ok(Err(()));
    }

    let translation_unit = panko_sema::resolve_types(session, translation_unit);
    session.handle_diagnostics();

    if args.print.contains(&Step::Typeck) {
        println!("{}", translation_unit.as_sexpr());
    }
    if let Some(Step::Typeck) = args.stop_after {
        return Ok(Err(()));
    }

    let translation_unit = panko_sema::layout(bump, translation_unit);

    if args.print.contains(&Step::Layout) {
        println!("{}", translation_unit.as_sexpr());
    }
    if let Some(Step::Layout) = args.stop_after {
        return Ok(Err(()));
    }

    let code = panko_codegen::emit(translation_unit, args.debug);

    if args.print.contains(&Step::Codegen) {
        println!("{}{}", code.0, code.1);
    }
    if let Some(Step::Codegen) = args.stop_after {
        return Ok(Err(()));
    }

    let assembly_filename = object_filename.with_extension("S");
    write!(
        File::create(&assembly_filename).wrap_err_with(|| {
            format!(
                "could not create output file `{}`",
                assembly_filename.display(),
            )
        })?,
        "{}{}",
        code.0,
        code.1,
    )
    .wrap_err_with(|| {
        format!(
            "could not write to output file `{}`",
            object_filename.display(),
        )
    })?;

    Command::new("as")
        .arg(&assembly_filename)
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

    Ok(Ok(object_filename))
}

fn link(executable_filename: &Path, object_filenames: &[PathBuf], print: &[Step]) -> Result<()> {
    Command::new("cc")
        .args(object_filenames)
        .arg("-o")
        .arg(executable_filename)
        .status()
        .wrap_err_with(|| "could not execute linker `cc`")?
        .exit_ok()
        .wrap_err_with(|| "linker `cc` failed")?;
    if print.contains(&Step::Link) {
        Command::new("objdump")
            .arg("-d")
            .arg("-Mintel")
            .arg(executable_filename)
            .status()?
            .exit_ok()?;
    }
    Ok(())
}
