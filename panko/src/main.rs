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
use color_eyre::eyre::eyre;
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
    #[arg(long, default_value = "ld")]
    ld_path: PathBuf,
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

    let Args {
        filenames,
        stop_after,
        print,
        output_filename,
        debug,
        treat_error_as_bug,
        include_paths,
        ld_path,
    } = Args::parse();

    let compile_args = CompileArgs {
        stop_after,
        print,
        debug,
        treat_error_as_bug,
        include_paths,
    };

    let object_filenames: Result<Vec<Result<_, ()>>> = filenames
        .iter()
        .map(|filename| {
            compile(
                filename,
                output_filename
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

    if let Some(Step::Assemble) = stop_after {
        return Ok(());
    }

    let object_filenames: Vec<_> = match object_filenames?.into_iter().collect() {
        Ok(object_filenames) => object_filenames,
        Err(()) => return Ok(()),
    };

    let executable_filename = output_filename.unwrap_or_else(|| match &filenames[..] {
        [] => unreachable!(),
        [filename] =>
            Path::new(filename.file_name().expect("TODO: is this unreachable?")).with_extension(""),
        [_, _, ..] => PathBuf::from("a.out"),
    });

    link(
        &ld_path,
        &executable_filename,
        &object_filenames,
        &compile_args.print,
    )?;

    if let Some(Step::Link) = stop_after {
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

    let assembly_filename = object_filename.with_extension("s");
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

fn find_crt_path(crt: &str) -> Result<PathBuf> {
    let search_paths = ["/usr/lib", "/usr/lib/x86_64-linux-gnu"];
    for path in search_paths {
        let path = Path::new(path).join(crt);
        if let Ok(true) = path.try_exists() {
            return Ok(path);
        }
    }
    Err(eyre!("could not find path for `{crt}`"))
}

fn link(
    ld_path: &Path,
    executable_filename: &Path,
    object_filenames: &[PathBuf],
    print: &[Step],
) -> Result<()> {
    Command::new(ld_path)
        .args([
            "-m",
            "elf_x86_64",
            "-pie",
            "-dynamic-linker",
            "/lib64/ld-linux-x86-64.so.2",
            "-L/usr/lib/x86_64-linux-gnu",
            "-L/usr/lib",
            "-L/lib",
        ])
        .arg(
            // for now we only need to link `Scrt1.o`, see https://stackoverflow.com/a/27786892
            // (mirror of http://dev.gentoo.org/%7Evapier/crt.txt)
            find_crt_path("Scrt1.o")?,
        )
        .args(object_filenames)
        .arg("-lc")
        .arg("-o")
        .arg(executable_filename)
        .status()
        .wrap_err_with(|| format!("could not execute linker `{}`", ld_path.display()))?
        .exit_ok()
        .wrap_err_with(|| format!("linker `{}` failed", ld_path.display()))?;
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
