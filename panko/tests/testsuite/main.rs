#![feature(exit_status_error)]

use std::borrow::Borrow;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::process::ExitStatus;
use std::process::Output;

use insta_cmd::assert_cmd_snapshot;
use insta_cmd::get_cargo_bin;
use itertools::Itertools as _;
use regex::Captures;
use regex::Regex;
use rstest::rstest;

trait CaptureOutputForLibtest {
    fn status_with_captured_output(&mut self) -> io::Result<ExitStatus>;
}

impl CaptureOutputForLibtest for Command {
    fn status_with_captured_output(&mut self) -> io::Result<ExitStatus> {
        let output = self.output()?;
        if !output.stdout.is_empty() {
            println!("{}", String::from_utf8_lossy(&output.stdout));
        }
        if !output.stderr.is_empty() {
            eprintln!("{}", String::from_utf8_lossy(&output.stderr));
        }
        Ok(output.status)
    }
}

fn relative_to(path: &Path, target: impl AsRef<Path>) -> &Path {
    path.strip_prefix(target.as_ref()).unwrap()
}

#[rstest]
#[case::scopes("scope", "scopes")]
#[case::typeck("typeck", "typeck")]
#[case::layout("layout", "layout")]
fn test(
    #[case] snapshot_name_prefix: &str,
    #[case] step: &str,
    #[files("tests/cases/**/test_*.c")] filename: PathBuf,
) {
    let filename = relative_to(
        &filename,
        Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap(),
    );
    assert_cmd_snapshot!(
        format!("{snapshot_name_prefix}-{}", filename.display()),
        Command::new(get_cargo_bin("panko"))
            .current_dir("..")
            .arg(format!("--print={step}"))
            .arg(format!("--stop-after={step}"))
            .arg(filename),
    );
}

#[rstest]
fn execute_test(#[files("tests/cases/execute/**/test_*.c")] filename: PathBuf) {
    let source = std::fs::read_to_string(&filename).unwrap();

    let expected_return_code_re =
        Regex::new(r"(?m)^// \[\[return: (?P<return_code>.*?)\]\]$").unwrap();

    let expected_return_codes = expected_return_code_re.captures_iter(&source).map(|captures| {
        let return_code = captures.name("return_code").unwrap().as_str();
        return_code.parse::<i32>().unwrap_or_else(|err| {
            panic!("while parsing `return_code` in {expected_return_code_re:?}:\n{err:?} in capture {return_code:?}")
        })
    }).collect_vec();
    assert!(
        expected_return_codes.len() <= 1,
        "test source invalid: more than one return code set: {expected_return_codes:?}",
    );
    let expected_return_code = expected_return_codes.first().copied().unwrap_or(0);

    let expected_print_re = Regex::new(r"(?m)^// \[\[print: (?P<output>.*?)\]\]$").unwrap();
    let expected_output: String = expected_print_re
        .captures_iter(&source)
        .map(|captures| captures.name("output").unwrap().as_str().to_string() + "\n")
        .collect();

    let cmdline_arguments_re = Regex::new(r"(?m)^// \[\[arg: (?P<arg>.*?)\]\]$").unwrap();
    let expand_escape_sequences_re = Regex::new(r"\\[\\n]").unwrap();

    let cmdline_arguments = cmdline_arguments_re
        .captures_iter(&source)
        .map(|captures| {
            expand_escape_sequences_re.replace_all(
                captures.name("arg").unwrap().as_str(),
                |captures: &Captures| match captures.get(0).unwrap().as_str() {
                    r"\\" => "\\",
                    r"\n" => "\n",
                    _ => unreachable!(),
                },
            )
        })
        .collect_vec();

    let filename = relative_to(
        &filename,
        Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap(),
    );

    let output_dir = tempfile::tempdir().unwrap();
    let executable_filename = output_dir
        .path()
        .join(filename.file_name().unwrap())
        .with_extension("");

    Command::new(get_cargo_bin("panko"))
        .current_dir("..")
        .arg(filename)
        .arg("-o")
        .arg(&executable_filename)
        .status_with_captured_output()
        .unwrap()
        .exit_ok()
        .unwrap();

    let Output { status, stdout, stderr } = Command::new(&executable_filename)
        .args(cmdline_arguments.iter().map(Borrow::<str>::borrow))
        .output()
        .unwrap();
    let stdout = std::str::from_utf8(&stdout).unwrap();
    let stderr = std::str::from_utf8(&stderr).unwrap();
    let actual_exit_code = status.code().unwrap_or_else(
        #[cfg(unix)]
        || {
            panic!(
                "process `{}` died with {status}",
                executable_filename.display(),
            )
        },
        #[cfg(not(unix))]
        || unreachable!("`ExitStatus::code()` can only be `None` on Unix"),
    );

    assert_eq!(
        expected_return_code, actual_exit_code,
        "test program did not exit with expected return code {expected_return_code}",
    );

    assert_eq!(
        stdout, expected_output,
        "output on stdout (left) did not match expected output (right)",
    );
    assert_eq!(stderr, "", "no output on stderr is expected");
}
