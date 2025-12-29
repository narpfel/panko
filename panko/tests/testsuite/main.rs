#![feature(gen_blocks)]
#![feature(iterator_try_collect)]
#![feature(unqualified_local_imports)]

use std::collections::HashSet;
use std::ffi::OsStr;
use std::fs::read_to_string;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::process::ExitCode;
use std::sync::LazyLock;

use insta_cmd::assert_cmd_snapshot;
use insta_cmd::get_cargo_bin;
use itertools::Itertools as _;
use panko_compiletest::Context;
use panko_compiletest::ExpectedResult;
use panko_compiletest::TestCase;
use panko_compiletest::execute_runtest;
use panko_compiletest::relative_to;
use regex::Regex;

static NOSNAPSHOT_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?m)^// \[\[nosnapshot\]\]$").unwrap());
static PREPROCESSOR_ONLY_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?m)^// \[\[preprocessor-only\]\]$").unwrap());

fn any_is_match(re: &Regex, sources: &[String]) -> bool {
    sources.iter().any(|source| re.is_match(source))
}

fn execute_step_test(
    test_name: &Path,
    filenames: Vec<PathBuf>,
    snapshot_name_prefix: &str,
    step: &str,
) {
    let test_name = relative_to(
        &std::fs::canonicalize(test_name).unwrap(),
        Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap(),
    )
    .to_owned();
    let filenames = filenames.into_iter().map(|filename| {
        relative_to(
            &std::fs::canonicalize(filename).unwrap(),
            Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap(),
        )
        .to_owned()
    });
    assert_cmd_snapshot!(
        format!("{snapshot_name_prefix}-{}", test_name.display()),
        Command::new(get_cargo_bin("panko"))
            .current_dir("..")
            .arg(format!("--print={step}"))
            .arg(format!("--stop-after={step}"))
            .args(filenames),
    );
}

fn make_snapshot_test_case(
    path: &Path,
    filenames: &[PathBuf],
    snapshot_name_prefix: &'static str,
    step: &'static str,
) -> TestCase {
    let path = path.to_owned();
    let filenames = filenames.to_owned();
    TestCase {
        name: format!("{step}::{}", path.display()),
        test_fn: Box::new(move |_context: &Context| {
            execute_step_test(&path, filenames, snapshot_name_prefix, step)
        }),
        expected_result: ExpectedResult::Success,
    }
}

fn test_cases_from_filename(path: PathBuf) -> impl Iterator<Item = TestCase> {
    let components: HashSet<_> = path
        .iter()
        .filter_map(|c| Some(c.to_str()?.to_owned()))
        .collect();

    gen move {
        let filenames = if path.is_dir() {
            glob::glob(&format!(
                "{}/*.c",
                glob::Pattern::escape(&path.display().to_string())
            ))
            .unwrap()
            .try_collect()
            .unwrap()
        }
        else if path.extension() == Some(OsStr::new("c")) {
            vec![path.clone()]
        }
        else {
            unreachable!(
                "test case `{path:?}` is neither a single-file test case nor a directory test case",
            );
        };

        assert!(!filenames.is_empty());

        let file_contents = filenames
            .iter()
            .map(|filename| read_to_string(filename).unwrap())
            .collect_vec();

        if components.contains("execute") {
            let path = path.clone();
            let filenames = filenames.clone();
            yield TestCase {
                name: format!("execute::{}", path.display()),
                test_fn: Box::new(move |context: &Context| {
                    execute_runtest(context, &path, filenames)
                }),
                expected_result: ExpectedResult::Success,
            };
        }

        if any_is_match(&NOSNAPSHOT_RE, &file_contents) {
            return;
        }

        if components.contains("preprocessor") {
            yield make_snapshot_test_case(&path, &filenames, "preprocess", "preprocess");
        }

        if any_is_match(&PREPROCESSOR_ONLY_RE, &file_contents) {
            return;
        }

        for (snapshot_name_prefix, step) in [
            ("scope", "scopes"),
            ("typeck", "typeck"),
            ("layout", "layout"),
        ] {
            yield make_snapshot_test_case(&path, &filenames, snapshot_name_prefix, step);
        }
    }
}

fn discover(pattern: &str) -> impl Iterator<Item = TestCase> {
    glob::glob(pattern).unwrap().flat_map(|filename| {
        let filename = filename.unwrap();
        test_cases_from_filename(filename)
    })
}

fn main() -> ExitCode {
    panko_compiletest::run(discover("tests/cases/**/test_*"))
}
