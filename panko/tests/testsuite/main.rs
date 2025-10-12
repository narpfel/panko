#![feature(gen_blocks)]
#![feature(iterator_try_collect)]
#![feature(unqualified_local_imports)]

use std::collections::HashSet;
use std::ffi::OsStr;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::process::ExitCode;

use insta_cmd::assert_cmd_snapshot;
use insta_cmd::get_cargo_bin;
use panko_compiletest::Context;
use panko_compiletest::ExpectedResult;
use panko_compiletest::TestCase;
use panko_compiletest::execute_runtest;
use panko_compiletest::relative_to;

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
        if components.contains("preprocessor") {
            let path = path.clone();
            let filenames = filenames.clone();
            yield TestCase {
                name: format!("preprocessor::{}", path.display()),
                test_fn: Box::new(move |_context: &Context| {
                    execute_step_test(&path, filenames, "preprocess", "preprocess")
                }),
                expected_result: ExpectedResult::Success,
            };
        }
        for (snapshot_name_prefix, step) in [
            ("scope", "scopes"),
            ("typeck", "typeck"),
            ("layout", "layout"),
        ] {
            if let Some(name) = path.file_name()
                && let Some(name) = name.to_str()
                && !(name.starts_with("test_nosnapshot_") || name.starts_with("test_only_expand_"))
            {
                let path = path.clone();
                let filenames = filenames.clone();
                yield TestCase {
                    name: format!("{step}::{}", path.display()),
                    test_fn: Box::new(move |_context: &Context| {
                        execute_step_test(&path, filenames, snapshot_name_prefix, step)
                    }),
                    expected_result: ExpectedResult::Success,
                }
            }
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
