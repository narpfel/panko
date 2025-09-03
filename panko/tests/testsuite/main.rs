#![feature(gen_blocks)]
#![feature(unqualified_local_imports)]

use std::collections::HashSet;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

use insta_cmd::assert_cmd_snapshot;
use insta_cmd::get_cargo_bin;
use panko_compiletest::Context;
use panko_compiletest::ExpectedResult;
use panko_compiletest::TestCase;
use panko_compiletest::execute_runtest;
use panko_compiletest::relative_to;

fn execute_step_test(filename: impl AsRef<Path>, snapshot_name_prefix: &str, step: &str) {
    let filename = std::fs::canonicalize(filename).unwrap();
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

fn test_cases_from_filename(filename: PathBuf) -> impl Iterator<Item = TestCase> {
    let components: HashSet<_> = filename
        .iter()
        .filter_map(|c| Some(c.to_str()?.to_owned()))
        .collect();

    gen move {
        if components.contains("execute") {
            let filename = filename.clone();
            yield TestCase {
                name: format!("execute::{}", filename.display()),
                test_fn: Box::new(move |context: &Context| execute_runtest(context, filename)),
                expected_result: ExpectedResult::Success,
            };
        }
        if components.contains("preprocessor") {
            let filename = filename.clone();
            yield TestCase {
                name: format!("preprocessor::{}", filename.display()),
                test_fn: Box::new(move |_context: &Context| {
                    execute_step_test(filename, "preprocess", "preprocess")
                }),
                expected_result: ExpectedResult::Success,
            };
        }
        for (snapshot_name_prefix, step) in [
            ("scope", "scopes"),
            ("typeck", "typeck"),
            ("layout", "layout"),
        ] {
            if let Some(name) = filename.file_name()
                && let Some(name) = name.to_str()
                && !(name.starts_with("test_nosnapshot_") || name.starts_with("test_only_expand_"))
            {
                let filename = filename.clone();
                yield TestCase {
                    name: format!("{step}::{}", filename.display()),
                    test_fn: Box::new(move |_context: &Context| {
                        execute_step_test(filename, snapshot_name_prefix, step)
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

fn main() {
    panko_compiletest::run(discover("tests/cases/**/test_*.c"))
}
