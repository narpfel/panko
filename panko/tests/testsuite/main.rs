#![feature(exit_status_error)]
#![feature(unqualified_local_imports)]

use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

use insta_cmd::assert_cmd_snapshot;
use insta_cmd::get_cargo_bin;
use rstest::rstest;

#[rstest]
#[case::scopes("scope", "scopes")]
#[case::typeck("typeck", "typeck")]
#[case::layout("layout", "layout")]
fn test(
    #[case] snapshot_name_prefix: &str,
    #[case] step: &str,
    #[files("tests/cases/**/test_*.c")]
    #[exclude("/test_nosnapshot_.*\\.c$")]
    #[exclude("/test_only_expand_.*\\.c$")]
    filename: PathBuf,
) {
    let filename = panko_compiletest::relative_to(
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
    panko_compiletest::execute_runtest(&filename);
}
