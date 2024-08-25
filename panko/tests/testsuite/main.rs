use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

use insta_cmd::assert_cmd_snapshot;
use insta_cmd::get_cargo_bin;
use rstest::rstest;

fn relative_to(path: &Path, target: impl AsRef<Path>) -> &Path {
    path.strip_prefix(target.as_ref()).unwrap()
}

#[rstest]
#[case::scopes("scope", "scopes")]
#[case::typeck("typeck", "typeck")]
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
