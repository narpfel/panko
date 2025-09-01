#![feature(duration_millis_float)]
#![feature(exit_status_error)]
#![feature(gen_blocks)]
#![feature(internal_output_capture)]
#![feature(mpmc_channel)]
#![feature(result_option_map_or_default)]
#![feature(string_from_utf8_lossy_owned)]
#![feature(unqualified_local_imports)]

use std::borrow::Borrow;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::io;
use std::io::Write;
use std::io::set_output_capture;
use std::io::stdout;
use std::iter::repeat_n;
use std::num::NonZero;
use std::panic::AssertUnwindSafe;
use std::panic::catch_unwind;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::process::ExitStatus;
use std::process::Output;
use std::sync::Arc;
use std::sync::LazyLock;
use std::sync::Mutex;
use std::sync::mpmc;
use std::sync::mpsc;
use std::thread;
use std::time::Instant;

use insta_cmd::assert_cmd_snapshot;
use insta_cmd::get_cargo_bin;
use itertools::Itertools as _;
use regex::Captures;
use regex::Regex;

const FG_BOLD: &str = "\x1B[1m";
const FG_RED: &str = "\x1B[31m";
const FG_GREEN: &str = "\x1B[32m";
const FG_YELLOW: &str = "\x1B[33m";
const RESET: &str = "\x1B[m";

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

pub fn relative_to(path: &Path, target: impl AsRef<Path>) -> &Path {
    path.strip_prefix(target.as_ref()).unwrap()
}

fn expand_escape_sequences(s: &str) -> String {
    static EXPAND_ESCAPE_SEQUENCES_RE: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"\\[0\\n]").unwrap());

    EXPAND_ESCAPE_SEQUENCES_RE
        .replace_all(s, |captures: &Captures| {
            match captures.get(0).unwrap().as_str() {
                r"\0" => "\0",
                r"\\" => "\\",
                r"\n" => "\n",
                _ => unreachable!(),
            }
        })
        .into_owned()
}

struct Context;

trait TestFn {
    fn run(self: Box<Self>, context: &Context);
}

impl<F> TestFn for F
where
    F: FnOnce(&Context),
{
    fn run(self: Box<Self>, context: &Context) {
        self(context)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum TestResult {
    Success,
    Failure,
    XFail,
    XPass,
    Skip,
}

impl fmt::Display for TestResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Success => write!(f, "{FG_GREEN}.{RESET}"),
            Self::Failure => write!(f, "{FG_BOLD}{FG_RED}F{RESET}"),
            Self::XFail => write!(f, "{FG_BOLD}{FG_YELLOW}x{RESET}"),
            Self::XPass => write!(f, "{FG_RED}X{RESET}"),
            Self::Skip => write!(f, "{FG_YELLOW}s{RESET}"),
        }
    }
}

enum ExpectedResult {
    Success,
    #[expect(dead_code)]
    Failure,
}

#[derive(Default)]
struct Outcomes {
    outcomes: HashMap<TestResult, Vec<(String, String)>>,
}

impl Outcomes {
    fn add(&mut self, case: String, result: TestResult, output: String) {
        self.outcomes
            .entry(result)
            .or_default()
            .push((case, output))
    }

    fn get(&self, result: TestResult) -> &[(String, String)] {
        self.outcomes.get(&result).map_or_default(Vec::as_slice)
    }
}

struct TestCase {
    name: String,
    test_fn: Box<dyn TestFn + Send>,
    expected_result: ExpectedResult,
}

struct OutputCapture {
    old: Option<Arc<Mutex<Vec<u8>>>>,
    output: Arc<Mutex<Vec<u8>>>,
}

impl OutputCapture {
    fn new() -> Self {
        let output = Arc::new(Mutex::new(Vec::new()));
        Self {
            old: set_output_capture(Some(Arc::clone(&output))),
            output,
        }
    }

    fn get(&self) -> Vec<u8> {
        std::mem::take(&mut self.output.lock().unwrap())
    }
}

impl Drop for OutputCapture {
    fn drop(&mut self) {
        set_output_capture(self.old.take());
    }
}

impl TestCase {
    fn run(self) -> (String, TestResult, Vec<u8>) {
        let Self { name, test_fn, expected_result } = self;
        let context = Context;
        let output_capture = OutputCapture::new();
        let result = match catch_unwind(AssertUnwindSafe(|| test_fn.run(&context))) {
            Ok(()) => match expected_result {
                ExpectedResult::Success => TestResult::Success,
                ExpectedResult::Failure => TestResult::XPass,
            },
            Err(_panic_payload) => match expected_result {
                ExpectedResult::Success => TestResult::Failure,
                ExpectedResult::Failure => TestResult::XFail,
            },
        };
        let output = output_capture.get();
        (name, result, output)
    }
}

pub fn execute_runtest(filename: impl AsRef<Path>) {
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

    let expected_print_re = Regex::new(r"(?m)^\s*// \[\[print: (?P<output>.*?)\]\]$").unwrap();
    let expected_output: String = expected_print_re
        .captures_iter(&source)
        .map(|captures| {
            expand_escape_sequences(&(captures.name("output").unwrap().as_str().to_string() + "\n"))
        })
        .collect();

    let cmdline_arguments_re = Regex::new(r"(?m)^// \[\[arg: (?P<arg>.*?)\]\]$").unwrap();

    let cmdline_arguments = cmdline_arguments_re
        .captures_iter(&source)
        .map(|captures| expand_escape_sequences(captures.name("arg").unwrap().as_str()))
        .collect_vec();

    let filename = std::fs::canonicalize(filename).unwrap();
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
        .env("CLICOLOR_FORCE", "1")
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

    pretty_assertions::assert_eq!(
        expected_output,
        stdout,
        "expected output (left) did not match output on stdout (right)",
    );
    assert_eq!("", stderr, "no output on stderr is expected");
}

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

gen fn test_cases_from_filename(filename: PathBuf) -> TestCase {
    let components: HashSet<_> = filename
        .iter()
        .filter_map(|c| Some(c.to_str()?.to_owned()))
        .collect();

    if components.contains("execute") {
        let filename = filename.clone();
        yield TestCase {
            name: format!("execute::{}", filename.display()),
            test_fn: Box::new(move |_context: &Context| execute_runtest(filename)),
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

fn discover(pattern: &str) -> impl Iterator<Item = TestCase> {
    glob::glob(pattern).unwrap().flat_map(|filename| {
        let filename = filename.unwrap();
        test_cases_from_filename(filename)
    })
}

fn run_tests() {
    let start = Instant::now();

    let cases = discover("tests/cases/**/test_*.c").collect_vec();
    let case_count = cases.len();
    let digit_count = usize::try_from(case_count.ilog10() + 1).unwrap();
    println!("\nrunning {} tests", cases.len());

    let (tx, test_case_receiver) = mpmc::channel();
    for case in cases {
        tx.send(case).unwrap();
    }
    drop(tx);

    let (result_sender, result_receiver) = mpsc::channel();

    let runners = repeat_n(
        (test_case_receiver, result_sender),
        thread::available_parallelism().map_or(1, NonZero::get),
    )
    .map(|(test_case_receiver, result_sender)| {
        thread::spawn(move || {
            while let Ok(case) = test_case_receiver.recv() {
                result_sender.send(case.run()).unwrap();
            }
        })
    })
    .collect_vec();

    let chunk_size = 87;
    let mut outcomes = Outcomes::default();

    for (i, (name, result, output)) in result_receiver.iter().enumerate() {
        print!("{result}");
        outcomes.add(name, result, String::from_utf8_lossy_owned(output));
        stdout().flush().unwrap();
        let i = i + 1;
        if i.is_multiple_of(chunk_size) {
            println!(" {i:>digit_count$}/{case_count}");
        }
    }
    println!();

    for runner in runners {
        runner.join().unwrap();
    }

    let time = start.elapsed();

    let failures = outcomes.get(TestResult::Failure);
    let status = if failures.is_empty() {
        format!("{FG_GREEN}ok{RESET}")
    }
    else {
        format!("{FG_RED}FAILED{RESET}")
    };

    if !failures.is_empty() {
        println!("\nfailures:\n");
        for (name, output) in failures {
            println!("---- {name} stdout ----");
            println!("{output}")
        }

        println!("\nfailures:");
        for (name, _) in failures {
            println!("    {name}");
        }
        println!();
    }

    println!(
        "test result: {status}. {passed} passed; {failed} failed; {xfailed} xfailed; \
        {xpassed} xpassed; {skipped} skipped; finished in {:.2}s\n",
        time.as_millis_f64() / 1_000.0,
        passed = outcomes.get(TestResult::Success).len(),
        failed = outcomes.get(TestResult::Failure).len(),
        xfailed = outcomes.get(TestResult::XFail).len(),
        xpassed = outcomes.get(TestResult::XPass).len(),
        skipped = outcomes.get(TestResult::Skip).len(),
    );
}

pub fn run() {
    color_eyre::install().unwrap();

    match catch_unwind(run_tests) {
        Ok(()) => {}
        Err(panic) => eprintln!(
            "test setup panicked with {}",
            panic.downcast_ref::<&str>().unwrap(),
        ),
    }
}
