#![feature(duration_millis_float)]
#![feature(exit_status_error)]
#![feature(internal_output_capture)]
#![feature(mpmc_channel)]
#![feature(result_option_map_or_default)]
#![feature(string_from_utf8_lossy_owned)]
#![feature(unqualified_local_imports)]

use std::borrow::Borrow;
use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::io;
use std::io::Write;
use std::io::set_output_capture;
use std::io::stdout;
use std::iter::repeat_n;
use std::num::NonZero;
use std::os::unix::process::ExitStatusExt;
use std::panic::AssertUnwindSafe;
use std::panic::catch_unwind;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::process::ExitCode;
use std::process::ExitStatus;
use std::process::Output;
use std::sync::Arc;
use std::sync::LazyLock;
use std::sync::Mutex;
use std::sync::mpmc;
use std::sync::mpsc;
use std::thread;
use std::time::Instant;

use insta_cmd::get_cargo_bin;
use itertools::Itertools as _;
use regex::Captures;
use regex::Regex;
use regex::bytes;

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

pub struct Context {
    expected_result: RefCell<ExpectedResult>,
    expects_failure: Cell<bool>,
}

impl Context {
    fn new(expected_result: ExpectedResult) -> Self {
        Self {
            expected_result: RefCell::new(expected_result),
            expects_failure: Cell::new(false),
        }
    }

    fn expect_failure(&self) {
        self.expects_failure.set(true);
    }

    fn should_panic_with_output(&self, regex: &str) {
        let mut expected_result = self.expected_result.borrow_mut();
        let regexes = match &mut *expected_result {
            result @ ExpectedResult::Success => {
                *result = ExpectedResult::ShouldPanic(Vec::new());
                let ExpectedResult::ShouldPanic(regexes) = &mut *result
                else {
                    unreachable!()
                };
                regexes
            }
            ExpectedResult::ShouldPanic(regexes) => regexes,
        };
        regexes.push(bytes::Regex::new(regex).unwrap());
    }
}

pub trait TestFn {
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
            Self::XPass => write!(f, "{FG_BOLD}{FG_RED}X{RESET}"),
            Self::Skip => write!(f, "{FG_YELLOW}s{RESET}"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExpectedResult {
    Success,
    ShouldPanic(Vec<bytes::Regex>),
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

    fn read(&self) -> Vec<u8> {
        self.output.lock().unwrap().clone()
    }

    fn get(self) -> Vec<u8> {
        std::mem::take(&mut self.output.lock().unwrap())
    }
}

impl Drop for OutputCapture {
    fn drop(&mut self) {
        set_output_capture(self.old.take());
    }
}

fn check_should_panic(name: &str, output: &[u8], expecteds: &[bytes::Regex]) -> TestResult {
    for expected in expecteds {
        if !expected.is_match(output) {
            eprintln!("{FG_BOLD}`{name}`{RESET}: output did not match `{expected:?}`");
            return TestResult::Failure;
        }
    }
    TestResult::Success
}

pub struct TestCase {
    pub name: String,
    pub test_fn: Box<dyn TestFn + Send>,
    pub expected_result: ExpectedResult,
}

impl TestCase {
    fn run(self) -> (String, TestResult, Vec<u8>) {
        let Self { name, test_fn, expected_result } = self;
        let context = Context::new(expected_result);
        let output_capture = OutputCapture::new();
        let result = catch_unwind(AssertUnwindSafe(|| {
            test_fn.run(&context);
            if context.expects_failure.get() {
                eprintln!("test {FG_BOLD}`{name}`{RESET} was marked xfail but passed");
            }
        }));
        let expected_result = &*context.expected_result.borrow();
        let expects_failure = context.expects_failure.get();
        let result = match result {
            Ok(()) => match (expected_result, expects_failure) {
                (ExpectedResult::Success, false) => TestResult::Success,
                (ExpectedResult::Success, true) => TestResult::XPass,
                (ExpectedResult::ShouldPanic(_), false) => TestResult::Failure,
                (ExpectedResult::ShouldPanic(_), true) => TestResult::XFail,
            },
            Err(_panic_payload) => match (expected_result, expects_failure) {
                (ExpectedResult::Success, false) => TestResult::Failure,
                (ExpectedResult::Success, true) => TestResult::XFail,
                (ExpectedResult::ShouldPanic(expected_messages), _) => {
                    let result =
                        check_should_panic(&name, &output_capture.read(), expected_messages);
                    match expects_failure {
                        false => result,
                        true => match result {
                            TestResult::Success => TestResult::XPass,
                            TestResult::Failure => TestResult::XFail,
                            _ => unreachable!(),
                        },
                    }
                }
            },
        };
        (name, result, output_capture.get())
    }
}

type AssertSignal<'a> = Box<dyn FnOnce(&Path, ExitStatus) + 'a>;

fn make_exit_with_signal_assertion(source: &str) -> AssertSignal {
    let signal_re = Regex::new(r"(?m)^// \[\[signal: SIG(?P<signal>.*?)\]\]$").unwrap();
    let signals = signal_re
        .captures_iter(source)
        .map(|captures| captures.name("signal").unwrap().as_str())
        .collect_vec();

    match signals.into_iter().at_most_one() {
        Ok(None) => Box::new(|executable_filename, status| {
            panic!(
                "process `{}` died with {status}",
                executable_filename.display(),
            )
        }),
        Ok(Some(expected_signal_name)) => {
            let signal_names_table = Command::new("kill").arg("--table").output().unwrap().stdout;
            let signal_names_table = str::from_utf8(&signal_names_table)
                .unwrap()
                .split_whitespace()
                .collect_vec();
            let signal_index = signal_names_table
                .iter()
                .position(|&name| name == expected_signal_name)
                .unwrap();
            let expected_signal: i32 = signal_names_table[signal_index - 1].parse().unwrap();

            Box::new(move |_, status| {
                let actual_signal = status.signal().unwrap();
                assert_eq!(
                    expected_signal, actual_signal,
                    "test program did not exit with expected signal SIG{expected_signal_name} ({expected_signal})",
                )
            })
        }
        Err(signals) => panic!(
            "test source invalid: more than one signal specified: {:?}",
            signals.collect_vec(),
        ),
    }
}

pub fn execute_runtest(context: &Context, test_name: &Path, filenames: Vec<PathBuf>) {
    let source: String = filenames
        .iter()
        .map(std::fs::read_to_string)
        .collect::<Result<Vec<_>, _>>()
        .unwrap()
        .join("\n");

    let known_bug_re = Regex::new(r"(?m)^// \[\[known-bug\]\]$").unwrap();
    let is_known_bug = known_bug_re.is_match(&source);
    if is_known_bug {
        context.expect_failure();
    }

    let compile_error_re =
        Regex::new(r"(?m)^// \[\[compile-error: (?P<error_message>.*?)\]\]$").unwrap();
    let compile_error_messages = compile_error_re
        .captures_iter(&source)
        .map(|captures| captures.name("error_message").unwrap().as_str());
    for error_message in compile_error_messages {
        context.should_panic_with_output(error_message);
    }

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

    let assert_exit_with_signal = make_exit_with_signal_assertion(&source);

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

    let filenames = filenames
        .into_iter()
        .map(|filename| {
            relative_to(
                &std::fs::canonicalize(filename).unwrap(),
                Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap(),
            )
            .to_owned()
        })
        .collect_vec();

    let output_dir = tempfile::tempdir().unwrap();
    let executable_filename = output_dir
        .path()
        .join(test_name.file_name().unwrap())
        .with_extension("");

    Command::new(get_cargo_bin("panko"))
        .env("CLICOLOR_FORCE", "1")
        .current_dir("..")
        .args(filenames)
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
    match status.code() {
        Some(actual_exit_code) => assert_eq!(
            expected_return_code, actual_exit_code,
            "test program did not exit with expected return code {expected_return_code}",
        ),
        None => assert_exit_with_signal(&executable_filename, status),
    }

    pretty_assertions::assert_eq!(
        expected_output,
        stdout,
        "expected output (left) did not match output on stdout (right)",
    );
    assert_eq!("", stderr, "no output on stderr is expected");
}

#[must_use]
fn run_tests(cases: impl Iterator<Item = TestCase>) -> ExitCode {
    let start = Instant::now();

    let cases = cases.collect_vec();
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

    let (failures, xpassed) = (
        outcomes.get(TestResult::Failure),
        outcomes.get(TestResult::XPass),
    );
    let is_failed = !failures.is_empty() || !xpassed.is_empty();
    let status = match is_failed {
        true => format!("{FG_RED}FAILED{RESET}"),
        false => format!("{FG_GREEN}ok{RESET}"),
    };

    if is_failed {
        println!("\nfailures:\n");
        let failures = failures.iter().chain(xpassed).collect_vec();
        for (name, output) in &failures {
            println!("---- {name} stdout ----");
            println!("{output}")
        }

        println!("\nfailures:");
        for (name, _) in &failures {
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

    match is_failed {
        true => ExitCode::FAILURE,
        false => ExitCode::SUCCESS,
    }
}

#[must_use]
pub fn run(cases: impl Iterator<Item = TestCase>) -> ExitCode {
    color_eyre::install().unwrap();

    run_tests(cases)
}
