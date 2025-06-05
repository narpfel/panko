#![feature(duration_millis_float)]
#![feature(exit_status_error)]
#![feature(internal_output_capture)]
#![feature(unqualified_local_imports)]

use std::borrow::Borrow;
use std::fmt;
use std::io;
use std::io::Write;
use std::io::set_output_capture;
use std::io::stdout;
use std::panic::AssertUnwindSafe;
use std::panic::catch_unwind;
use std::path::Path;
use std::process::Command;
use std::process::ExitStatus;
use std::process::Output;
use std::sync::Arc;
use std::sync::LazyLock;
use std::sync::Mutex;
use std::time::Instant;

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

enum TestResult {
    Success,
    Failure,
    XFail,
    XPass,
    #[expect(dead_code)]
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

fn discover(pattern: &str) -> impl Iterator<Item = TestCase> {
    glob::glob(pattern).unwrap().map(|filename| {
        let filename = filename.unwrap();
        TestCase {
            name: filename.display().to_string(),
            test_fn: Box::new(move |_context: &Context| execute_runtest(filename)),
            expected_result: ExpectedResult::Success,
        }
    })
}

fn run_tests() {
    let cases = discover("tests/cases/execute/**/test_*.c").collect_vec();
    let case_count = cases.len();
    let digit_count = usize::try_from(case_count.ilog10() + 1).unwrap();
    println!("running {} tests ({digit_count})", cases.len());

    let start = Instant::now();
    let chunk_size = 87;
    let chunks = cases.into_iter().chunks(chunk_size);
    let mut failures = Vec::new();

    for (i, chunk) in chunks.into_iter().enumerate() {
        let chunk = chunk.collect_vec();
        let chunk_len = chunk.len();
        for case in chunk {
            let (name, result, output) = case.run();
            print!("{result}");
            if matches!(result, TestResult::Failure) {
                failures.push((name, output));
            }
            stdout().flush().unwrap();
        }
        if chunk_len == chunk_size {
            println!(" {:>digit_count$}/{case_count}", (i + 1) * chunk_size);
        }
        else {
            println!();
        }
    }
    let time = start.elapsed();

    let status = if failures.is_empty() {
        format!("{FG_GREEN}ok{RESET}")
    }
    else {
        format!("{FG_RED}FAILED{RESET}")
    };

    if !failures.is_empty() {
        println!("\nfailures:\n");
        for (name, output) in &failures {
            println!("---- {name} stdout ----");
            println!("{}", String::from_utf8_lossy(output));
        }

        println!("\nfailures:");
        for (name, _) in &failures {
            println!("    {name}");
        }
        println!();
    }

    println!(
        "test result: {status}, ?? passed, ?? failed, ?? xfailed, ?? xpassed, ?? skipped; finished in {:.2}s",
        time.as_millis_f64() / 1_000.0,
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
