#![feature(duration_millis_float)]
#![feature(internal_output_capture)]
#![feature(unqualified_local_imports)]

use std::fmt;
use std::io::Write;
use std::io::set_output_capture;
use std::io::stdout;
use std::panic::AssertUnwindSafe;
use std::panic::catch_unwind;
use std::sync::Arc;
use std::sync::Mutex;
use std::time::Instant;

use itertools::Itertools as _;

const FG_BOLD: &str = "\x1B[1m";
const FG_RED: &str = "\x1B[31m";
const FG_GREEN: &str = "\x1B[32m";
const FG_YELLOW: &str = "\x1B[33m";
const RESET: &str = "\x1B[m";

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

fn run_tests() {
    let mut cases = vec![];
    for filename in glob::glob("tests/cases/execute/**/test_*.c").unwrap() {
        cases.push(TestCase {
            name: filename.as_deref().unwrap().to_string_lossy().into_owned(),
            test_fn: Box::new(move |_context: &Context| {
                // TODO: actually test something
            }),
            expected_result: ExpectedResult::Success,
        });
    }
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
