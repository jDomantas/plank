#![allow(dead_code)]

extern crate plank_errors;
extern crate plank_syntax;
extern crate plank_frontend;
extern crate plank_ir;
extern crate plank_interpreter;

mod test_parser;

use std::fs;
use std::io;
use std::io::prelude::*;
use plank_errors::reporter::Diagnostic;


fn build_code(source: &str) -> Result<plank_ir::Program, Vec<Diagnostic>> {
    let reporter = plank_errors::Reporter::new();
    let tokens = plank_syntax::lex(source, reporter.clone());
    let program = plank_syntax::parse(tokens, reporter.clone());
    plank_frontend::compile(&program, reporter.clone()).map_err(|()| {
        reporter.get_diagnostics()
    })
}

#[derive(Debug)]
enum TestResult {
    BuildFail(Vec<Diagnostic>),
    BadBuildPass(Vec<test_parser::Error>),
    BuildErrorMismatch(Vec<Diagnostic>, Vec<test_parser::Error>),
    IoMismatch { expected: Vec<u8>, got: Vec<u8> },
    MalformedTest(test_parser::ParseError),
    InterpreterExit(i32),
    InterpreterError(plank_interpreter::Error),
    Ok,
}

fn match_build_errors(expected: Vec<test_parser::Error>, got: Vec<Diagnostic>) -> TestResult {
    let mut unmatched = Vec::new();
    for err in expected {
        let mut matched = false;
        for actual in &got {
            if let Some(span) = actual.primary_span {
                if span.start.line == err.line && actual.message.contains(&err.message) {
                    matched = true;
                    break;
                }
            }
        }
        if !matched {
            unmatched.push(err);
        }
    }
    if unmatched.is_empty() {
        TestResult::Ok
    } else {
        TestResult::BuildErrorMismatch(got, unmatched)
    }
}

fn interpret_program(program: plank_ir::Program, input: Vec<u8>, output: Vec<u8>) -> TestResult {
    let mut input = ::std::io::Cursor::new(input);
    let mut actual_output = Vec::new();
    match plank_interpreter::run_program(&program, &mut input, &mut actual_output) {
        Ok(0) if actual_output == output => TestResult::Ok,
        Ok(0) => TestResult::IoMismatch { expected: output, got: actual_output },
        Ok(code) => TestResult::InterpreterExit(code),
        Err(e) => TestResult::InterpreterError(e),
    }
}

fn run_test(source: &str) -> TestResult {
    let expectation = match test_parser::parse_test(source) {
        Ok(e) => e,
        Err(e) => return TestResult::MalformedTest(e),
    };
    match expectation {
        test_parser::Expectation::BuildErrors(errors) => {
            match build_code(source) {
                Ok(_) => TestResult::BadBuildPass(errors),
                Err(got) => match_build_errors(errors, got),
            }
        }
        test_parser::Expectation::Io { input, output} => {
            match build_code(source) {
                Ok(program) => interpret_program(program, input, output),
                Err(e) => TestResult::BuildFail(e),
            }
        }
        test_parser::Expectation::BuildSuccess => {
            match build_code(source) {
                Ok(_) => TestResult::Ok,
                Err(e) => TestResult::BuildFail(e),
            }
        }
    }
}

fn run_tests() -> io::Result<Vec<(String, String, TestResult)>> {
    let mut test_results = Vec::new();
    for dir in TEST_DIRS {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            if !entry.file_type()?.is_file() {
                continue;
            }
            let mut file = fs::File::open(entry.path())?;
            let mut source = String::new();
            file.read_to_string(&mut source)?;
            let test_result = run_test(&source);
            let test_name = entry.path().display().to_string();
            if let TestResult::Ok = test_result {
                println!("test {} ... ok", test_name);
            } else {
                println!("test {} ... FAIL", test_name);
            }
            test_results.push((test_name, source, test_result));
        }
    }
    Ok(test_results)
}

fn print_expected_errors(errors: &[test_parser::Error]) {
    for err in errors {
        println!("at line {}: {}", err.line + 1, err.message);
    }
}

fn print_output(out: &[u8]) {
    for &byte in out {
        if byte < 32 || byte >= 127 || byte == b'\\' {
            print!("\\x{:>02X}", byte);
        } else {
            print!("{}", byte as char);
        }
    }
    println!();
}

fn report_results(results: &[(String, String, TestResult)]) {
    let mut passed = 0;
    for &(ref name, ref source, ref result) in results {
        match *result {
            TestResult::Ok => passed += 1,
            TestResult::BuildFail(ref diagnostics) => {
                println!("========================================");
                println!("test {}", name);
                println!("unexpected build failure");
                plank_errors::printer::print_diagnostics(source, diagnostics);
                println!();
            }
            TestResult::BadBuildPass(ref expected) => {
                println!("========================================");
                println!("test {}", name);
                println!("build passed but expected failure");
                print_expected_errors(expected);
            }
            TestResult::BuildErrorMismatch(ref got, ref expected) => {
                println!("========================================");
                println!("test {}", name);
                println!("build error mismatch");
                println!(">> unmatched errors:");
                print_expected_errors(expected);
                println!(">> actual errors:");
                plank_errors::printer::print_diagnostics(source, got);
                println!();
            }
            TestResult::IoMismatch { ref expected, ref got } => {
                println!("========================================");
                println!("test {}", name);
                println!("wrong output");
                print!("Expected: ");
                print_output(expected);
                print!("Got:      ");
                print_output(got);
                println!();
            }
            TestResult::MalformedTest(ref err) => {
                println!("========================================");
                println!("test {}", name);
                println!("malformed test");
                println!("{}", err);
                println!();
            }
            TestResult::InterpreterExit(code) => {
                println!("========================================");
                println!("test {}", name);
                println!("interpreted program exited with code {}", code);
                println!();
            }
            TestResult::InterpreterError(ref err) => {
                println!("========================================");
                println!("test {}", name);
                println!("interpreted program crashed");
                match *err {
                    plank_interpreter::Error::BadDeref => {
                        println!("invalid memory access");
                    }
                    plank_interpreter::Error::DivisionByZero => {
                        println!("division by zero");
                    }
                    plank_interpreter::Error::MissingSymbol(_) => {
                        println!("missing symbol definition");
                    }
                    plank_interpreter::Error::Io(ref err) => {
                        println!("io error: {}", err);
                    }
                }
                println!();
            }
        }
    }
    println!("========================================");
    println!("tests:  {}", results.len());
    println!("passed: {}", passed);
    println!("failed: {}", results.len() - passed);
}

fn main() {
    match run_tests() {
        Ok(results) => report_results(&results),
        Err(e) => println!("io error: {}", e),
    }
}

const TEST_DIRS: &'static [&'static str] = &[
    // test examples because obviously we want the
    // examples people come accross to build successfully
    "./examples",
    "./tests/compile-fail",
    "./tests/pass",
];
