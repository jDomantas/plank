#![allow(dead_code)]

extern crate plank_errors;
extern crate plank_syntax;
extern crate plank_frontend;
extern crate plank_ir;
extern crate plank_interpreter;

mod test_parser;

use std::fs;
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

fn main() {
    for dir in TEST_DIRS {
        let entries = fs::read_dir(dir).expect("failed to read dir");
        for entry in entries {
            let entry = entry.unwrap();
            if !entry.file_type().unwrap().is_file() {
                continue;
            }
            let mut file = fs::File::open(entry.path()).unwrap();
            let mut contents = String::new();
            file.read_to_string(&mut contents).unwrap();
            let test_result = run_test(&contents);
            if let TestResult::Ok = test_result {
                println!("test {} ... ok", entry.path().display());
            } else {
                println!("test {} ... FAIL", entry.path().display());
                println!("{:?}", test_result);
            }
        }
    }
}

const TEST_DIRS: &'static [&'static str] = &[
    "./examples",
    "./tests/compile-fail",
    "./tests/pass",
];
