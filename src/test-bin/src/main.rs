#![allow(dead_code)]

extern crate clap;
extern crate errors;
extern crate syntax;

use std::convert::From;
use std::io;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use errors::reporter::Diagnostic;


#[derive(Debug)]
enum Error {
    Io(io::Error),
    Build(Vec<Diagnostic>),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}

#[derive(Debug)]
enum Command {
    Lex,
    Parse,
}

#[derive(Debug)]
enum Stream {
    File(PathBuf),
    Std,
}

#[derive(Debug)]
struct Params {
    command: Command,
    input: Stream,
    output: Stream,
}

type Result<T> = ::std::result::Result<T, Error>;

fn main() {
    match run() {
        Ok(()) => {}
        Err(Error::Build(_)) => {
            ::std::process::exit(1);
        }
        Err(Error::Io(err)) => {
            println!("IO error:\n{}", err);
            ::std::process::exit(1);
        }
    }
}

fn run() -> Result<()> {
    let params = parse_params()?;
    let input = read_input(&params.input)?;
    let result = match params.output {
        Stream::Std => {
            let stdout = io::stdout();
            let stdout = stdout.lock();
            run_command(&input, &params.command, stdout)
        }
        Stream::File(ref name) => {
            let file = ::std::fs::File::create(name)?;
            run_command(&input, &params.command, file)
        }
    };
    if let Err(Error::Build(ref diagnostics)) = result {
        errors::print_diagnostics(&input, &diagnostics);
    }
    result
}

fn run_command<W: Write>(input: &str, command: &Command, mut output: W) -> Result<()> {
    match *command {
        Command::Lex => {
            let tokens = lex(&input)?;
            for tok in &tokens {
                output.write_fmt(format_args!("{:?}\n", tok))?;
            }
            Ok(())
        }
        Command::Parse => {
            let _ = parse(&input)?;
            output.write(b"Program parsed successfully")?;
            Ok(())
        }
    }
}

fn parse_params() -> Result<Params> {
    use clap::{App, Arg};

    let matches = App::new("Plank compiler")
        .arg(Arg::with_name("lex")
            .long("lex")
            .help("List tokens in input")
            .conflicts_with_all(&["parse"]))
        .arg(Arg::with_name("parse")
            .long("parse")
            .help("Parse input")
            .conflicts_with_all(&["lex"]))
        .arg(Arg::with_name("input")
            .index(1)
            .help("Set input file, uses stdin if none provided"))
        .arg(Arg::with_name("output")
            .short("o")
            .long("output")
            .takes_value(true)
            .help("Set output file, uses stdout if none provided"))
        .get_matches();
    let command = if matches.is_present("lex") {
        Command::Lex
    } else if matches.is_present("parse") {
        Command::Parse
    } else {
        Command::Parse
    };

    let input = match matches.value_of_os("input") {
        Some(path) => Stream::File(Path::new(path).to_owned()),
        None => Stream::Std,
    };

    let output = match matches.value_of_os("output") {
        Some(path) => Stream::File(Path::new(path).to_owned()),
        None => Stream::Std,
    };

    Ok(Params {
        command,
        input,
        output,
    })
}

fn read_file(name: &Path) -> Result<String> {
    use std::fs::File;
    let mut file = File::open(name)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn read_stdin() -> Result<String> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let mut input = String::new();
    stdin.read_to_string(&mut input)?;
    Ok(input)
}

fn read_input(stream: &Stream) -> Result<String> {
    match *stream {
        Stream::File(ref file) => read_file(file),
        Stream::Std => read_stdin(),
    }
}

fn lex(source: &str) -> Result<Vec<syntax::tokens::Token>> {
    let reporter = errors::Reporter::new();
    let tokens = syntax::lex(source, reporter.clone());
    let diagnostics = reporter.get_diagnostics();
    if diagnostics.len() == 0 {
        Ok(tokens
            .into_iter()
            .map(syntax::position::Spanned::into_value)
            .collect())
    } else {
        Err(Error::Build(diagnostics))
    }
}

fn parse(source: &str) -> Result<syntax::ast::Program> {
    // don't reuse lex function, because we wan't to merge lex and parse errors
    let reporter = errors::Reporter::new();
    let tokens = syntax::lex(source, reporter.clone());
    let program = syntax::parse(tokens, reporter.clone());
    let diagnostics = reporter.get_diagnostics();
    if diagnostics.len() == 0 {
        Ok(program)
    } else {
        Err(Error::Build(diagnostics))
    }
}
