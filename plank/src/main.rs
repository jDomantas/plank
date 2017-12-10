extern crate clap;
extern crate plank_errors;
extern crate plank_syntax;
extern crate plank_frontend;
extern crate plank_ir;
extern crate plank_interpreter;
extern crate plank_x86_backend;

mod ast_printer;

use std::convert::From;
use std::io;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use plank_errors::Reporter;


#[derive(Debug)]
enum Error {
    Io(io::Error),
    BuildFail,
    Interpreter(plank_interpreter::Error),
    InterpreterExit(i32),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}

impl From<plank_interpreter::Error> for Error {
    fn from(err: plank_interpreter::Error) -> Error {
        Error::Interpreter(err)
    }
}

#[derive(Debug)]
enum Command {
    Lex,
    Parse,
    EmitIr,
    Interpret,
    CompileX86,
}

#[derive(Debug)]
enum Stream {
    File(PathBuf),
    Std,
}

#[derive(Debug)]
struct Params {
    command: Command,
    optimize: bool,
    skip_prelude: bool,
    input: Stream,
    output: Stream,
}

type Result<T> = ::std::result::Result<T, Error>;

fn main() {
    match run() {
        Ok(()) => {}
        Err(Error::BuildFail) => {
            eprintln!("error: build failed");
            ::std::process::exit(1);
        }
        Err(Error::Io(err)) => {
            eprintln!("IO error:\n{}", err);
            ::std::process::exit(1);
        }
        Err(Error::Interpreter(ref err)) => {
            eprintln!("Interpreter failed:\n{}", err);
            ::std::process::exit(1);
        }
        Err(Error::InterpreterExit(code)) => {
            eprintln!("Interpreter exited with status code {}", code);
            ::std::process::exit(code);
        }
    }
}

fn run() -> Result<()> {
    let params = parse_params()?;
    let input = read_input(&params.input)?;
    match params.output {
        Stream::Std => {
            let stdout = io::stdout();
            let stdout = stdout.lock();
            run_command(&input, &params.command, params.optimize, params.skip_prelude, stdout)
        }
        Stream::File(ref name) => {
            let file = ::std::fs::File::create(name)?;
            run_command(&input, &params.command, params.optimize, params.skip_prelude, file)
        }
    }
}

fn run_command<W: Write>(input: &str, command: &Command, optimize: bool, skip_prelude: bool, output: W) -> Result<()> {
    match *command {
        Command::Lex => lex(input, output),
        Command::Parse => parse(input, output),
        Command::EmitIr => emit_ir(input, output, optimize),
        Command::Interpret => interpret(input, output, optimize),
        Command::CompileX86 => compile_x86(input, output, optimize, skip_prelude),
    }
}

fn parse_params() -> Result<Params> {
    use clap::{App, Arg};

    let matches = App::new("Plank compiler")
        .arg(Arg::with_name("lex")
            .long("lex")
            .help("List tokens in input")
            .conflicts_with_all(&["parse", "emit-ir", "interpret", "emit-asm"]))
        .arg(Arg::with_name("parse")
            .long("parse")
            .help("Parse input")
            .conflicts_with_all(&["lex", "emit-ir", "interpret", "emit-asm"]))
        .arg(Arg::with_name("emit-ir")
            .long("emit-ir")
            .help("Compile to plank IR")
            .conflicts_with_all(&["lex", "parse", "interpret", "emit-asm"]))
        .arg(Arg::with_name("interpret")
            .long("interpret")
            .help("Compile to IR and interpret")
            .conflicts_with_all(&["lex", "parse", "emit-ir", "emit-asm"]))
        .arg(Arg::with_name("emit-asm")
            .long("emit-asm")
            .help("Compile to x86 assembly")
            .conflicts_with_all(&["lex", "parse", "emit-ir", "interpret"]))
        .arg(Arg::with_name("optimize")
            .long("optimize")
            .short("O")
            .help("Perform optimizations on IR"))
        .arg(Arg::with_name("no-prelude")
            .long("no-prelude")
            .help("Don't emit asm prelude"))
        .arg(Arg::with_name("input")
            .index(1)
            .help("Set input file, uses stdin if none provided"))
        .arg(Arg::with_name("output")
            .short("o")
            .long("output")
            .takes_value(true)
            .help("Set output file, uses stdout if none provided"))
        .get_matches();
    let default_command = Command::Interpret;
    let command = if matches.is_present("lex") {
        Command::Lex
    } else if matches.is_present("parse") {
        Command::Parse
    } else if matches.is_present("emit-ir") {
        Command::EmitIr
    } else if matches.is_present("interpret") {
        Command::Interpret
    } else if matches.is_present("emit-asm") {
        Command::CompileX86
    } else {
        default_command
    };

    let input = match matches.value_of_os("input") {
        Some(path) => Stream::File(Path::new(path).to_owned()),
        None => Stream::Std,
    };

    let output = match matches.value_of_os("output") {
        Some(path) => Stream::File(Path::new(path).to_owned()),
        None => Stream::Std,
    };
    
    let optimize = matches.is_present("optimize");
    let skip_prelude = matches.is_present("no-prelude");

    Ok(Params {
        command,
        optimize,
        skip_prelude,
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

fn emit_diagnostics(input: &str, reporter: Reporter) -> Result<()> {
    let mut diagnostics = reporter.get_diagnostics();
    diagnostics.sort_by_key(|d| d.primary_span.map(|s| s.start));
    plank_errors::print_diagnostics(&input, &diagnostics);
    if reporter.has_errors() {
        Err(Error::BuildFail)
    } else {
        Ok(())
    }
}

fn lex<W: Write>(source: &str, mut output: W) -> Result<()> {
    let reporter = Reporter::new();
    let tokens = plank_syntax::lex(source, reporter.clone());
    emit_diagnostics(source, reporter)?;
    for tok in tokens {
        output.write_fmt(format_args!("{:?}\n", *tok))?;
    }
    Ok(())
}

fn parse<W: Write>(source: &str, mut output: W) -> Result<()> {
    let reporter = Reporter::new();
    let tokens = plank_syntax::lex(source, reporter.clone());
    let program = plank_syntax::parse(tokens, reporter.clone());
    emit_diagnostics(source, reporter)?;
    let formatted = ast_printer::format_program(&program);
    output.write_all(formatted.as_bytes())?;
    output.write_all(b"\n")?;
    Ok(())
}

fn emit_ir<W: Write>(source: &str, mut output: W, optimize: bool) -> Result<()> {
    let reporter = Reporter::new();
    let tokens = plank_syntax::lex(source, reporter.clone());
    let program = plank_syntax::parse(tokens, reporter.clone());
    let ir = plank_frontend::compile(&program, reporter.clone());
    emit_diagnostics(source, reporter)?;
    let mut ir = ir.expect("no errors but failed to produce IR");
    if optimize {
        plank_ir::optimization::optimize(&mut ir);
    }
    plank_ir::emit_program(&ir, &mut output)?;
    if let Err((sym, err)) = plank_ir::validate_ir(&ir) {
        eprintln!("ir validation error in function {:?}: {:?}", sym, err);
    }
    Ok(())
}

fn interpret<W: Write>(source: &str, output: W, optimize: bool) -> Result<()> {
    let reporter = Reporter::new();
    let tokens = plank_syntax::lex(source, reporter.clone());
    let program = plank_syntax::parse(tokens, reporter.clone());
    let ir = plank_frontend::compile(&program, reporter.clone());
    emit_diagnostics(source, reporter)?;
    let mut ir = ir.expect("build succeeded but failed to produce IR");
    if optimize {
        plank_ir::optimization::optimize(&mut ir);
    }
    let input = io::empty();
    let exit_code = plank_interpreter::run_program(&ir, input, output)?;
    if exit_code == 0 {
        Ok(())
    } else {
        Err(Error::InterpreterExit(exit_code))
    }
}

fn compile_x86<W: Write>(source: &str, mut output: W, optimize: bool, skip_prelude: bool) -> Result<()> {
    let reporter = Reporter::new();
    let tokens = plank_syntax::lex(source, reporter.clone());
    let program = plank_syntax::parse(tokens, reporter.clone());
    let ir = plank_frontend::compile(&program, reporter.clone());
    emit_diagnostics(source, reporter)?;
    let mut ir = ir.expect("build succeeded but failed to produce IR");
    if optimize {
        plank_ir::optimization::optimize(&mut ir);
    }
    plank_x86_backend::fix_function_returns(&mut ir);
    let asm = plank_x86_backend::compile_program(&ir);
    if !skip_prelude {
        plank_x86_backend::print_prelude(&mut output)?;
    }
    plank_x86_backend::print_asm(output, &asm)?;
    Ok(())
}
