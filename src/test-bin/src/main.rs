extern crate errors;
extern crate syntax;


type Result<T> = ::std::result::Result<T, ()>;

fn main() {
    if run().is_err() {
        ::std::process::exit(1);
    }
}

fn run() -> Result<()> {
    let file = get_parameter()?;
    let source = read_file(&file)?;
    compile(&source)
}

fn get_parameter() -> Result<String> {
    let mut args = ::std::env::args().collect::<Vec<_>>();
    if args.len() == 2 {
        Ok(args.pop().expect("arg disappeared"))
    } else {
        eprintln!("Usage: {} <file>", args[0]);
        Err(())
    }
}

fn read_file(name: &str) -> Result<String> {
    use std::io::prelude::*;
    use std::fs::File;
    let mut file = File::open(name).map_err(|e| {
        eprintln!("Unable to open '{}': {}", name, e);
    })?;
    let mut contents = String::new();
    file.read_to_string(&mut contents).map_err(|e| {
        eprintln!("Unable to read '{}': {}", name, e);
    })?;
    Ok(contents)
}

fn compile(source: &str) -> Result<()> {
    let reporter = errors::Reporter::new();
    let tokens = syntax::lex(source, reporter.clone());
    let _ = syntax::parse(tokens, reporter.clone());
    let diagnostics = reporter.get_diagnostics();
    if diagnostics.len() == 0 {
        println!("OK");
        Ok(())
    } else {
        errors::print_diagnostics(source, &diagnostics);
        Err(())
    }
}
