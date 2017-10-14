extern crate plank_errors;
extern crate plank_syntax;

mod resolved_ast;
mod symbols;
mod resolve_symbols;

use plank_errors::Reporter;
use plank_syntax::ast::Program;
use symbols::Symbols;


#[derive(Default)]
struct CompileCtx {
    symbols: Symbols,
    reporter: Reporter,
}

pub fn compile(program: &Program, reporter: Reporter) {
    let mut ctx = CompileCtx {
        symbols: Default::default(),
        reporter,
    };

    resolve_symbols::resolve_program(program, &mut ctx);
}
