extern crate plank_errors;
extern crate plank_syntax;

mod resolved_ast;
mod symbols;
mod resolve_symbols;
mod type_param_check;
mod wildcard_check;

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

    let mut resolved = resolve_symbols::resolve_program(program, &mut ctx);
    type_param_check::check_type_params(&mut resolved, &mut ctx);
    wildcard_check::check_for_wildcards(&resolved, &mut ctx);
}
