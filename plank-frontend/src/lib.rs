extern crate plank_errors;
extern crate plank_syntax;

mod ast {
    pub mod resolved;
    pub mod typed;
    pub mod cfg;
}
mod symbols;
mod resolve_symbols;
mod type_param_check;
mod wildcard_check;
mod type_check;
mod build_cfg;
mod dead_code;
mod return_check;
mod gen_constructors;

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
    let typed = type_check::type_check(&resolved, &mut ctx);
    let mut cfg = build_cfg::build_cfg(&typed, &mut ctx);
    dead_code::remove_dead_code(&mut cfg, &mut ctx);
    return_check::check_returns(&cfg, &mut ctx);
    gen_constructors::add_constructors(&mut cfg);
    ast::cfg::printer::print_program(&cfg, &ctx);
}
