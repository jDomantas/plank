extern crate plank_errors;
extern crate plank_ir;
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
mod cast_check;
mod build_cfg;
mod dead_code;
mod return_check;
mod gen_constructors;
mod struct_layout;
mod build_ir;
mod assign_check;
mod struct_check;
mod literal_size_check;

mod builtins {
    use ast::resolved::Symbol;
    pub const SIZE_OF: Symbol = Symbol(0);
    pub const ALIGN_OF: Symbol = Symbol(1);
    pub const GETC: Symbol = Symbol(2);
    pub const PUTC: Symbol = Symbol(3);

    pub const SIZE_OF_TYPE_PARAM: Symbol = Symbol(4);
    pub const ALIGN_OF_TYPE_PARAM: Symbol = Symbol(5);
    pub const PUTC_PARAM: Symbol = Symbol(6);
}

use plank_errors::Reporter;
use plank_syntax::ast::Program;
use symbols::Symbols;


struct CompileCtx {
    symbols: Symbols,
    reporter: Reporter,
}

pub fn compile(program: &Program, reporter: Reporter) -> Result<plank_ir::Program, ()> {
    let mut ctx = CompileCtx {
        symbols: Symbols::new(),
        reporter,
    };

    let mut resolved = resolve_symbols::resolve_program(program, &mut ctx);
    type_param_check::check_type_params(&mut resolved, &mut ctx);
    wildcard_check::check_for_wildcards(&resolved, &mut ctx);
    struct_check::check_program(&mut resolved, &mut ctx);
    let mut typed = type_check::type_check(&resolved, &mut ctx);
    literal_size_check::check_program(&mut typed, &mut ctx);
    cast_check::check_casts(&mut typed, &mut ctx);
    let mut cfg = build_cfg::build_cfg(&typed, &mut ctx);
    dead_code::remove_dead_code(&mut cfg, &mut ctx);
    assign_check::check_program(&cfg, &mut ctx);
    return_check::check_returns(&mut cfg, &mut ctx);
    gen_constructors::add_constructors(&mut cfg);
    if ctx.reporter.has_errors() {
        Err(())
    } else {
        Ok(build_ir::build_ir(&cfg, &ctx))
    }
}
