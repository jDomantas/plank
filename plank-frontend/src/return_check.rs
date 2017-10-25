use ast::cfg::{Program, Function, BlockEnd};
use CompileCtx;


fn check_function(f: &Function, ctx: &mut CompileCtx) {
    fn is_error(end: &BlockEnd) -> bool {
        match *end {
            BlockEnd::Error => true,
            _ => false,
        }
    }

    if f.blocks.iter().any(|(_, b)| is_error(&b.end)) {
        let span = f.complete_span;
        ctx.reporter
            .error("not all paths return a value", span)
            .span(span)
            .build();
    }
}


pub(crate) fn check_returns(program: &Program, ctx: &mut CompileCtx) {
    for (_, f) in &program.functions {
        check_function(f, ctx);
    }
}