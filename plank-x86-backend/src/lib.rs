extern crate plank_ir;

mod compiler;
mod printer;
mod x86;

pub use compiler::compile_program;
pub use printer::print_asm;
