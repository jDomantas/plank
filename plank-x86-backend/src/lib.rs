extern crate plank_ir;

mod compiler;
mod printer;
mod return_fix;
mod x86;

pub use compiler::compile_program;
pub use printer::{print_asm, print_prelude};
pub use return_fix::fix_function_returns;
