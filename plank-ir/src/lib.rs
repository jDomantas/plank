pub mod ir;
mod printer;
mod validation;

pub use ir::Program;
pub use printer::emit_program;
pub use validation::validate_ir;
