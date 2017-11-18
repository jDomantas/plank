pub mod ir;
pub mod optimization;
pub mod analysis;
pub mod validation;
mod printer;

pub use ir::Program;
pub use printer::emit_program;
pub use validation::validate_ir;
