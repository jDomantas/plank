pub mod analysis;
pub mod ir;
pub mod optimization;
mod printer;
pub mod validation;

pub use ir::Program;
pub use printer::emit_program;
pub use validation::validate_ir;
