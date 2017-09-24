#![deny(missing_docs)]

//! A library to build and format diagnostics for use in plank compiler.

pub mod position;
pub mod reporter;
pub mod printer;

pub use reporter::Reporter;
pub use printer::{print_diagnostic, print_diagnostics};
