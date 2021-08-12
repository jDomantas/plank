#![deny(missing_docs)]

//! A library to build and format diagnostics for use in plank compiler.

pub mod position;
pub mod printer;
pub mod reporter;

pub use printer::{print_diagnostic, print_diagnostics};
pub use reporter::Reporter;
