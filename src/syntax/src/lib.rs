// #![allow(dead_code)]

extern crate errors;


pub mod ast;
pub mod position;
pub mod tokens;
mod lexer;
mod parser;

pub use lexer::lex;
pub use parser::parse;


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
