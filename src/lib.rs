//! Oster: source -> tokens -> AST -> resolve -> types -> ownership/borrow -> C11.
#![forbid(unsafe_code)]
pub mod ast;
pub mod codegen;
pub mod lexer;
pub mod own;
pub mod parser;
pub mod resolve;
pub mod typeck;