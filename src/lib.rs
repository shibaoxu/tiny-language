#![feature(seek_stream_len)]
#![feature(trait_upcasting)]
#![allow(incomplete_features)]

extern crate core;

pub mod lexer;
pub mod parser;
pub mod ast;
pub mod repl;
pub mod evaluator;

pub use lexer::Lexer;
// pub use lexer::ast;
