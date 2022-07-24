#![feature(seek_stream_len)]

// extern crate core;

pub mod lexer;

pub use lexer::Lexer;
pub use lexer::Token;
pub use lexer::repl;