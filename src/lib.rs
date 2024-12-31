#![feature(box_patterns)]

pub mod ast;
pub mod lex;
pub mod parse;

pub(crate) type StopInteger = i32;
pub(crate) type StopRawLiteral = String;
pub(crate) type StopBool = bool;
