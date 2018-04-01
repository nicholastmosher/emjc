#![feature(nll)]
#![recursion_limit = "128"]
#![deny(warnings)]

#[macro_use]
extern crate log;
#[macro_use]
extern crate failure;
#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate tendril;
extern crate uuid;

use std::result;
use failure::Error;

type Result<T> = result::Result<T, Error>;

/// Declares a module called "lexer", which defines lexical tokens
/// and the lexing function. See "lexer.rs".
pub mod lexer;

pub mod syntax;

pub mod semantics;

//pub mod emj_grammar;
