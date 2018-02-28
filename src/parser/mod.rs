#![allow(dead_code)]

//pub mod stack_parser;
pub mod recursive_parser;
pub mod ast;
pub mod visitor;

use lexer::Token;

pub use self::recursive_parser::Parser as Parser;

#[derive(Debug, Fail)]
enum ParseError {
    #[fail(display = "parse stack has no nonterminals, but there's more input")]
    PrematureConclusion,
//    #[fail(display = "parse stack top ({}) does not match current token ({})", _0, _1)]
//    TerminalMismatch(Terminal, Terminal),
//    #[fail(display = "nonterminal {:?} has no production rule for terminal {}", _0, _1)]
//    MissingProduction(NonTerminal, Terminal),
    #[fail(display = "read unexpected token")]
    UnexpectedToken,
}

