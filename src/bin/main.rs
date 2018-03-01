#![deny(warnings)]

#[macro_use]
extern crate log;
extern crate env_logger;
extern crate failure;
extern crate clap;
extern crate emjc_lib as emjc;

use std::fs::File;
use std::io::Read;

use failure::Error;

use clap::{
    App,
    AppSettings,
    Arg,
    ArgGroup,
    ArgMatches,
};

/// Defines the command-line interface for this program. This is located
/// in its own function because it allows us to generate auto-completion
/// scripts for Bash and Zsh at compile time. I haven't actually made use
/// of this but it's possible.
fn app<'a, 'b>() -> App<'a, 'b> {
    App::new("emjc")
        .settings(&[
            AppSettings::ArgRequiredElseHelp,
            AppSettings::ColoredHelp,
            AppSettings::DeriveDisplayOrder,
        ])
        .usage("emjc [options] <source file>")
        .arg(Arg::with_name("file")
            .index(1)
            .required(true)
            .takes_value(true)
            .value_name("source file")
            .help("The eMiniJava (emj) program to compile"))
        .arg(Arg::with_name("lex").long("--lex"))
        .arg(Arg::with_name("ast").long("--ast"))
        .group(ArgGroup::with_name("options")
            .required(true)
            .args(&["lex", "ast"]))
}

/// The entry point to the "emjc" application. Here we initialize the
/// logger, set up the argument parser, and pass execution to "execute",
/// which invokes the correct emjc_lib module.
fn main() {
    env_logger::init();
    debug!("Initialized logger");

    let matches = app().get_matches();
    match execute(&matches) {
        Ok(_) => return,
        Err(e) => println!("{}", e),
    }
}

fn execute(args: &ArgMatches) -> Result<(), Error> {

    let filename = args.value_of("file").unwrap();
    let mut file = File::open(filename).unwrap();

    let input = {
        let mut s = String::new();
        let _ = file.read_to_string(&mut s);
        s
    };

    let (_lex, ast) = match (args.is_present("lex"), args.is_present("ast")) {
        (true, _) => (true, false),
        (_, true) => (false, true),
        _ => unreachable!(),
    };

    use emjc::lexer::{
        self,
        Token,
        TokenType,
    };

    let lexer = lexer::Lexer::new(&input).unwrap();

    let _lang = vec! [
        Token { ty: TokenType::LPAREN, text: "".to_owned(), line: 0, column: 0 },
        Token { ty: TokenType::BANG, text: "".to_owned(), line: 0, column: 0 },
        Token { ty: TokenType::PLUS, text: "".to_owned(), line: 0, column: 0 },
        Token { ty: TokenType::BANG, text: "".to_owned(), line: 0, column: 0 },
        Token { ty: TokenType::RPAREN, text: "".to_owned(), line: 0, column: 0 },
        Token { ty: TokenType::EOF, text: "".to_owned(), line: 0, column: 0 },
    ];

    if ast {
        let mut parser = emjc::parser::Parser::new(lexer);
        let program = parser.parse_program()?;

        use emjc::parser::visitor::Visitor;
        let printer = emjc::parser::visitor::printer::Printer::new();
        printer.visit_program(&program);
    }

    Ok(())
}
