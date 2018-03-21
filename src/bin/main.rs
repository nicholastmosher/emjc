#![deny(warnings)]

#[macro_use]
extern crate log;
extern crate env_logger;
extern crate failure;
extern crate clap;
extern crate emjc_lib as emjc;

use std::fs::File;
use std::io::BufReader;
use std::rc::Rc;

use failure::Error;
use clap::{
    App,
    AppSettings,
    Arg,
    ArgGroup,
    ArgMatches,
};

use emjc::lexer::Lexer;
use emjc::syntax::Parser;
use emjc::syntax::visitor::printer::Printer;
use emjc::semantics::name_analyzer::NameAnalyzer;
use emjc::semantics::pretty_printer::PrettyPrinter;

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
        .arg(Arg::with_name("name").long("--name"))
        .arg(Arg::with_name("pretty print").long("--pp"))
        .group(ArgGroup::with_name("options")
            .required(true)
            .args(&["lex", "ast", "name", "pretty print"]))
}

/// The entry point to the "emjc" application. Here we initialize the
/// logger, set up the argument syntax, and pass execution to "execute",
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
    let file = File::open(filename).unwrap();
    let mut reader = BufReader::new(file);

    let lex  = args.is_present("lex");
    let ast  = args.is_present("ast");
    let name = args.is_present("name");
    let pp   = args.is_present("pretty print");

    let lexer = Lexer::new(&mut reader).unwrap();
    if lex {
        for token in lexer.iter() {
            println!("{}", token);
        }
    }

    let mut parser = Parser::new(lexer);
    let program = Rc::new(parser.parse_program()?);

    if ast {
        let mut printer = Printer::new();
        printer.print(&program);
    }

    if name {
        let mut name_analyzer = NameAnalyzer::new();
        name_analyzer.analyze(&program);
    }

    if pp {
        let mut pretty_printer = PrettyPrinter::new();
        pretty_printer.print(&program);
    }

    Ok(())
}
