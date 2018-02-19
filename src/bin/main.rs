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

    if args.is_present("lex") {
        let tokens = emjc::lexer::lex(&input)?;
        if tokens.failed.is_empty() {
            for t in tokens.iter() {
                println!("{}", t);
            }
        } else {
            println!("Failed to lex {}", filename);
            for e in tokens.failed.iter() {
                println!("Unrecognized token: {}", e);
            }
        }
    }

    if args.is_present("ast") {
        println!("TODO: Generate abstract syntax tree");
    }

    Ok(())
}
