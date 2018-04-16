#![feature(nll)]
#![deny(warnings)]

#[macro_use]
extern crate log;
extern crate env_logger;
#[macro_use]
extern crate failure;
extern crate clap;
extern crate emjc_lib as emjc;

use std::env;
use std::fs::File;
use std::io::{
    BufReader,
    Write,
};
use std::rc::Rc;
use std::process::Command;

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
use emjc::semantics::name_analysis::NameAnalyzer;
use emjc::semantics::pretty_printer::PrettyPrinter;
use emjc::semantics::type_analysis::TypeChecker;
use emjc::codegen::generator::CodeGenerator;

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
        .arg(Arg::with_name("type").long("--type"))
        .arg(Arg::with_name("cgen").long("--cgen"))
        .group(ArgGroup::with_name("options")
            .required(true)
            .args(&["lex", "ast", "name", "pretty print", "type", "cgen"]))
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

const JASMIN: &[u8] = include_bytes!("../../resources/jasmin.jar");

fn execute(args: &ArgMatches) -> Result<(), Error> {

    let filename = args.value_of("file").unwrap();
    let file = File::open(filename).unwrap();
    let mut reader = BufReader::new(file);

    let lex  = args.is_present("lex");
    let ast  = args.is_present("ast");
    let name = args.is_present("name");
    let pp   = args.is_present("pretty print");
    let kind = args.is_present("type");
    let cgen = args.is_present("cgen");

    let mut lexer = Lexer::new(&mut reader).unwrap();
    if lex {
        for token in lexer.iter() {
            println!("{}", token);
        }
        return Ok(());
    }

    let mut parser = Parser::new(&mut lexer);
    let program = Rc::new(parser.parse_program()?);

    if ast {
        let mut printer = Printer::new();
        printer.print(&program);
        return Ok(());
    }

    let source_map = lexer.source_map;

    let mut errors = 0;
    let mut name_analyzer = NameAnalyzer::new();
    name_analyzer.analyze(&program);

    if name || kind || cgen {
        for err in name_analyzer.errors.iter() {
            eprintln!("{}", err);
        }
        errors += name_analyzer.errors.len();
        if !kind { return Ok(()); }
    }

    let mut type_analyzer = TypeChecker::new(&source_map, &program);
    type_analyzer.analyze();

    if kind || cgen {
        for error in type_analyzer.errors.iter() {
            eprintln!("{}", error);
        }
        errors += type_analyzer.errors.len();
    }

    if pp {
        let mut pretty_printer = PrettyPrinter::new();
        pretty_printer.print(&program);
        return Ok(());
    }

    if errors != 0 {
        return Ok(());
    }

    if kind || name {
        println!("Valid eMiniJava Program");
    }

    let mut cg = CodeGenerator::new();

    if cgen {

        let home_dir = env::home_dir()
            .ok_or(format_err!("Could not open home directory to launch Jasmin"))?;

        let jasmin_path = {
            let mut jasmin = home_dir.clone();
            jasmin.push("jasmin.jar");
            jasmin
        };

        // If the jasmin jar doesn't exist on disk, write it to use later.
        if !jasmin_path.exists() {
            let mut jasmin_jar = File::create(&jasmin_path)
                .map_err(|_| format_err!("Failed to create jasmin.jar in '{}'", home_dir.display()))?;

            jasmin_jar.write_all(JASMIN)
                .map_err(|_| format_err!("Failed to write '{}'", jasmin_path.display()))?;
        }

        // Create a '.jasmin' directory to put the intermediate assembly files into.
        let codegen_path = {
            let mut codegen = env::current_dir()
                .map_err(|_| format_err!("Failed to open the current directory"))?;
            codegen.push(".jasmin");
            codegen
        };
        std::fs::create_dir_all(&codegen_path)
            .map_err(|_| format_err!("Failed to create directory: '{}'", codegen_path.display()))?;

        // Generate the assembly and output the instructions for each class into their own file.
        cg.gen_program(&program);

        let mut jasmin_files = Vec::<String>::new();

        for class in cg.classes.iter() {
            let mut base_path = codegen_path.clone();
            let mut filename = class.name.clone();
            filename.push_str(".jasmin");
            base_path.push(filename);
            jasmin_files.push(format!("{}", base_path.display()));
            let mut class_file = File::create(&base_path)
                .map_err(|_| format_err!("Failed to open '{}'", base_path.display()))?;
            let _ = writeln!(class_file, "{}", class);
            for method in class.methods.iter() {
                let _ = writeln!(class_file, "{}", method);
            }
        }

        // Create a '.class' directory to put the intermediate assembly files into.
        let classpath = {
            let mut codegen = env::current_dir()
                .map_err(|_| format_err!("Failed to open the current directory"))?;
            codegen.push(".class");
            codegen
        };

        Command::new("java")
            .arg("-jar")
            .arg(format!("{}", jasmin_path.display()))
            .arg("-d").arg(format!("{}", classpath.display()))
            .args(jasmin_files)
            .spawn()
            .and_then(|mut child| child.wait())
            .map_err(|_| format_err!("Failed to invoke Jasmin on 'codegen.jasmin'"))?;
    }

    Ok(())
}
