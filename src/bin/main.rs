#[macro_use] extern crate log;
extern crate failure;
extern crate clap;
extern crate simplelog;
extern crate emjc_lib as emjc;

use std::fs::File;
use std::io::Read;

use failure::Error;

use clap::{
    App,
    AppSettings,
    Arg,
    ArgMatches,
};

use simplelog::{
    SimpleLogger,
    LevelFilter,
    Config,
};

fn app<'a, 'b>() -> App<'a, 'b> {
    App::new("emjc")
        .settings(&[
            AppSettings::ColoredHelp,
        ])
        .arg(Arg::with_name("file")
            .index(1)
            .required(true)
            .takes_value(true)
            .value_name("file"))
}

fn main() {
    let _ = SimpleLogger::init(LevelFilter::Debug, Config::default());

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

    let _ = emjc::lex(&input);

    Ok(())
}
