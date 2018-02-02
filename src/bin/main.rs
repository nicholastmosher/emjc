#[macro_use] extern crate log;
extern crate failure;
extern crate clap;
extern crate simplelog;
extern crate emjc_lib as emjc;

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
        .arg(Arg::with_name("blah"))
}

fn main() {
    let _ = SimpleLogger::init(LevelFilter::Debug, Config::default());
    debug!("Initialized logger");

    let matches = app().get_matches();
    match execute(&matches) {
        Ok(_) => return,
        Err(e) => println!("{}", e),
    }
}

fn execute(_: &ArgMatches) -> Result<(), Error> {

    emjc::lex();
    Ok(())
}
