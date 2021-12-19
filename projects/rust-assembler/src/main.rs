use rasm::Config;
use std::env::args;

fn main() {
    let config = Config::new(&mut args()).unwrap_or_else(|err| {
        println!(
            "Error reading command line args. Exiting with message:\n{}",
            err
        );
        std::process::exit(1);
    });

    config.run();
}
