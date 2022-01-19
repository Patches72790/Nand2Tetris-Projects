use rasm::Config;
use std::env::args;

fn main() {
    let mut config = Config::new(&mut args()).unwrap_or_else(|err| {
        println!(
            "Error reading command line args. Exiting with message:\n{}",
            err
        );
        std::process::exit(1);
    });

    if let Err(msg) = config.run() {
        println!("Error running rasm. Exiting with message:\n{}", msg);
    }
}
