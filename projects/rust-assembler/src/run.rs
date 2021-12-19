use std::env::Args;
use std::fs;

pub struct Config {
    pub input_stream: String,
}

impl Config {
    pub fn new(args: &mut Args) -> Result<Config, &'static str> {
        if args.len() < 2 {
            return Err("Error reading command line args. Must provide 2 arguments");
        }
        args.next();

        let filename = match args.next() {
            Some(val) => val,
            None => return Err("Error reading filename from args."),
        };

        let input_stream = fs::read_to_string(filename).unwrap_or_else(|err| {
            return err.to_string();
        });

        Ok(Config { input_stream })
    }
}
