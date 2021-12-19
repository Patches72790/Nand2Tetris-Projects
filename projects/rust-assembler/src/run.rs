use crate::code::CodeGenerator;
use crate::parser::Parser;
use std::env::Args;
use std::fs;

pub struct Config {
    parser: Parser,
    code_generator: CodeGenerator,
}

impl Config {
    pub fn new(args: &mut Args) -> Result<Config, &'static str> {
        if args.len() < 2 {
            return Err("Must provide filename as arg.\nUsage: rasm [filename]");
        }
        args.next();

        let filename = match args.next() {
            Some(val) => val,
            None => return Err("Error reading filename from args."),
        };

        let input_stream = match fs::read_to_string(filename) {
            Ok(input) => input,
            Err(msg) => return Err("Error reading input from file."),
        };
        Ok(Config {
            parser: Parser::new(input_stream),
            code_generator: CodeGenerator::new(),
        })
    }

    pub fn run(&self) {
        for (i, line) in self.parser.input_stream.lines().enumerate() {
            println!("Line {}: {}", i, line);
        }
        let commands = self.parser.parse_input();
        for command in commands {
            println!("Command: {:?}", command);
        }
    }
}
