use crate::code::CodeGenerator;
use crate::parser::Parser;
use crate::SymTable;
use crate::DEBUG_MODE;
use std::env::Args;
use std::fs;
use std::io::Write;

pub struct Config {
    code_generator: CodeGenerator,
    symtable: SymTable,
    input_stream: String,
    filename: String,
}

impl Config {
    pub fn new(args: &mut Args) -> Result<Config, &'static str> {
        if args.len() < 2 || args.len() > 2 {
            return Err("Must provide filename as arg.\nUsage: rasm [filename]");
        }
        args.next();

        let filename = match args.next() {
            Some(val) => val,
            None => return Err("Error reading filename from args."),
        };

        let input_stream = match fs::read_to_string(filename.clone()) {
            Ok(input) => input,
            Err(_) => return Err("Error reading input from file."),
        };
        Ok(Config {
            code_generator: CodeGenerator::new(),
            symtable: SymTable::new(),
            input_stream,
            filename,
        })
    }

    pub fn run(&mut self) -> Result<(), &str> {
        if !DEBUG_MODE.unwrap_or("").is_empty() {
            for (i, line) in self.input_stream.lines().enumerate() {
                println!("Line {}: {}", i, line);
            }
        }

        // TODO -- Build symbol table for labels/address mappings
        self.symtable.build_table(&self.input_stream);

        // parse input stream
        let mut parser = Parser::new(&mut self.symtable);
        let commands = parser.parse_input(&self.input_stream);

        // TODO -- emit code based on commands read
        let code = self.code_generator.emit_code(&commands);

        if !DEBUG_MODE.unwrap_or("").is_empty() {
            for command in commands {
                println!("Command: {:?}", command);
            }

            for (i, line) in code.iter().enumerate() {
                println!("Codegen: #{}: {:?}", i, line);
            }

            println!("\n!!!SYM TABLE OUTPUT!!!\n");
            self.symtable.print_symtable();
        }

        // write output to filename given
        self.write_to_output_file(&code)?;

        Ok(())
    }

    fn write_to_output_file(&self, code: &Vec<String>) -> std::result::Result<(), &'static str> {
        let filename = self.filename.clone() + &".hack".to_string();
        let mut f = std::fs::File::create(filename.clone()).unwrap();

        for line in code.iter() {
            write!(f, "{}\n", line).unwrap();
        }

        Ok(())
        //        let arr = code.iter().map(|s| s.as_bytes()).collect();
        //        match OpenOptions::new().write(true).open(self.filename.clone()) {
        //            Ok(file) => match file.write_all(arr) {
        //                Ok(_) => Ok(()),
        //                Err(_) => Err("Error writing to file"),
        //            },
        //            Error => Err("Error writing to file"),
        //        }
    }
}
