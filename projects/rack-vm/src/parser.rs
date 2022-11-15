use std::str::Split;

use anyhow::{Error, Result};

type Label = String;

#[derive(Debug)]
pub enum OpCode {
    Arithmetic(BinaryCommand),
    Push(MemorySegment, u16),
    Pop(MemorySegment, u16),
    Label(Label),
    GoTo(Label),
    If(Label),
    Function(String, u8), // fn name, numVars
    Return,
    Call(String, u8), // fn name, numArgs
}

#[derive(Debug)]
pub enum MemorySegment {
    Argument,
    Local,
    This,
    That,
    Constant,
    Static,
    Temp,
    Pointer,
}

#[derive(Debug)]
pub enum BinaryCommand {
    Add,
    Sub,
    Neg,
    And,
    Or,
    Not,
    Eq,
    Lt,
    Gt,
}

pub struct RackVMParser {
    input: String,
}

impl RackVMParser {
    pub fn new(input: &str) -> RackVMParser {
        RackVMParser {
            input: input.to_string(),
        }
    }

    pub fn parse(&self) -> Result<Vec<OpCode>> {
        Ok(self
            .input
            .lines()
            .map(|s| self.parse_opcode(s).unwrap())
            .collect::<Vec<OpCode>>())
    }

    fn parse_opcode(&self, line: &str) -> Option<OpCode> {
        let mut tokens = line.trim().split(' ');
        let command = tokens.next().unwrap();
        match command {
            // arithmetic commands
            "add" => Some(OpCode::Arithmetic(BinaryCommand::Add)),
            "sub" => Some(OpCode::Arithmetic(BinaryCommand::Sub)),
            "neg" => Some(OpCode::Arithmetic(BinaryCommand::Neg)),
            "eq" => Some(OpCode::Arithmetic(BinaryCommand::Eq)),
            "gt" => Some(OpCode::Arithmetic(BinaryCommand::Gt)),
            "lt" => Some(OpCode::Arithmetic(BinaryCommand::Lt)),
            "and" => Some(OpCode::Arithmetic(BinaryCommand::And)),
            "or" => Some(OpCode::Arithmetic(BinaryCommand::Or)),
            "not" => Some(OpCode::Arithmetic(BinaryCommand::Not)),

            // stack commands
            "push" | "pop" => self.parse_stack_command(command, &mut tokens),

            // branch commands
            "label" | "goto" | "if-goto" => todo!("Branch commands not implemented yet"),

            // Function commands
            "function" | "call" | "return" => todo!("Function commands not implemented yet"),
            _ => None,
        }
    }

    fn parse_stack_command(&self, opcode: &str, tokens: &mut Split<char>) -> Option<OpCode> {
        let segment = tokens
            .next()
            .expect("Error parsing stack command without a segment component");
        let index = tokens
            .next()
            .expect("Error parsing stack command without an index component");
        let zip = match segment {
            "argument" => (MemorySegment::Argument, index.parse::<u16>().unwrap()),
            "local" => (MemorySegment::Local, index.parse::<u16>().unwrap()),
            "static" => (MemorySegment::Static, index.parse::<u16>().unwrap()),
            "constant" => (MemorySegment::Constant, index.parse::<u16>().unwrap()),
            "this" => (MemorySegment::This, index.parse::<u16>().unwrap()),
            "that" => (MemorySegment::That, index.parse::<u16>().unwrap()),
            "pointer" => (MemorySegment::Pointer, index.parse::<u16>().unwrap()),
            "temp" => (MemorySegment::Temp, index.parse::<u16>().unwrap()),

            _ => panic!("Error parsing stack command with invalid segment"),
        };

        if opcode == "push" {
            Some(OpCode::Push(zip.0, zip.1))
        } else {
            Some(OpCode::Pop(zip.0, zip.1))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parser_with_stack_opcodes() {
        let test_opcodes = "push argument 1\npop pointer 1\nadd\npush local 9954\npop temp 1234\n";
        let parser = RackVMParser::new(test_opcodes);
        let result = parser.parse();
        if let Ok(opcodes) = result {
            println!("{:?}", opcodes);
        } else {
            panic!()
        }
    }
}
