use anyhow::{bail, Result};

use crate::codewriter::AsmInstr;

type Label = String;

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

pub enum MemorySegment {
    SP,
    Arg,
    This,
    That,
    Constant,
    Static,
    Temp,
    Pointer,
}

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
    commands: Vec<String>,
}

impl RackVMParser {
    pub fn new() -> RackVMParser {
        RackVMParser { commands: vec![] }
    }

    pub fn parse(&self) -> Result<Vec<AsmInstr>> {
        let _ = self.commands.iter().map(|line| {
            let opcode = self.parse_opcode(line).unwrap();

            match opcode {
                OpCode::Arithmetic(_) => Ok(""),
                _ => bail!("Error parsing opcode"),
            }
        });
        Ok(vec![])
    }

    fn parse_opcode(&self, line: &str) -> Option<OpCode> {
        None
    }
}
