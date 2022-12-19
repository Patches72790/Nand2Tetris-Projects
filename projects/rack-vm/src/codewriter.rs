use anyhow::{Ok, Result};

use crate::{
    a_literal,
    parser::{BinaryCommand, MemorySegment, OpCode},
    pop_stack, write_a, write_c,
};

pub type AsmInstr = String;

pub struct RackVMCodeWriter {}

pub trait CodeWriter {
    fn codegen(code: &[OpCode]) -> Result<Vec<String>>;
}

impl CodeWriter for RackVMCodeWriter {
    fn codegen(code: &[OpCode]) -> Result<Vec<String>> {
        let asm_code = code
            .iter()
            .flat_map(|opcode| match opcode {
                OpCode::Arithmetic(cmd) => RackVMCodeWriter::write_arithmetic_code(cmd),
                OpCode::Push(segment, index) => RackVMCodeWriter::write_push_code(segment, index),
                OpCode::Pop(segment, index) => RackVMCodeWriter::write_pop_code(segment, index),
                other => panic!("Error unimplemented opcode in code writer: {:?}", other),
            })
            .collect::<Vec<String>>();

        Ok(asm_code)
    }
}

impl RackVMCodeWriter {
    pub fn new() -> RackVMCodeWriter {
        RackVMCodeWriter {}
    }

    fn write_push_code(segment: &MemorySegment, index: &u16) -> Vec<String> {
        match segment {
            MemorySegment::Constant => {
                vec![
                    write_a!(index),
                    write_c!("D=A"),
                    a_literal!("SP"),
                    write_c!("A=M"),
                    write_c!("M=D"),
                    a_literal!("SP"),
                    write_c!("M=M+1"),
                ]
            }
            other => todo!("Memory segment unimplemented in code writer: {:?}", other),
        }
    }
    fn write_pop_code(segment: &MemorySegment, index: &u16) -> Vec<String> {
        todo!("Haven't implemented pop code yet!")
    }

    fn write_arithmetic_code(cmd: &BinaryCommand) -> Vec<String> {
        match cmd {
            BinaryCommand::Add => vec![pop_stack!("R13"), pop_stack!("R14")].concat(),
            other => todo!("Arithmetic command unimplemented: {:?}", other),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::RackVMParser;

    use super::*;
    #[test]
    fn test_code_writer() {
        let opcodes = "push constant 69\n";
        let parser = RackVMParser::new(opcodes);
        let result = parser.parse().unwrap();

        let result = RackVMCodeWriter::codegen(&result).unwrap();

        println!("{:?}", result);

        assert!(false);
    }
}
