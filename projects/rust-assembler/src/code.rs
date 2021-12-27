use crate::{AddrType, CCommandType, CommandType};

pub struct CodeGenerator {}

impl CodeGenerator {
    pub fn new() -> CodeGenerator {
        CodeGenerator {}
    }

    pub fn emit_code(&self, commands: &Vec<CommandType>) -> Vec<String> {
        commands.iter().map(|cmd| match cmd {
            CommandType::ACommand(addr_type) => match addr_type {
                AddrType::Number(num) => (*num).to_string(),
                AddrType::Symbol(sym) => *sym, // todo lookup symbol for addr/value
            },
            CommandType::LCommand(addr_type) => match addr_type {
                AddrType::Number(num) => {
                    panic!("Cannot have number address type for LCommand symbol!")
                }
                AddrType::Symbol(sym) => 0.to_string(), // lookup symbol in symtable for addr/value
            },
            CommandType::CCommand(c_cmd_type) => {
                if c_cmd_type.is_jmp_cmd {
                    let comp_part = match c_cmd_type.comp {
                        Some(part) => part,
                        None => panic!("Error, no c command part in jmp cmd!"),
                    };

                    let jmp_part = match c_cmd_type.jump {
                        Some(part) => part,
                        None => panic!("Error, no jump command part in jmp cmd!"),
                    };
                } else {
                    let comp_part = match c_cmd_type.comp {
                        Some(part) => part,
                        None => panic!("Error, no c command part in dest cmd!"),
                    };

                    let dest_part = match c_cmd_type.dest {
                        Some(part) => part,
                        None => panic!("Error, no dest command part in dest cmd!"),
                    };
                }
                todo!()
            }
        });
        vec![]
    }

    fn dest() {}

    fn comp() {}

    fn jump() {}
}
