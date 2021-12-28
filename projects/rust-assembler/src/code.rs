use crate::{AddrType, CCommandType, CommandType};

pub struct CodeGenerator {}

impl CodeGenerator {
    pub fn new() -> CodeGenerator {
        CodeGenerator {}
    }

    pub fn emit_code(&self, commands: &Vec<CommandType>) -> Vec<String> {
        commands
            .iter()
            .map(|cmd| match cmd {
                CommandType::ACommand(addr_type) => match addr_type {
                    AddrType::Number(num) => CodeGenerator::to_bit_repr(num),
                    AddrType::Symbol(sym) => "VAR:".to_string() + &sym.to_string(), // todo lookup symbol for addr/value
                },
                CommandType::LCommand(addr_type) => match addr_type {
                    AddrType::Number(num) => {
                        panic!("Cannot have number address type for LCommand symbol!")
                    }
                    AddrType::Symbol(sym) => "SYMBOL".to_string(), // lookup symbol in symtable for addr/value
                },
                CommandType::CCommand(c_cmd_type) => {
                    if c_cmd_type.is_jmp_cmd {
                        let comp_part = match &c_cmd_type.comp {
                            Some(part) => part,
                            None => panic!("Error, no c command part in jmp cmd! {:?}", c_cmd_type),
                        };

                        let jmp_part = match &c_cmd_type.jump {
                            Some(part) => part,
                            None => panic!("Error, no jump command part in jmp cmd!"),
                        };
                    } else {
                        let comp_part = match &c_cmd_type.comp {
                            Some(part) => part,
                            None => panic!("Error, no c command part in dest cmd!"),
                        };

                        let dest_part = match &c_cmd_type.dest {
                            Some(part) => part,
                            None => panic!("Error, no dest command part in dest cmd!"),
                        };
                    }
                    "c_cmd:".to_string()
                }
            })
            .collect::<Vec<String>>()
    }

    fn to_bit_repr(num: &i32) -> String {
        if *num < 0 || ((1 << 15) & (*num)) > 0 {
            panic!("Cannot convert negative number or number larger than 15 bits!");
        }

        if *num == 0 {
            return 0.to_string();
        }
        let mut copy = *num;
        let mut result: Vec<String> = vec![];
        while copy > 0 {
            result.push((copy % 2).to_string());
            copy = copy / 2;
        }

        result.reverse();
        result.join("")
    }

    fn dest() {}

    fn comp() {}

    fn jump() {}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_bit_repr() {
        assert_eq!(CodeGenerator::to_bit_repr(&16), "10000".to_string());
        assert_eq!(CodeGenerator::to_bit_repr(&13), "1101".to_string());
        assert_eq!(CodeGenerator::to_bit_repr(&0), "0".to_string());
    }

    #[test]
    #[should_panic]
    fn test_bad_to_bit_repr() {
        assert_eq!(CodeGenerator::to_bit_repr(&-16), "0".to_string());
        assert_eq!(CodeGenerator::to_bit_repr(&32768), "0".to_string());
    }

    #[test]
    #[should_panic]
    fn test_overflow_to_bit_repr_1() {
        assert_eq!(CodeGenerator::to_bit_repr(&32768), "0".to_string());
    }

    #[test]
    fn test_overflow_to_bit_repr_2() {
        assert_eq!(
            CodeGenerator::to_bit_repr(&(32768 - 1)),
            "111111111111111".to_string()
        );
    }
}
