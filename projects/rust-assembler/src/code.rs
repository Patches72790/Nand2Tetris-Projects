use crate::{
    AddrType, CCommandType, CommandType, CompType, CompTypeA, CompTypeM, DestType, JumpType,
};

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
                    AddrType::Number(num) => {
                        let bits = CodeGenerator::to_bit_repr(num);
                        let num_zeroes = 16 - bits.len();
                        return vec!['0'; num_zeroes].iter().collect::<String>().to_string()
                            + &bits;
                    }
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
                        let comp_mnemonic = CodeGenerator::comp(&comp_part);
                        let jump_mnemonic = CodeGenerator::jump(&jmp_part);
                        return "111".to_string() + &comp_mnemonic + "000" + &jump_mnemonic;
                    } else {
                        let comp_part = match &c_cmd_type.comp {
                            Some(part) => part,
                            None => panic!("Error, no c command part in dest cmd!"),
                        };

                        let dest_part = match &c_cmd_type.dest {
                            Some(part) => part,
                            None => panic!("Error, no dest command part in dest cmd!"),
                        };
                        let comp_mnemonic = CodeGenerator::comp(&comp_part);
                        let dest_mnemonic = CodeGenerator::dest(&dest_part);
                        return "111".to_string() + &comp_mnemonic + &dest_mnemonic + "000";
                    }
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

    fn dest(dest_part: &DestType) -> String {
        match dest_part {
            DestType::M => String::from("001"),
            DestType::D => String::from("010"),
            DestType::MD => String::from("011"),
            DestType::A => String::from("100"),
            DestType::AM => String::from("101"),
            DestType::AD => String::from("110"),
            DestType::AMD => String::from("111"),
            _ => String::from("000"),
        }
    }

    fn comp(comp_part: &CompType) -> String {
        match comp_part {
            CompType::M(type_m) => {
                let c_part = match type_m {
                    &CompTypeM::M => String::from("110000"),
                    &CompTypeM::NotM => String::from("110001"),
                    &CompTypeM::NegM => String::from("110011"),
                    &CompTypeM::MplusOne => String::from("110111"),
                    &CompTypeM::MminusOne => String::from("110010"),
                    &CompTypeM::DminusM => String::from("010011"),
                    &CompTypeM::DplusM => String::from("000010"),
                    &CompTypeM::MminusD => String::from("000111"),
                    &CompTypeM::DandM => String::from("000000"),
                    &CompTypeM::DorM => String::from("010101"),
                };
                return "1".to_string() + &c_part;
            }
            CompType::A(type_a) => {
                let c_part = match type_a {
                    &CompTypeA::Zero => String::from("101010"),
                    &CompTypeA::One => String::from("111111"),
                    &CompTypeA::NegOne => String::from("111010"),
                    &CompTypeA::D => String::from("001100"),
                    &CompTypeA::A => String::from("110000"),
                    &CompTypeA::NotD => String::from("001101"),
                    &CompTypeA::NotA => String::from("110001"),
                    &CompTypeA::NegD => String::from("001111"),
                    &CompTypeA::NegA => String::from("110011"),
                    &CompTypeA::DplusOne => String::from("011111"),
                    &CompTypeA::AplusOne => String::from("110111"),
                    &CompTypeA::DminusOne => String::from("001110"),
                    &CompTypeA::AminusOne => String::from("110010"),
                    &CompTypeA::DplusA => String::from("000010"),
                    &CompTypeA::DminusA => String::from("010011"),
                    &CompTypeA::AminusD => String::from("000111"),
                    &CompTypeA::DandA => String::from("000000"),
                    &CompTypeA::DorA => String::from("010101"),
                };
                return "0".to_string() + &c_part;
            }
        }
    }

    fn jump(jump_part: &JumpType) -> String {
        match jump_part {
            JumpType::JGT => String::from("001"),
            JumpType::JEQ => String::from("010"),
            JumpType::JGE => String::from("011"),
            JumpType::JLT => String::from("100"),
            JumpType::JNE => String::from("101"),
            JumpType::JLE => String::from("110"),
            JumpType::JMP => String::from("111"),
            _ => String::from("000"),
        }
    }
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
