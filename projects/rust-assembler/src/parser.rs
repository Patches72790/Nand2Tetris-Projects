use crate::SymTable;

pub struct Parser<'a> {
    symtable: &'a mut SymTable,
    current_address: u32,
}

#[derive(Debug)]
pub enum CommandType {
    ACommand(AddrType),
    CCommand(CCommandType),
    LCommand(AddrType),
}
#[derive(Debug)]
pub enum AddrType {
    Number(u32),
    Symbol(String), // impl TODO
}
#[derive(Debug)]
pub struct CCommandType {
    pub is_jmp_cmd: bool,
    pub dest: Option<DestType>,
    pub comp: Option<CompType>,
    pub jump: Option<JumpType>,
}

impl CCommandType {
    pub fn yield_c_cmd_parts(&self) {
        todo!()
    }
}

#[derive(Debug)]
pub enum DestType {
    M,
    D,
    MD,
    A,
    AM,
    AD,
    AMD,
}
#[derive(Debug)]
pub enum JumpType {
    JGT,
    JEQ,
    JGE,
    JLT,
    JNE,
    JLE,
    JMP,
}

#[derive(Debug)]
pub enum CompTypeA {
    Zero,
    One,
    NegOne,
    D,
    A,
    NotD,
    NotA,
    NegD,
    NegA,
    DplusOne,
    AplusOne,
    DminusOne,
    AminusOne,
    DplusA,
    DminusA,
    AminusD,
    DandA,
    DorA,
}

#[derive(Debug)]
pub enum CompTypeM {
    M,
    NotM,
    NegM,
    MplusOne,
    MminusOne,
    DminusM,
    DplusM,
    MminusD,
    DandM,
    DorM,
}

#[derive(Debug)]
pub enum CompType {
    M(CompTypeM),
    A(CompTypeA),
}

impl Parser<'_> {
    pub fn new(symtable: &mut SymTable) -> Parser {
        Parser {
            symtable,
            current_address: 16,
        }
    }

    pub fn parse_input(&mut self, input_stream: &str) -> Vec<CommandType> {
        input_stream
            .lines()
            .filter_map(|line| {
                if line.is_empty() || line.trim_start().starts_with('/') {
                    return None;
                }

                println!("Parsing current line: ---->{}", line.trim_start());
                self.command_type(line.trim_start())
            })
            .collect::<Vec<CommandType>>()
    }

    fn command_type(&mut self, line: &str) -> Option<CommandType> {
        let mut chars = line.chars();
        let first_char = chars.next().unwrap_or_else(|| {
            panic!("Error reading first character of line!");
        });

        match first_char {
            // parsing for A instructions
            '@' => Some(CommandType::ACommand(
                self.symbol(&chars.take_while(|c| *c != ' ').collect::<String>()),
            )),
            // parsing for comp instructions (dest, comp, jump)
            'D' | 'M' | 'A' | '0' => Some(CommandType::CCommand(self.c_command(
                &(first_char.to_string() + &chars.take_while(|c| *c != ' ').collect::<String>()),
            ))),
            // parsing for Label pseudo-instructions
            '(' => Some(CommandType::LCommand(
                self.pseudo_label(&chars.take_while(|c| *c != ')').collect::<String>()),
            )),
            _ => None,
        }
    }

    fn pseudo_label(&self, line: &str) -> AddrType {
        match self.symtable.get_address(line) {
            Some(addr) => AddrType::Number(addr),
            None => panic!("Use of undeclared pseudo-label!"),
        }
    }

    /// Returns the symbol or decimal value of the current
    /// A command (@XXX or (XXX)).
    /// Also returns the symbol for the L Command label
    fn symbol(&mut self, line: &str) -> AddrType {
        let number_or_sym = match line.parse::<u32>() {
            Ok(val) => Some(val),
            Err(_) => None,
        };

        match number_or_sym {
            Some(num) => AddrType::Number(num),
            None => {
                // if symbol exists, extract address and use it as ACommand's addr param
                if let Some(addr) = self.symtable.get_address(line) {
                    AddrType::Number(addr)
                } else {
                    // if user defined variable doesn't exist, add it to symtable
                    let addr = self.current_address;
                    self.current_address += 1;
                    self.symtable.add_entry(line, addr);
                    AddrType::Number(addr)
                }
            }
        }
    }

    fn c_command(&self, line: &str) -> CCommandType {
        let comp_tokens: Vec<&str> = line.split('=').collect();
        if comp_tokens.len() == 1 {
            let tokens: Vec<&str> = line.split(';').collect();

            if tokens.len() == 1 {
                panic!("Error incorrect syntax! Must be either comp or jump for C instructions!");
            }

            return CCommandType {
                is_jmp_cmd: true,
                dest: None,
                comp: self.comp(tokens[0]),
                jump: self.jump(tokens[1]),
            };
        }

        CCommandType {
            is_jmp_cmd: false,
            dest: self.dest(comp_tokens[0]),
            comp: self.comp(comp_tokens[1]),
            jump: None,
        }
    }

    fn dest(&self, token: &str) -> Option<DestType> {
        match token {
            "M" => Some(DestType::M),
            "D" => Some(DestType::D),
            "A" => Some(DestType::A),
            "MD" | "DM" => Some(DestType::MD),
            "AM" | "MA" => Some(DestType::AM),
            "AD" | "DA" => Some(DestType::AD),
            "AMD" | "ADM" | "DAM" | "DMA" | "MAD" | "MDA" => Some(DestType::AMD),
            _ => None,
        }
    }

    fn comp(&self, token: &str) -> Option<CompType> {
        match token {
            "0" => Some(CompType::A(CompTypeA::Zero)),
            "1" => Some(CompType::A(CompTypeA::One)),
            "-1" => Some(CompType::A(CompTypeA::NegOne)),
            "D" => Some(CompType::A(CompTypeA::D)),
            "A" => Some(CompType::A(CompTypeA::A)),
            "!D" => Some(CompType::A(CompTypeA::NotD)),
            "!A" => Some(CompType::A(CompTypeA::NotA)),
            "-D" => Some(CompType::A(CompTypeA::NegD)),
            "-A" => Some(CompType::A(CompTypeA::NegA)),
            "D+1" => Some(CompType::A(CompTypeA::DplusOne)),
            "A+1" => Some(CompType::A(CompTypeA::AplusOne)),
            "D-1" => Some(CompType::A(CompTypeA::DminusOne)),
            "A-1" => Some(CompType::A(CompTypeA::AminusOne)),
            "D+A" => Some(CompType::A(CompTypeA::DplusA)),
            "D-A" => Some(CompType::A(CompTypeA::DminusA)),
            "A-D" => Some(CompType::A(CompTypeA::AminusD)),
            "D&A" => Some(CompType::A(CompTypeA::DandA)),
            "D|A" => Some(CompType::A(CompTypeA::DorA)),
            "M" => Some(CompType::M(CompTypeM::M)),
            "!M" => Some(CompType::M(CompTypeM::NotM)),
            "-M" => Some(CompType::M(CompTypeM::NegM)),
            "M+1" => Some(CompType::M(CompTypeM::MplusOne)),
            "M-1" => Some(CompType::M(CompTypeM::MminusOne)),
            "D+M" => Some(CompType::M(CompTypeM::DplusM)),
            "D-M" => Some(CompType::M(CompTypeM::DminusM)),
            "M-D" => Some(CompType::M(CompTypeM::MminusD)),
            "D&M" => Some(CompType::M(CompTypeM::DandM)),
            "D|M" => Some(CompType::M(CompTypeM::DorM)),
            _ => None,
        }
    }

    fn jump(&self, token: &str) -> Option<JumpType> {
        match token {
            "JGT" => Some(JumpType::JGT),
            "JEQ" => Some(JumpType::JEQ),
            "JGE" => Some(JumpType::JGE),
            "JLT" => Some(JumpType::JLT),
            "JNE" => Some(JumpType::JNE),
            "JLE" => Some(JumpType::JLE),
            "JMP" => Some(JumpType::JMP),
            _ => None,
        }
    }
}
