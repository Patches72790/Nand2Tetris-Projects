pub struct Parser {
    pub input_stream: String,
}

#[derive(Debug)]
pub enum CommandType {
    ACommand(AddrType),
    CCommand(CCommandType),
    LCommand(AddrType),
}
#[derive(Debug)]
pub enum AddrType {
    Number(i32),
    Symbol(String), // impl TODO
}
#[derive(Debug)]
pub struct CCommandType {
    dest: Option<DestType>,
    comp: Option<CompType>,
    jump: Option<JumpType>,
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
pub enum CompType {
    Zero,
    One,
    NegOne,
    D,
    A,
    M,
    NotD,
    NotA,
    NotM,
    NegD,
    NegA,
    NegM,
    DplusOne,
    AplusOne,
    MplusOne,
    DminusOne,
    AminusOne,
    MminusOne,
    DplusA,
    DplusM,
    DminusA,
    DminusM,
    AminusD,
    MminusD,
    DandA,
    DandM,
    DorA,
    DorM,
}

impl Parser {
    pub fn new(input_stream: String) -> Parser {
        Parser { input_stream }
    }

    pub fn parse_input(&self) -> Vec<CommandType> {
        let mut commands = vec![];

        for line in self.input_stream.lines() {
            if line.len() == 0 {
                continue;
            }
            match self.command_type(line) {
                Some(cmd) => commands.push(cmd),
                None => continue,
            };
        }

        commands
    }

    fn command_type(&self, line: &str) -> Option<CommandType> {
        let mut chars = line.chars();
        let first_char = chars.next().unwrap_or_else(|| {
            panic!("Error reading first character of line!");
        });

        match first_char {
            // parsing for A instructions
            '@' => Some(CommandType::ACommand(
                self.symbol(&chars.collect::<String>().to_string()),
            )),
            // parsing for comp instructions (dest, comp, jump)
            'D' | 'M' | 'A' | '0' => Some(CommandType::CCommand(self.c_command(line))),
            // parsing for Label pseudo-instructions
            '(' => Some(CommandType::LCommand(
                self.symbol(
                    &chars
                        .take_while(|c| c.is_alphabetic())
                        .collect::<String>()
                        .to_string(),
                ),
            )),
            _ => None,
        }
    }

    /// Returns the symbol or decimal value of the current
    /// A command (@XXX or (XXX)).
    /// Also returns the symbol for the L Command label
    fn symbol(&self, line: &str) -> AddrType {
        let number_or_sym = match line.parse::<i32>() {
            Ok(val) => Some(val),
            Err(_) => None,
        };

        match number_or_sym {
            Some(num) => AddrType::Number(num),
            None => AddrType::Symbol(line.to_string()),
            // TODO lookup address of symbol in symtable before adding
            // case 1) user defined goto label
            // case 2) predefined labels
            // case 3) user defined variable
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
                dest: None,
                comp: self.comp(tokens[0]),
                jump: self.jump(tokens[1]),
            };
        }

        CCommandType {
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
            "0" => Some(CompType::Zero),
            "1" => Some(CompType::One),
            "-1" => Some(CompType::NegOne),
            "D" => Some(CompType::D),
            "A" => Some(CompType::A),
            "!D" => Some(CompType::NotD),
            "!A" => Some(CompType::NotA),
            "-D" => Some(CompType::NegD),
            "-A" => Some(CompType::NegA),
            "D+1" => Some(CompType::DplusOne),
            "A+1" => Some(CompType::AplusOne),
            "D-1" => Some(CompType::DminusOne),
            "A-1" => Some(CompType::AminusOne),
            "D+A" => Some(CompType::DplusA),
            "D-A" => Some(CompType::DminusA),
            "A-D" => Some(CompType::AminusD),
            "D&A" => Some(CompType::DandA),
            "D|A" => Some(CompType::DorA),
            "M" => Some(CompType::M),
            "!M" => Some(CompType::NotM),
            "-M" => Some(CompType::NegM),
            "M+1" => Some(CompType::MplusOne),
            "M-1" => Some(CompType::MminusOne),
            "D+M" => Some(CompType::DplusM),
            "D-M" => Some(CompType::DminusM),
            "M-D" => Some(CompType::MminusD),
            "D&M" => Some(CompType::DandM),
            "D|M" => Some(CompType::DorM),
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
