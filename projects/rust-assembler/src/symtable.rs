use std::collections::HashMap;

pub struct SymTable {
    table: HashMap<String, u32>,
    address: u32,
}

impl SymTable {
    pub fn new() -> SymTable {
        SymTable {
            table: HashMap::new(),
            address: 0,
        }
    }

    pub fn print_symtable(&self) {
        for (k, v) in self.table.iter() {
            println!("Symbol: {}, Address: {}", k, v);
        }
    }

    pub fn build_table(&mut self, input_stream: &str) {
        input_stream.lines().for_each(|line| {
            if line.len() == 0 || line.trim_start().starts_with("/") {
                return;
            }
            if let Some(sym) = self.read_line(line) {
                self.table.insert(sym, self.address + 1);
            }
        })
    }

    fn read_line(&mut self, line: &str) -> Option<String> {
        let mut chars = line.chars();
        let first_char = chars
            .next()
            .expect("Error reading first character of line!");

        match first_char {
            '(' => Some(String::from(
                &chars
                    .take_while(|c| *c != ')')
                    .collect::<String>()
                    .to_string(),
            )),
            _ => {
                self.address += 1;
                None
            }
        }
    }

    pub fn add_entry(&mut self, symbol: &str, address: u32) {
        self.table.insert(symbol.to_string(), address);
    }

    pub fn contains(&self, symbol: &str) -> bool {
        self.table.contains_key(symbol)
    }

    pub fn get_address(&self, symbol: &str) -> Option<u32> {
        match self.table.get(symbol) {
            Some(val) => Some(*val),
            None => None,
        }
    }
}
