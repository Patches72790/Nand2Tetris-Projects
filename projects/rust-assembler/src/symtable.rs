use std::cell::RefCell;
use std::collections::HashMap;

pub struct SymTable {
    table: RefCell<HashMap<String, u32>>,
    address: u32,
}

impl SymTable {
    pub fn new() -> SymTable {
        SymTable {
            table: RefCell::new(HashMap::new()),
            address: 0,
        }
    }

    pub fn print_symtable(&self) {
        for (k, v) in self.table.borrow().iter() {
            println!("Symbol: {}, Address: {}", k, v);
        }
    }

    fn insert_predefined_symbols(&mut self) {
        self.table.borrow_mut().insert(String::from("SP"), 0);
        self.table.borrow_mut().insert(String::from("LCL"), 1);
        self.table.borrow_mut().insert(String::from("ARG"), 2);
        self.table.borrow_mut().insert(String::from("THIS"), 3);
        self.table.borrow_mut().insert(String::from("THAT"), 4);
        self.table.borrow_mut().insert(String::from("R0"), 0);
        self.table.borrow_mut().insert(String::from("R1"), 1);
        self.table.borrow_mut().insert(String::from("R2"), 2);
        self.table.borrow_mut().insert(String::from("R3"), 3);
        self.table.borrow_mut().insert(String::from("R4"), 4);
        self.table.borrow_mut().insert(String::from("R5"), 5);
        self.table.borrow_mut().insert(String::from("R6"), 6);
        self.table.borrow_mut().insert(String::from("R7"), 7);
        self.table.borrow_mut().insert(String::from("R8"), 8);
        self.table.borrow_mut().insert(String::from("R9"), 9);
        self.table.borrow_mut().insert(String::from("R10"), 10);
        self.table.borrow_mut().insert(String::from("R11"), 11);
        self.table.borrow_mut().insert(String::from("R12"), 12);
        self.table.borrow_mut().insert(String::from("R13"), 13);
        self.table.borrow_mut().insert(String::from("R14"), 14);
        self.table.borrow_mut().insert(String::from("R15"), 15);
        self.table
            .borrow_mut()
            .insert(String::from("SCREEN"), 16384);
        self.table.borrow_mut().insert(String::from("KBD"), 24576);
    }

    pub fn build_table(&mut self, input_stream: &str) {
        self.insert_predefined_symbols();

        input_stream.lines().for_each(|line| {
            if line.len() == 0 || line.trim_start().starts_with("/") {
                return;
            }
            if let Some(sym) = self.read_line(line) {
                self.table.borrow_mut().insert(sym, self.address + 1);
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
        self.table.borrow_mut().insert(symbol.to_string(), address);
    }

    pub fn contains(&self, symbol: &str) -> bool {
        self.table.borrow().contains_key(symbol)
    }

    pub fn get_address(&self, symbol: &str) -> Option<u32> {
        match self.table.borrow().get(symbol) {
            Some(val) => Some(*val),
            None => None,
        }
    }
}
