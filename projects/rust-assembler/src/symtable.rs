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

    pub fn build_table(&self, input_stream: &String) {}

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
