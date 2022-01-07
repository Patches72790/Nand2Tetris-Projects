mod code;
mod parser;
mod run;
mod symtable;

pub use code::*;
pub use parser::*;
pub use run::*;
pub use symtable::*;

pub static DEBUG_MODE: Option<&'static str> = std::option_env!("DEBUG_MODE");
