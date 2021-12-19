mod code;
mod parser;
mod run;

pub use code::*;
pub use parser::*;
pub use run::*;

pub static DEBUG_MODE: Option<&'static str> = std::option_env!("DEBUG_MODE");
