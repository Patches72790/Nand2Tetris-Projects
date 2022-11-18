const SP_BASE: u8 = 0;
const LCL_BASE: u8 = 1;
const ARG_BASE: u8 = 2;
const THIS_BASE: u8 = 3;
const THAT_BASE: u8 = 4;
const TEMP_BASE: u8 = 5;
const R13: u8 = 13;
const R14: u8 = 14;
const R15: u8 = 15;

#[macro_export]
macro_rules! write_a {
    ($i:ident) => {
        format!("@{}", $i)
    };
}

#[macro_export]
macro_rules! a_literal {
    ($l:literal) => {
        format!("@{}", $l)
    };
}

#[macro_export]
macro_rules! write_c {
    ($l:literal) => {
        format!("{}", $l)
    };
}

#[macro_export]
macro_rules! pop_stack {
    ($register:literal) => {
        vec![
            a_literal!("SP"), // SP--
            write_c!("M=M-1"),
            a_literal!("SP"), // RAM[addr] = RAM[SP]
            write_c!("A=M"),
            write_c!("D=M"),
            format!("@{}", $register),
            write_c!("M=D"),
        ]
    };
}

#[macro_export]
macro_rules! push_stack {
    ($register:literal) => {
        vec![format!("@{}", $register)]
        //todo
    };
}
