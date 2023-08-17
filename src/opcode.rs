pub const CONSTANT_INTEGER: u8 = 0;
pub const CONSTANT_FLOAT: u8 = 1;
pub const CONSTANT_STRING: u8 = 2;
pub const ADD: u8 = 3;
pub const SUB: u8 = 4;
pub const MUL: u8 = 5;
pub const DIV: u8 = 6;
pub const EQ: u8 = 7;
pub const NEQ: u8 = 8;
pub const LESS: u8 = 9;
pub const LESS_EQUAL: u8 = 10;
pub const GREATER: u8 = 11;
pub const GREATER_EQUAL: u8 = 12;
pub const PRINT: u8 = 13;
pub const DEFINE_GLOBAL: u8 = 14;
pub const DEFINE_LOCAL: u8 = 15;
pub const SET_GLOBAL: u8 = 16;
pub const SET_LOCAL: u8 = 17;
pub const PUSH_GLOBAL: u8 = 18;
pub const PUSH_LOCAL: u8 = 19;
pub const POP: u8 = 20;
pub const LOCAL_POP: u8 = 21;
pub const PUSH_FRAME: u8 = 22;
pub const POP_FRAME: u8 = 23;
pub const LOOP: u8 = 24;
pub const JMP: u8 = 25;
pub const JMP_IF_FALSE: u8 = 26;
pub const READ_INPUT: u8 = 27;


pub fn to_string(code: u8) -> String {
   match code {
        0 => "CONSTANT_INTEGER".to_owned(),
        1 => "CONSTANT_FLOAT".to_owned(),
        2 => "CONSTANT_STRING".to_owned(),
        3 => "ADD".to_owned(),
        4 => "SUB".to_owned(),
        5 => "MUL".to_owned(),
        6 => "DIV".to_owned(),
        7 => "EQ".to_owned(),
        8 => "NEQ".to_owned(),
        9 => "LESS".to_owned(),
        10 => "LESS_EQUAL".to_owned(),
        11 => "GREATER".to_owned(),
        12 => "GREATER_EQUAL".to_owned(),
        13 => "PRINT".to_owned(),
        14 => "DEFINE_GLOBAL".to_owned(),
        15 => "DEFINE_LOCAL".to_owned(),
        16 => "SET_GLOBAL".to_owned(),
        17 => "SET_LOCAL".to_owned(),
        18 => "PUSH_GLOBAL".to_owned(),
        19 => "PUSH_LOCAL".to_owned(),
        20 => "POP".to_owned(),
        21 => "LOCAL_POP".to_owned(),
        22 => "PUSH_FRAME".to_owned(),
        23 => "POP_FRAME".to_owned(),
        24 => "LOOP".to_owned(),
        25 => "JMP".to_owned(),
        26 => "JMP_IF_FALSE".to_owned(),
        27 => "READ_INPUT".to_owned(),
       _ => "UNKNOWN".to_owned(),
   }
}
