// OP CODE START
pub const CONSTANT_INTEGER: u8 = 0;
pub const CONSTANT_FLOAT: u8 = 1;
pub const CONSTANT_STRING: u8 = 2;
pub const CONSTANT_BOOL: u8 = 3;
pub const ADD: u8 = 4;
pub const SUB: u8 = 5;
pub const MUL: u8 = 6;
pub const DIV: u8 = 7;
pub const EQ: u8 = 8;
pub const NEQ: u8 = 9;
pub const LESS: u8 = 10;
pub const LESS_EQUAL: u8 = 11;
pub const GREATER: u8 = 12;
pub const GREATER_EQUAL: u8 = 13;
pub const PRINT: u8 = 14;
pub const DEFINE_GLOBAL: u8 = 15;
pub const DEFINE_LOCAL: u8 = 16;
pub const SET_GLOBAL: u8 = 17;
pub const SET_LOCAL: u8 = 18;
pub const PUSH_GLOBAL: u8 = 19;
pub const PUSH_LOCAL: u8 = 20;
pub const POP: u8 = 21;
pub const LOCAL_POP: u8 = 22;
pub const PUSH_FRAME: u8 = 23;
pub const POP_FRAME: u8 = 24;
pub const LOOP: u8 = 25;
pub const BREAK: u8 = 26;
pub const JMP: u8 = 27;
pub const JMP_IF_FALSE: u8 = 28;
pub const READ_INPUT: u8 = 29;
// OP CODE END

#[allow(dead_code)]
pub fn to_string(code: u8) -> String {
    match code {
        0 => "CONSTANT_INTEGER".to_owned(),
        1 => "CONSTANT_FLOAT".to_owned(),
        2 => "CONSTANT_STRING".to_owned(),
        3 => "CONSTANT_BOOL".to_owned(),
        4 => "ADD".to_owned(),
        5 => "SUB".to_owned(),
        6 => "MUL".to_owned(),
        7 => "DIV".to_owned(),
        8 => "EQ".to_owned(),
        9 => "NEQ".to_owned(),
        10 => "LESS".to_owned(),
        11 => "LESS_EQUAL".to_owned(),
        12 => "GREATER".to_owned(),
        13 => "GREATER_EQUAL".to_owned(),
        14 => "PRINT".to_owned(),
        15 => "DEFINE_GLOBAL".to_owned(),
        16 => "DEFINE_LOCAL".to_owned(),
        17 => "SET_GLOBAL".to_owned(),
        18 => "SET_LOCAL".to_owned(),
        19 => "PUSH_GLOBAL".to_owned(),
        20 => "PUSH_LOCAL".to_owned(),
        21 => "POP".to_owned(),
        22 => "LOCAL_POP".to_owned(),
        23 => "PUSH_FRAME".to_owned(),
        24 => "POP_FRAME".to_owned(),
        25 => "LOOP".to_owned(),
        26 => "BREAK".to_owned(),
        27 => "JMP".to_owned(),
        28 => "JMP_IF_FALSE".to_owned(),
        29 => "READ_INPUT".to_owned(),
        _ => "UNKNOWN".to_owned(),
    }
}
