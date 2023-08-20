// OP CODE START
pub const CONSTANT_INTEGER: u8 = 0;
pub const CONSTANT_FLOAT: u8 = 1;
pub const CONSTANT_STRING: u8 = 2;
pub const CONSTANT_BOOL: u8 = 3;
pub const CREATE_VEC: u8 = 4;
pub const INDEX_VEC: u8 = 5;
pub const SET_VEC: u8 = 6;
pub const CREATE_HASH_MAP: u8 = 7;
pub const INDEX_HASH_MAP: u8 = 8;
pub const SET_HASH_MAP: u8 = 9;
pub const ADD: u8 = 10;
pub const SUB: u8 = 11;
pub const MUL: u8 = 12;
pub const DIV: u8 = 13;
pub const EQ: u8 = 14;
pub const NEQ: u8 = 15;
pub const LESS: u8 = 16;
pub const LESS_EQUAL: u8 = 17;
pub const GREATER: u8 = 18;
pub const GREATER_EQUAL: u8 = 19;
pub const PRINT: u8 = 20;
pub const DEFINE_GLOBAL: u8 = 21;
pub const DEFINE_LOCAL: u8 = 22;
pub const SET_GLOBAL: u8 = 23;
pub const SET_LOCAL: u8 = 24;
pub const PUSH_GLOBAL: u8 = 25;
pub const PUSH_LOCAL: u8 = 26;
pub const POP: u8 = 27;
pub const LOCAL_POP: u8 = 28;
pub const PUSH_FRAME: u8 = 29;
pub const POP_FRAME: u8 = 30;
pub const LOOP: u8 = 31;
pub const BREAK: u8 = 32;
pub const JMP: u8 = 33;
pub const JMP_IF_FALSE: u8 = 34;
pub const READ_INPUT: u8 = 35;
// OP CODE END

#[allow(dead_code)]
pub fn to_string(code: u8) -> String {
    match code {
        0 => "CONSTANT_INTEGER".to_owned(),
        1 => "CONSTANT_FLOAT".to_owned(),
        2 => "CONSTANT_STRING".to_owned(),
        3 => "CONSTANT_BOOL".to_owned(),
        4 => "CREATE_VEC".to_owned(),
        5 => "INDEX_VEC".to_owned(),
        6 => "SET_VEC".to_owned(),
        7 => "CREATE_HASH_MAP".to_owned(),
        8 => "INDEX_HASH_MAP".to_owned(),
        9 => "SET_HASH_MAP".to_owned(),
        10 => "ADD".to_owned(),
        11 => "SUB".to_owned(),
        12 => "MUL".to_owned(),
        13 => "DIV".to_owned(),
        14 => "EQ".to_owned(),
        15 => "NEQ".to_owned(),
        16 => "LESS".to_owned(),
        17 => "LESS_EQUAL".to_owned(),
        18 => "GREATER".to_owned(),
        19 => "GREATER_EQUAL".to_owned(),
        20 => "PRINT".to_owned(),
        21 => "DEFINE_GLOBAL".to_owned(),
        22 => "DEFINE_LOCAL".to_owned(),
        23 => "SET_GLOBAL".to_owned(),
        24 => "SET_LOCAL".to_owned(),
        25 => "PUSH_GLOBAL".to_owned(),
        26 => "PUSH_LOCAL".to_owned(),
        27 => "POP".to_owned(),
        28 => "LOCAL_POP".to_owned(),
        29 => "PUSH_FRAME".to_owned(),
        30 => "POP_FRAME".to_owned(),
        31 => "LOOP".to_owned(),
        32 => "BREAK".to_owned(),
        33 => "JMP".to_owned(),
        34 => "JMP_IF_FALSE".to_owned(),
        35 => "READ_INPUT".to_owned(),
        _ => "UNKNOWN".to_owned(),
    }
}
