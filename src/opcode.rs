// OP CODE START
pub const CONSTANT_INTEGER: u8 = 0;
pub const CONSTANT_FLOAT: u8 = 1;
pub const CONSTANT_STRING: u8 = 2;
pub const CONSTANT_BOOL: u8 = 3;
pub const CREATE_VEC: u8 = 4;
pub const CREATE_HASH_MAP: u8 = 5;
pub const CREATE_STRUCT: u8 = 6;
pub const GET_INDEX: u8 = 7;
pub const SET_INDEX: u8 = 8;
pub const GET_FIELD: u8 = 9;
pub const SET_FIELD: u8 = 10;
pub const ADD: u8 = 11;
pub const SUB: u8 = 12;
pub const MUL: u8 = 13;
pub const DIV: u8 = 14;
pub const EQ: u8 = 15;
pub const NEQ: u8 = 16;
pub const LESS: u8 = 17;
pub const LESS_EQUAL: u8 = 18;
pub const GREATER: u8 = 19;
pub const GREATER_EQUAL: u8 = 20;
pub const PRINT: u8 = 21;
pub const DEFINE_GLOBAL: u8 = 22;
pub const DEFINE_LOCAL: u8 = 23;
pub const SET_GLOBAL: u8 = 24;
pub const SET_LOCAL: u8 = 25;
pub const PUSH_GLOBAL: u8 = 26;
pub const PUSH_LOCAL: u8 = 27;
pub const POP: u8 = 28;
pub const LOCAL_POP: u8 = 29;
pub const PUSH_FRAME: u8 = 30;
pub const POP_FRAME: u8 = 31;
pub const LOOP: u8 = 32;
pub const BREAK: u8 = 33;
pub const JMP: u8 = 34;
pub const JMP_IF_FALSE: u8 = 35;
pub const READ_INPUT: u8 = 36;
pub const CALL: u8 = 37;
pub const RETURN: u8 = 38;
// OP CODE END

#[allow(dead_code)]
pub fn to_string(code: u8) -> String {
    match code {
        0 => "CONSTANT_INTEGER".to_owned(),
        1 => "CONSTANT_FLOAT".to_owned(),
        2 => "CONSTANT_STRING".to_owned(),
        3 => "CONSTANT_BOOL".to_owned(),
        4 => "CREATE_VEC".to_owned(),
        5 => "CREATE_HASH_MAP".to_owned(),
        6 => "CREATE_STRUCT".to_owned(),
        7 => "GET_INDEX".to_owned(),
        8 => "SET_INDEX".to_owned(),
        9 => "GET_FIELD".to_owned(),
        10 => "SET_FIELD".to_owned(),
        11 => "ADD".to_owned(),
        12 => "SUB".to_owned(),
        13 => "MUL".to_owned(),
        14 => "DIV".to_owned(),
        15 => "EQ".to_owned(),
        16 => "NEQ".to_owned(),
        17 => "LESS".to_owned(),
        18 => "LESS_EQUAL".to_owned(),
        19 => "GREATER".to_owned(),
        20 => "GREATER_EQUAL".to_owned(),
        21 => "PRINT".to_owned(),
        22 => "DEFINE_GLOBAL".to_owned(),
        23 => "DEFINE_LOCAL".to_owned(),
        24 => "SET_GLOBAL".to_owned(),
        25 => "SET_LOCAL".to_owned(),
        26 => "PUSH_GLOBAL".to_owned(),
        27 => "PUSH_LOCAL".to_owned(),
        28 => "POP".to_owned(),
        29 => "LOCAL_POP".to_owned(),
        30 => "PUSH_FRAME".to_owned(),
        31 => "POP_FRAME".to_owned(),
        32 => "LOOP".to_owned(),
        33 => "BREAK".to_owned(),
        34 => "JMP".to_owned(),
        35 => "JMP_IF_FALSE".to_owned(),
        36 => "READ_INPUT".to_owned(),
        37 => "CALL".to_owned(),
        38 => "RETURN".to_owned(),
        _ => "UNKNOWN".to_owned(),
    }
}
