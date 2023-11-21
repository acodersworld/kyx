// OP CODE START
pub const CONSTANT_INTEGER: u8 = 0;
pub const CONSTANT_FLOAT: u8 = 1;
pub const CONSTANT_STRING: u8 = 2;
pub const CONSTANT_BOOL: u8 = 3;
pub const CONSTANT_CHAR: u8 = 4;
pub const CREATE_VEC: u8 = 5;
pub const CREATE_HASH_MAP: u8 = 6;
pub const CREATE_STRUCT: u8 = 7;
pub const CREATE_UNION: u8 = 8;
pub const CREATE_TUPLE: u8 = 9;
pub const GET_INDEX: u8 = 10;
pub const SET_INDEX: u8 = 11;
pub const GET_FIELD: u8 = 12;
pub const SET_FIELD: u8 = 13;
pub const NOT: u8 = 14;
pub const ADD: u8 = 15;
pub const SUB: u8 = 16;
pub const MUL: u8 = 17;
pub const DIV: u8 = 18;
pub const MOD: u8 = 19;
pub const EQ: u8 = 20;
pub const NEQ: u8 = 21;
pub const LESS: u8 = 22;
pub const LESS_EQUAL: u8 = 23;
pub const GREATER: u8 = 24;
pub const GREATER_EQUAL: u8 = 25;
pub const PRINT: u8 = 26;
pub const DEFINE_GLOBAL: u8 = 27;
pub const DEFINE_LOCAL: u8 = 28;
pub const SET_GLOBAL: u8 = 29;
pub const SET_LOCAL: u8 = 30;
pub const PUSH_GLOBAL: u8 = 31;
pub const PUSH_LOCAL: u8 = 32;
pub const PUSH_METHOD: u8 = 33;
pub const POP: u8 = 34;
pub const LOCAL_POP: u8 = 35;
pub const PUSH_FRAME: u8 = 36;
pub const POP_FRAME: u8 = 37;
pub const LOOP: u8 = 38;
pub const BREAK: u8 = 39;
pub const JMP: u8 = 40;
pub const JMP_IF_FALSE: u8 = 41;
pub const JMP_IF_DETERMINANT_MISMATCH: u8 = 42;
pub const READ_INPUT: u8 = 43;
pub const CALL: u8 = 44;
pub const CALL_INTERFACE: u8 = 45;
pub const CALL_BUILTIN: u8 = 46;
pub const RETURN: u8 = 47;
// OP CODE END

#[allow(dead_code)]
pub fn to_string(code: u8) -> String {
    match code {
        0 => "CONSTANT_INTEGER".to_owned(),
        1 => "CONSTANT_FLOAT".to_owned(),
        2 => "CONSTANT_STRING".to_owned(),
        3 => "CONSTANT_BOOL".to_owned(),
        4 => "CONSTANT_CHAR".to_owned(),
        5 => "CREATE_VEC".to_owned(),
        6 => "CREATE_HASH_MAP".to_owned(),
        7 => "CREATE_STRUCT".to_owned(),
        8 => "CREATE_UNION".to_owned(),
        9 => "CREATE_TUPLE".to_owned(),
        10 => "GET_INDEX".to_owned(),
        11 => "SET_INDEX".to_owned(),
        12 => "GET_FIELD".to_owned(),
        13 => "SET_FIELD".to_owned(),
        14 => "NOT".to_owned(),
        15 => "ADD".to_owned(),
        16 => "SUB".to_owned(),
        17 => "MUL".to_owned(),
        18 => "DIV".to_owned(),
        19 => "MOD".to_owned(),
        20 => "EQ".to_owned(),
        21 => "NEQ".to_owned(),
        22 => "LESS".to_owned(),
        23 => "LESS_EQUAL".to_owned(),
        24 => "GREATER".to_owned(),
        25 => "GREATER_EQUAL".to_owned(),
        26 => "PRINT".to_owned(),
        27 => "DEFINE_GLOBAL".to_owned(),
        28 => "DEFINE_LOCAL".to_owned(),
        29 => "SET_GLOBAL".to_owned(),
        30 => "SET_LOCAL".to_owned(),
        31 => "PUSH_GLOBAL".to_owned(),
        32 => "PUSH_LOCAL".to_owned(),
        33 => "PUSH_METHOD".to_owned(),
        34 => "POP".to_owned(),
        35 => "LOCAL_POP".to_owned(),
        36 => "PUSH_FRAME".to_owned(),
        37 => "POP_FRAME".to_owned(),
        38 => "LOOP".to_owned(),
        39 => "BREAK".to_owned(),
        40 => "JMP".to_owned(),
        41 => "JMP_IF_FALSE".to_owned(),
        42 => "JMP_IF_DETERMINANT_MISMATCH".to_owned(),
        43 => "READ_INPUT".to_owned(),
        44 => "CALL".to_owned(),
        45 => "CALL_INTERFACE".to_owned(),
        46 => "CALL_BUILTIN".to_owned(),
        47 => "RETURN".to_owned(),
        _ => "UNKNOWN".to_owned(),
    }
}
