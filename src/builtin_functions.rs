pub mod vector {
    pub const LEN: u8 = 0;
    pub const PUSH: u8 = 1;
    pub const POP: u8 = 2;
    pub const SORT: u8 = 3;
    pub const CLEAR: u8 = 4;
    pub const CLONE: u8 = 5;
    pub const REMOVE: u8 = 6;
    pub const CONTAINS: u8 = 7;
}

pub mod string {
    pub const LEN: u8 = 0;
    pub const TO_INTEGER: u8 = 1;

    pub const SUBSTR: u8 = 2;
    pub const SUBSTR_FROM_START: u8 = 3;
    pub const SUBSTR_TO_END: u8 = 4;

    pub const SPLIT: u8 = 5;
    pub const SPLIT_WHITESPACE: u8 = 6;

    pub const TRIM: u8 = 7;
    pub const TRIM_START: u8 = 8;
    pub const TRIM_END: u8 = 9;

    pub const STARTS_WITH: u8 = 10;
    pub const ENDS_WITH: u8 = 11;
    pub const CONTAINS: u8 = 12;
    pub const REMOVE_CHAR: u8 = 13;
    pub const REPLACE_CHAR: u8 = 14;

    pub const APPEND: u8 = 15;
}

pub mod hashmap {
    pub const LEN: u8 = 0;
    pub const CONTAINS_KEY: u8 = 1;
    pub const KEYS: u8 = 2;
    pub const CLEAR: u8 = 3;
    pub const VALUES: u8 = 4;
    pub const REMOVE_ENTRY: u8 = 5;
}

pub mod ch {
    pub const TO_LOWERCASE: u8 = 0;
    pub const TO_UPPERCASE: u8 = 1;
    pub const TO_INTEGER: u8 = 2;
    pub const IS_DIGIT: u8 = 3;
    pub const ASCII_CODE: u8 = 4;
}

pub mod integer {
    pub const ABS: u8 = 0;
    pub const TO_STRING: u8 = 1;
}
