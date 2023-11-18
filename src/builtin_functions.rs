pub mod vector {
    pub const LEN: u8 = 0;
}

pub mod string {
    pub const LEN: u8 = 0;
    pub const TO_INTEGER: u8 = 1;
    pub const SUBSTR: u8 = 2;
    pub const SUBSTR_FROM_START: u8 = 3;
    pub const SUBSTR_TO_END: u8 = 4;
    pub const SPLIT: u8 = 5;
    pub const TRIM: u8 = 6;
    pub const TRIM_START: u8 = 7;
    pub const TRIM_END: u8 = 8;
}

pub mod hashmap {
    pub const CONTAINS_KEY: u8 = 0;
    pub const KEYS: u8 = 1;
}
