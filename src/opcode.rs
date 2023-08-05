
macro_rules! OP {
    ($name:ident, $value:expr) => {
        pub const $name: u8 = $value;
    };
}

OP!(CONSTANT_INTEGER,   0);
OP!(CONSTANT_FLOAT,     1);

OP!(ADDI, 2);
OP!(SUBI, 3);
OP!(MULI, 4);
OP!(DIVI, 5);

OP!(ADDF, 6);
OP!(SUBF, 7);
OP!(MULF, 8);
OP!(DIVF, 9);

OP!(PRINT, 10);
OP!(CONSTANT_STRING,     11);
