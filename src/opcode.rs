
macro_rules! OP {
    ($name:ident, $value:expr) => {
        pub const $name: u8 = $value;
    };
}

OP!(CONSTANT_INTEGER,   0);
OP!(CONSTANT_FLOAT,     1);

OP!(ADD,        2);
OP!(SUBTRACT,   3);
OP!(MULTIPLY,   4);
OP!(DIVIDE,     5);
