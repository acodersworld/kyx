use std::fs::File;
use std::io::prelude::*;

const OPCODES: &[&'static str] = &[
    "CONSTANT_INTEGER",
    "CONSTANT_FLOAT",
    //
    "ADDI",
    "SUBI",
    "MULI",
    "DIVI",
    "ADDF",
    "SUBF",
    "MULF",
    "DIVF",
    //
    "PRINT",
    "CONSTANT_STRING",
    "DEFINE_GLOBAL",
    "SET_GLOBAL",
    "PUSH_GLOBAL",
    "POP",
];

fn main() -> std::io::Result<()> {
    println!("cargo:rerun-if-changed=build.rs");

    let mut file = File::create("src/opcode.rs")?;
    for (i, opcode) in OPCODES.iter().enumerate() {
        let line = format!("pub const {}: u8 = {};\n", opcode, i);
        file.write_all(line.as_bytes())?;
    }

    Ok(())
}
