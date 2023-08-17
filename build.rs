use std::fs::File;
use std::io::prelude::*;

const OPCODES: &[&str] = &[
    "CONSTANT_INTEGER",
    "CONSTANT_FLOAT",
    "CONSTANT_STRING",
    //
    "ADD",
    "SUB",
    "MUL",
    "DIV",
    //
    "EQ",
    "NEQ",
    "LESS",
    "LESS_EQUAL",
    "GREATER",
    "GREATER_EQUAL",
    //
    "PRINT",
    "DEFINE_GLOBAL",
    "DEFINE_LOCAL",
    "SET_GLOBAL",
    "SET_LOCAL",
    "PUSH_GLOBAL",
    "PUSH_LOCAL",
    "POP",
    "LOCAL_POP",
    "PUSH_FRAME",
    "POP_FRAME",
    //
    "LOOP",
    "JMP",
    "JMP_IF_FALSE",
    //
    "READ_INPUT",
];

fn main() -> std::io::Result<()> {
    println!("cargo:rerun-if-changed=build.rs");

    let mut file = File::create("src/opcode.rs")?;
    for (i, opcode) in OPCODES.iter().enumerate() {
        let line = format!("pub const {}: u8 = {};\n", opcode, i);
        file.write_all(line.as_bytes())?;
    }

    file.write_all(b"\n\n")?;

    file.write_all(b"pub fn to_string(code: u8) -> String {\n")?;
    file.write_all(b"   match code {\n")?;

    for (i, opcode) in OPCODES.iter().enumerate() {
        let line = format!("        {} => \"{}\".to_owned(),\n", i, opcode);
        file.write_all(line.as_bytes())?;
    }
    file.write_all(b"       _ => \"UNKNOWN\".to_owned(),\n")?;
    file.write_all(b"   }\n")?;
    file.write_all(b"}\n")?;

    Ok(())
}
