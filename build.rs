use std::fs::File;
use std::io::prelude::*;

const OPCODES: &[&str] = &[
    "CONSTANT_INTEGER",
    "CONSTANT_FLOAT",
    "CONSTANT_STRING",
    "CONSTANT_BOOL",
    //
    "CREATE_VEC",
    "CREATE_HASH_MAP",
    "CREATE_STRUCT",
    "CREATE_UNION",
    "CREATE_TUPLE",
    //
    "GET_INDEX",
    "SET_INDEX",
    "GET_FIELD",
    "SET_FIELD",
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
    "PUSH_METHOD",
    "POP",
    "LOCAL_POP",
    "PUSH_FRAME",
    "POP_FRAME",
    //
    "LOOP",
    "BREAK",
    "JMP",
    "JMP_IF_FALSE",
    "JMP_IF_DETERMINANT_MISMATCH",
    //
    "READ_INPUT",
    //
    "CALL",
    "CALL_INTERFACE",
    "RETURN",
];

fn main() -> std::io::Result<()> {
    println!("cargo:rerun-if-changed=build.rs");

    let mut file = File::create("src/opcode.rs")?;
    file.write_all(b"// OP CODE START\n")?;
    for (i, opcode) in OPCODES.iter().enumerate() {
        let line = format!("pub const {}: u8 = {};\n", opcode, i);
        file.write_all(line.as_bytes())?;
    }
    file.write_all(b"// OP CODE END\n")?;

    file.write_all(b"\n")?;

    file.write_all(b"#[allow(dead_code)]\n")?;
    file.write_all(b"pub fn to_string(code: u8) -> String {\n")?;
    file.write_all(b"    match code {\n")?;

    for (i, opcode) in OPCODES.iter().enumerate() {
        let line = format!("        {} => \"{}\".to_owned(),\n", i, opcode);
        file.write_all(line.as_bytes())?;
    }
    file.write_all(b"        _ => \"UNKNOWN\".to_owned(),\n")?;
    file.write_all(b"    }\n")?;
    file.write_all(b"}\n")?;

    Ok(())
}
