use crate::float;
use crate::opcode;
use crate::read_input_type;
use crate::var_len_int;

pub struct Disassembler<'a> {
    code: &'a [u8],
    offset: usize,
}

impl<'a> Disassembler<'a> {
    pub fn new(code: &'a [u8]) -> Disassembler {
        Disassembler { code, offset: 0 }
    }

    pub fn set_offset(&mut self, offset: usize) {
        self.offset = offset;
    }

    pub fn disassemble_all(&mut self) {
        while self.offset < self.code.len() {
            self.disassemble_one();
        }
    }

    pub fn disassemble_one(&mut self) {
        let offset = self.offset;
        self.offset += 1;

        print!("{: >4} | ", offset);
        match self.code[offset] {
            opcode::CONSTANT_INTEGER => self.constant_integer_instruction(),
            opcode::CONSTANT_FLOAT => self.constant_float_instruction(),
            opcode::CONSTANT_STRING => self.constant_string_instruction(),
            opcode::CONSTANT_BOOL => self.constant_bool_instruction(),
            opcode::CREATE_VEC => self.create_vec(),
            opcode::CREATE_HASH_MAP => self.create_hash_map(),
            opcode::CREATE_TUPLE => self.create_tuple(),
            opcode::CREATE_STRUCT => self.create_struct(),
            opcode::CREATE_UNION => self.create_union(),
            opcode::GET_INDEX => self.simple_instruction("get index"),
            opcode::SET_INDEX => self.simple_instruction("set vec"),
            opcode::GET_FIELD => self.get_field(),
            opcode::SET_FIELD => self.set_field(),
            opcode::SET_GLOBAL => self.set_global(),
            opcode::SET_LOCAL => self.set_local(),
            opcode::PUSH_GLOBAL => self.push_global(),
            opcode::PUSH_LOCAL => self.push_local(),
            opcode::PUSH_METHOD => self.push_method(),
            opcode::DEFINE_GLOBAL => self.define_global(),
            opcode::DEFINE_LOCAL => self.define_local(),
            opcode::ADD => self.simple_instruction("add"),
            opcode::SUB => self.simple_instruction("sub"),
            opcode::MUL => self.simple_instruction("mul"),
            opcode::DIV => self.simple_instruction("div"),
            opcode::EQ => self.simple_instruction("eq"),
            opcode::NEQ => self.simple_instruction("neq"),
            opcode::LESS => self.simple_instruction("less"),
            opcode::LESS_EQUAL => self.simple_instruction("less equal"),
            opcode::GREATER => self.simple_instruction("greater"),
            opcode::GREATER_EQUAL => self.simple_instruction("greater equal"),
            opcode::PRINT => self.simple_instruction("print"),
            opcode::POP => self.simple_instruction("pop"),
            opcode::LOCAL_POP => self.simple_instruction("local pop"),
            opcode::PUSH_FRAME => self.simple_instruction("push frame"),
            opcode::POP_FRAME => self.simple_instruction("pop frame"),
            opcode::LOOP => self.jmp_instruction("loop", -1),
            opcode::BREAK => self.jmp_instruction("break", 1),
            opcode::JMP => self.jmp_instruction("jump", 1),
            opcode::JMP_IF_FALSE => self.jmp_instruction("jump if false", 1),
            opcode::JMP_IF_DETERMINANT_MISMATCH => self.jmp_if_determinant_mismatch(),
            opcode::READ_INPUT => self.read_instruction(),
            opcode::CALL => self.call(),
            opcode::RETURN => self.simple_instruction("return"),
            code => {
                panic!("Unknown instruction {}", code);
            }
        }
    }

    fn simple_instruction(&mut self, name: &str) {
        println!("{}", name);
    }

    fn jmp_instruction(&mut self, name: &str, sign: i64) {
        let jmp_offset = self.code[self.offset] as i64 * sign;
        println!(
            "{}: {} -> {}",
            name,
            jmp_offset,
            self.offset as i64 + jmp_offset
        );
        self.offset += 1;
    }

    fn jmp_if_determinant_mismatch(&mut self) {
        let member_idx = self.code[self.offset] as i64;
        let jmp_offset = self.code[self.offset + 1] as i64;
        println!(
            "jmp if determinant mismatch ({}): {} -> {}",
            member_idx,
            jmp_offset,
            self.offset as i64 + jmp_offset
        );
        self.offset += 2;
    }

    fn read_instruction(&mut self) {
        let read_type = self.code[self.offset];
        self.offset += 1;

        let read_type = match read_type {
            read_input_type::INTEGER => "integer",
            read_input_type::FLOAT => "float",
            read_input_type::STRING => "string",
            _ => "UNKNOWN",
        };

        println!("read {}", read_type);
    }

    fn constant_integer_instruction(&mut self) {
        let mut decoder = var_len_int::Decoder::new();
        while !decoder.step_decode(self.code[self.offset]) {
            self.offset += 1;
        }
        self.offset += 1;

        println!("CONSTANT INT: {}", decoder.val());
    }

    fn constant_float_instruction(&mut self) {
        let value = float::decode(&self.code[self.offset..self.offset + 4].try_into().unwrap());
        self.offset += 4;

        println!("CONSTANT FLOAT: {}", value);
    }

    fn constant_string_instruction(&mut self) {
        let idx = self.code[self.offset];
        self.offset += 1;

        println!("CONSTANT STRING: idx({})", idx);
    }

    fn constant_bool_instruction(&mut self) {
        let value = self.code[self.offset];
        self.offset += 1;

        println!("CONSTANT BOOL: {}", if value != 0 { true } else { false });
    }

    fn create_vec(&mut self) {
        let arg_count = self.code[self.offset];
        self.offset += 1;

        println!("CREATE VEC: {}", arg_count);
    }

    fn set_global(&mut self) {
        let idx = self.code[self.offset];
        self.offset += 1;

        println!("SET GLOBAL: idx({})", idx);
    }

    fn set_local(&mut self) {
        let idx = self.code[self.offset];
        self.offset += 1;

        println!("SET LOCAL: idx({})", idx);
    }

    fn get_field(&mut self) {
        let idx = self.code[self.offset];
        self.offset += 1;

        println!("GET FIELD: idx({})", idx);
    }

    fn set_field(&mut self) {
        let idx = self.code[self.offset];
        self.offset += 1;

        println!("SET FIELD: idx({})", idx);
    }

    fn create_hash_map(&mut self) {
        let arg_count = self.code[self.offset];
        self.offset += 1;

        println!("CREATE HASH MAP: {}", arg_count);
    }

    fn create_tuple(&mut self) {
        let elem_count = self.code[self.offset];
        self.offset += 1;

        println!("CREATE TUPLE: {}", elem_count);
    }

    fn create_struct(&mut self) {
        let arg_count = self.code[self.offset];
        self.offset += 1 + arg_count as usize;

        println!("CREATE STRUCT: {}", arg_count);
    }

    fn create_union(&mut self) {
        let member_count = self.code[self.offset];
        self.offset += 1;

        let idx = self.code[self.offset];
        self.offset += 1;

        println!("CREATE UNION: {} {}", member_count, idx);
    }

    fn push_global(&mut self) {
        let idx = self.code[self.offset];
        self.offset += 1;

        println!("PUSH GLOBAL: idx({})", idx);
    }

    fn push_local(&mut self) {
        let idx = self.code[self.offset];
        self.offset += 1;

        println!("PUSH LOCAL: idx({})", idx);
    }

    fn push_method(&mut self) {
        let mut idx: u16 = self.code[self.offset] as u16;
        self.offset += 1;
        idx |= (self.code[self.offset] as u16) << 8;
        self.offset += 1;

        println!("PUSH METHOD: idx({})", idx);
    }

    fn define_global(&mut self) {
        let idx = self.code[self.offset];
        self.offset += 1;

        println!("DEFINE GLOBAL idx({})", idx);
    }

    fn define_local(&mut self) {
        println!("DEFINE LOCAL");
    }

    fn call(&mut self) {
        let arity = self.code[self.offset];
        self.offset += 1;
        println!("CALL: arity({})", arity);
    }
}
