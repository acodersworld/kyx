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

    pub fn disassemble(&mut self) {
        while self.offset < self.code.len() {
            let offset = self.offset;
            self.offset += 1;

            print!("{: >4} | ", offset);
            match self.code[offset] {
                opcode::CONSTANT_INTEGER => self.constant_integer_instruction(),
                opcode::CONSTANT_FLOAT => self.constant_float_instruction(),
                opcode::CONSTANT_STRING => self.constant_string_instruction(),
                opcode::SET_GLOBAL => self.set_global(),
                opcode::SET_LOCAL => self.set_local(),
                opcode::PUSH_GLOBAL => self.push_global(),
                opcode::PUSH_LOCAL => self.push_local(),
                opcode::DEFINE_GLOBAL => self.define_global(),
                opcode::DEFINE_LOCAL => self.define_local(),
                opcode::ADDI => self.simple_instruction("addi"),
                opcode::SUBI => self.simple_instruction("subi"),
                opcode::MULI => self.simple_instruction("muli"),
                opcode::DIVI => self.simple_instruction("divi"),
                opcode::ADDF => self.simple_instruction("addf"),
                opcode::SUBF => self.simple_instruction("subf"),
                opcode::MULF => self.simple_instruction("mulf"),
                opcode::DIVF => self.simple_instruction("divf"),
                opcode::EQI => self.simple_instruction("eqi"),
                opcode::EQF => self.simple_instruction("eqf"),
                opcode::EQS => self.simple_instruction("eqs"),
                opcode::NEQI => self.simple_instruction("neqi"),
                opcode::NEQF => self.simple_instruction("neqf"),
                opcode::NEQS => self.simple_instruction("neqs"),
                opcode::PRINT => self.simple_instruction("print"),
                opcode::POP => self.simple_instruction("pop"),
                opcode::LOCAL_POP => self.simple_instruction("local pop"),
                opcode::PUSH_FRAME => self.simple_instruction("push frame"),
                opcode::POP_FRAME => self.simple_instruction("pop frame"),
                opcode::JMP => self.jmp_instruction("jump"),
                opcode::JMP_IF_FALSE => self.jmp_instruction("jump if false"),
                opcode::READ_INPUT => self.read_instruction(),
                code => {
                    println!("Unknown instruction {}", code);
                    return;
                }
            }
        }
    }

    fn simple_instruction(&mut self, name: &str) {
        println!("{}", name);
    }

    fn jmp_instruction(&mut self, name: &str) {
        let jmp_offset = self.code[self.offset];
        println!(
            "{}: {} -> {}",
            name,
            jmp_offset,
            self.offset + jmp_offset as usize
        );
        self.offset += 1;
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

    fn define_global(&mut self) {
        let idx = self.code[self.offset];
        self.offset += 1;

        println!("DEFINE GLOBAL idx({})", idx);
    }

    fn define_local(&mut self) {
        println!("DEFINE LOCAL");
    }
}
