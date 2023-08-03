use std::vec::Vec;
use crate::compiler::Compiler;
use crate::value::Value;
use crate::value::FromValue;
use crate::opcode;
use crate::var_len_int;
use crate::float;

pub struct VM {
    stack: Vec<Value>,
    offset: usize
}

impl VM {
    pub fn new() -> VM {
        VM {
            stack: Vec::new(),
            offset: 0
        }
    }

    pub fn top(self: &Self) -> &Value {
        self.stack.last().unwrap()
    }

    fn run(self: &mut Self, code: &Vec<u8>) {
        self.offset = 0;
        let len = code.len();

        while self.offset < len {
            println!("Ex instruction: {} {}", code[self.offset], self.offset);
            self.offset += 1;
            match code[self.offset-1] {
                opcode::CONSTANT_INTEGER => self.push_integer_instruction(code),
                opcode::CONSTANT_FLOAT => self.push_float_instruction(code),
                opcode::ADDI => self.integer_add(),
                opcode::SUBI => self.integer_sub(),
                opcode::ADDF => self.float_add(),
                opcode::SUBF => self.float_sub(),
                opcode::PRINT => self.print(),
                _ => {
                    println!("Unknown instruction: {}", code[self.offset])
                }
            }
        }
    }

    pub fn interpret(self: &mut Self, src: &str) -> Result<(), String> {
        let mut compiler = Compiler::new(src);
        compiler.compile()?;

        let chunk = compiler.take_chunk();
        let code = &chunk.code;

        self.run(code);
        Ok(())
    }

    fn binary_op<T, OP>(self: &mut Self, op: OP) 
    where
        T: FromValue<ValueType = T>,
        OP: FnOnce(T, T) -> Value
    {
        let st = &mut self.stack;

        let right = T::from_value(&st.pop().unwrap()).unwrap();
        let left = T::from_value(&st.last().unwrap()).unwrap();

        let result = op(right, left);
        *st.last_mut().unwrap() = result; 
    }

    fn integer_add(self: &mut Self) {
        self.binary_op::<i32, _>(|l, r| { Value::Integer(l + r) });
    }

    fn integer_sub(self: &mut Self) {
        self.binary_op::<i32, _>(|l, r| { Value::Integer(l - r) });
    }

    fn float_add(self: &mut Self) {
        self.binary_op::<f32, _>(|l, r| { Value::Float(l + r) });
    }

    fn float_sub(self: &mut Self) {
        self.binary_op::<f32, _>(|l, r| { Value::Float(l - r) });
    }

    fn push_integer_instruction(self: &mut Self, code: &Vec<u8>) {
        let mut decoder = var_len_int::Decoder::new(); 
        while !decoder.step_decode(code[self.offset]) {
            self.offset += 1;
        }
        self.offset += 1;

        self.stack.push(Value::Integer(decoder.val()));
    }

    fn push_float_instruction(self: &mut Self, code: &Vec<u8>) {
        let value = float::decode(&code[self.offset..self.offset+4].try_into().unwrap());
        self.offset += 4;

        self.stack.push(Value::Float(value));
    }

    fn print(self: &mut Self) {
        let value = self.stack.pop().unwrap();
        println!("{:?}", value);
    }
}

