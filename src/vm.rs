use std::vec::Vec;
use std::ptr::NonNull;

use crate::compiler::Compiler;
use crate::value::{Value, ManagedValue, FromValue, StringValue};
use crate::opcode;
use crate::var_len_int;
use crate::float;
use crate::disassembler;

pub struct VM {
    stack: Vec<Value>,
    objects: Vec<ManagedValue>,
    constant_strs: Vec<NonNull<StringValue>>,
    offset: usize
}

impl VM {
    pub fn new() -> VM {
        VM {
            stack: Vec::new(),
            objects: Vec::new(),
            constant_strs: Vec::new(),
            offset: 0
        }
    }

    pub fn top(self: &Self) -> &Value {
        self.stack.last().unwrap()
    }

    pub fn create_constant_str(self: &mut Self, s: &str) -> u8 {
        let mut str_val = Box::new(StringValue { val: s.to_string(), hash: 0 });
        let ptr = unsafe { NonNull::new_unchecked(str_val.as_mut() as *mut _) };
        self.objects.push(ManagedValue::Str(str_val));

        self.constant_strs.push(ptr);
        (self.constant_strs.len() - 1) as u8
    }

    fn run(self: &mut Self, code: &Vec<u8>) {
        self.offset = 0;
        let len = code.len();

        while self.offset < len {
            self.offset += 1;
            match code[self.offset-1] {
                opcode::CONSTANT_INTEGER => self.push_integer(code),
                opcode::CONSTANT_FLOAT => self.push_float(code),
                opcode::CONSTANT_STRING => self.push_constant_string(code),
                opcode::ADDI => self.integer_add(),
                opcode::SUBI => self.integer_sub(),
                opcode::MULI => self.integer_mul(),
                opcode::DIVI => self.integer_div(),
                opcode::ADDF => self.float_add(),
                opcode::SUBF => self.float_sub(),
                opcode::MULF => self.float_mul(),
                opcode::DIVF => self.float_div(),
                opcode::PRINT => self.print(),
                _ => {
                    println!("Unknown instruction: {}", code[self.offset])
                }
            }
        }
    }

    pub fn interpret(self: &mut Self, src: &str) -> Result<(), String> {
        let mut compiler = Compiler::new(self, src);
        compiler.compile()?;

        let chunk = compiler.take_chunk();
        let code = &chunk.code;

        self.run(code);
        let mut ds = disassembler::Disassembler::new(code);
        ds.disassemble();
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

    fn integer_mul(self: &mut Self) {
        self.binary_op::<i32, _>(|l, r| { Value::Integer(l * r) });
    }

    fn integer_div(self: &mut Self) {
        self.binary_op::<i32, _>(|l, r| { Value::Integer(l / r) });
    }

    fn float_add(self: &mut Self) {
        self.binary_op::<f32, _>(|l, r| { Value::Float(l + r) });
    }

    fn float_sub(self: &mut Self) {
        self.binary_op::<f32, _>(|l, r| { Value::Float(l - r) });
    }

    fn float_mul(self: &mut Self) {
        self.binary_op::<f32, _>(|l, r| { Value::Float(l * r) });
    }

    fn float_div(self: &mut Self) {
        self.binary_op::<f32, _>(|l, r| { Value::Float(l / r) });
    }

    fn push_integer(self: &mut Self, code: &Vec<u8>) {
        let mut decoder = var_len_int::Decoder::new(); 
        while !decoder.step_decode(code[self.offset]) {
            self.offset += 1;
        }
        self.offset += 1;

        self.stack.push(Value::Integer(decoder.val()));
    }

    fn push_float(self: &mut Self, code: &Vec<u8>) {
        let value = float::decode(&code[self.offset..self.offset+4].try_into().unwrap());
        self.offset += 4;

        self.stack.push(Value::Float(value));
    }

    fn push_constant_string(self: &mut Self, code: &Vec<u8>) {
        let idx = code[self.offset] as usize;
        self.offset += 1;

        self.stack.push(Value::Str(self.constant_strs[idx]));
    }

    fn print(self: &mut Self) {
        let value = self.stack.pop().unwrap();
        match value {
            Value::Float(f) => println!("{}", f),
            Value::Integer(i) => println!("{}", i),
            Value::Str(s) => println!("{}", unsafe { &s.as_ref().val }),
        }
    }
}

