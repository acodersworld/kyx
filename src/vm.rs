use std::collections::HashMap;
use std::ptr::NonNull;
use std::vec::Vec;

use crate::compiler::{Compiler, DataSection};
use crate::disassembler;
use crate::float;
use crate::opcode;
use crate::value::{FromValue, GcValue, StringValue, Value};
use crate::var_len_int;

pub trait Printer {
    fn print(&mut self, s: &str);
}

pub struct VM<'printer> {
    stack: Vec<Value>,
    objects: Vec<GcValue>,
    constant_strs: Vec<NonNull<StringValue>>,
    globals: HashMap<NonNull<StringValue>, Value>,
    locals: Vec<Vec<Value>>,
    offset: usize,

    compiler: Compiler,
    printer: &'printer mut dyn Printer,
}

struct VMDataSection<'a> {
    objects: &'a mut Vec<GcValue>,
    constant_strs: &'a mut Vec<NonNull<StringValue>>,
}

impl DataSection for VMDataSection<'_> {
    fn create_constant_str(&mut self, s: &str) -> u8 {
        for (idx, string) in self.constant_strs.iter().enumerate() {
            let val = unsafe { &string.as_ref().val };
            if val == s {
                return idx as u8;
            }
        }

        let mut str_val = Box::new(StringValue {
            val: s.to_string(),
            hash: 0,
        });
        let ptr = unsafe { NonNull::new_unchecked(str_val.as_mut() as *mut _) };
        self.objects.push(GcValue::Str(str_val));

        let idx = self.constant_strs.len() as u8;
        self.constant_strs.push(ptr);
        idx
    }
}

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Integer(i) => *i != 0,
        Value::Float(f) => *f != 0.0,
        Value::Str(s) => unsafe { !s.as_ref().val.is_empty() },
        Value::Bool(b) => *b,
    }
}

impl<'printer> VM<'printer> {
    pub fn new(printer: &'printer mut dyn Printer) -> VM<'printer> {
        VM {
            stack: Vec::new(),
            objects: Vec::new(),
            constant_strs: Vec::new(),
            globals: HashMap::new(),
            locals: Vec::new(),
            offset: 0,
            compiler: Compiler::new(),
            printer,
        }
    }

    fn run(&mut self, code: &[u8]) {
        self.offset = 0;
        let len = code.len();

        while self.offset < len {
            self.offset += 1;
            match code[self.offset - 1] {
                opcode::CONSTANT_INTEGER => self.push_integer(code),
                opcode::CONSTANT_FLOAT => self.push_float(code),
                opcode::CONSTANT_STRING => self.push_constant_string(code),
                opcode::SET_GLOBAL => self.set_global(code),
                opcode::SET_LOCAL => self.set_local(code),
                opcode::PUSH_GLOBAL => self.push_global(code),
                opcode::PUSH_LOCAL => self.push_local(code),
                opcode::DEFINE_GLOBAL => self.define_global(code),
                opcode::DEFINE_LOCAL => self.define_local(),
                opcode::ADDI => self.integer_add(),
                opcode::SUBI => self.integer_sub(),
                opcode::MULI => self.integer_mul(),
                opcode::DIVI => self.integer_div(),
                opcode::ADDF => self.float_add(),
                opcode::SUBF => self.float_sub(),
                opcode::MULF => self.float_mul(),
                opcode::DIVF => self.float_div(),
                opcode::EQI => self.integer_equals(),
                opcode::EQF => self.float_equals(),
                opcode::EQS => self.string_equals(),
                opcode::NEQI => self.integer_not_equals(),
                opcode::NEQF => self.float_not_equals(),
                opcode::NEQS => self.string_not_equals(),
                opcode::PRINT => self.print(),
                opcode::POP => self.pop(),
                opcode::LOCAL_POP => self.local_pop(),
                opcode::PUSH_FRAME => self.push_frame(),
                opcode::POP_FRAME => self.pop_frame(),
                opcode::JMP => self.jmp(code),
                opcode::JMP_IF_FALSE => self.jmp_if_false(code),
                opcode::READ_INPUT => self.read_input(code),
                _ => {
                    panic!("Unknown instruction: {}", code[self.offset - 1])
                }
            }
        }
    }

    pub fn interpret(&mut self, src: &str) -> Result<(), String> {
        let mut data_section = VMDataSection {
            objects: &mut self.objects,
            constant_strs: &mut self.constant_strs,
        };

        let chunk = self.compiler.compile(&mut data_section, src)?;
        let code = &chunk.code;

        let mut ds = disassembler::Disassembler::new(code);
        ds.disassemble();

        self.run(code);
        Ok(())
    }

    fn binary_op<T, OP>(&mut self, op: OP)
    where
        T: FromValue<ValueType = T>,
        OP: FnOnce(T::ValueType, T::ValueType) -> Value,
    {
        let st = &mut self.stack;

        let right = T::from_value(&st.pop().unwrap()).unwrap();
        let left = T::from_value(st.last().unwrap()).unwrap();

        let result = op(left, right);
        *st.last_mut().unwrap() = result;
    }

    fn integer_add(&mut self) {
        self.binary_op::<i32, _>(|l, r| Value::Integer(l + r));
    }

    fn integer_sub(&mut self) {
        self.binary_op::<i32, _>(|l, r| Value::Integer(l - r));
    }

    fn integer_mul(&mut self) {
        self.binary_op::<i32, _>(|l, r| Value::Integer(l * r));
    }

    fn integer_div(&mut self) {
        self.binary_op::<i32, _>(|l, r| Value::Integer(l / r));
    }

    fn float_add(&mut self) {
        self.binary_op::<f32, _>(|l, r| Value::Float(l + r));
    }

    fn float_sub(&mut self) {
        self.binary_op::<f32, _>(|l, r| Value::Float(l - r));
    }

    fn float_mul(&mut self) {
        self.binary_op::<f32, _>(|l, r| Value::Float(l * r));
    }

    fn float_div(&mut self) {
        self.binary_op::<f32, _>(|l, r| Value::Float(l / r));
    }

    fn integer_equals(&mut self) {
        self.binary_op::<i32, _>(|l, r| Value::Bool(l == r));
    }

    fn float_equals(&mut self) {
        self.binary_op::<f32, _>(|l, r| Value::Bool(l == r));
    }

    fn string_equals(&mut self) {
        self.binary_op::<NonNull<StringValue>, _>(|l, r| Value::Bool(l == r));
    }

    fn integer_not_equals(&mut self) {
        self.binary_op::<i32, _>(|l, r| Value::Bool(l != r));
    }

    fn float_not_equals(&mut self) {
        self.binary_op::<f32, _>(|l, r| Value::Bool(l != r));
    }

    fn string_not_equals(&mut self) {
        self.binary_op::<NonNull<StringValue>, _>(|l, r| Value::Bool(l != r));
    }

    fn push_integer(&mut self, code: &[u8]) {
        let mut decoder = var_len_int::Decoder::new();
        while !decoder.step_decode(code[self.offset]) {
            self.offset += 1;
        }
        self.offset += 1;

        self.stack.push(Value::Integer(decoder.val()));
    }

    fn push_float(&mut self, code: &[u8]) {
        let value = float::decode(&code[self.offset..self.offset + 4].try_into().unwrap());
        self.offset += 4;

        self.stack.push(Value::Float(value));
    }

    fn push_constant_string(&mut self, code: &[u8]) {
        let idx = code[self.offset] as usize;
        self.offset += 1;

        self.stack.push(Value::Str(self.constant_strs[idx]));
    }

    fn set_global(&mut self, code: &[u8]) {
        let idx = code[self.offset] as usize;
        self.offset += 1;

        let name = self.constant_strs[idx];
        assert!(self.globals.contains_key(&name));
        self.globals.insert(name, *self.stack.last().unwrap());
    }

    fn set_local(&mut self, code: &[u8]) {
        let idx = code[self.offset] as usize;
        self.offset += 1;

        assert!(!self.locals.is_empty());
        assert!(!self.stack.is_empty());
        let local = &mut self.locals.last_mut().unwrap()[idx];
        *local = self.stack.pop().unwrap();
    }

    fn push_global(&mut self, code: &[u8]) {
        let idx = code[self.offset] as usize;
        self.offset += 1;

        let name = self.constant_strs[idx];
        assert!(self.globals.contains_key(&name));
        self.stack.push(self.globals[&name]);
    }

    fn push_local(&mut self, code: &[u8]) {
        let idx = code[self.offset] as usize;
        self.offset += 1;

        assert!(!self.locals.is_empty());
        let locals = self.locals.last().unwrap();
        assert!(!locals.is_empty());
        self.stack.push(locals[idx]);
    }

    fn define_global(&mut self, code: &[u8]) {
        let idx = code[self.offset] as usize;
        self.offset += 1;

        let name = self.constant_strs[idx];
        let st = &mut self.stack;
        assert!(!self.globals.contains_key(&name));
        assert!(!st.is_empty());

        let value = st.pop().unwrap();
        self.globals.insert(name, value);
    }

    fn define_local(&mut self) {
        let st = &mut self.stack;
        assert!(!self.locals.is_empty());
        assert!(!st.is_empty());

        let locals = self.locals.last_mut().unwrap();
        let value = st.pop().unwrap();
        locals.push(value);
    }

    fn print(&mut self) {
        let value = self.stack.pop().unwrap();
        let s = match value {
            Value::Float(f) => format!("{}", f),
            Value::Integer(i) => format!("{}", i),
            Value::Str(s) => unsafe { &s.as_ref().val }.to_string(),
            Value::Bool(b) => format!("{}", b),
        };

        self.printer.print(&s);
    }

    fn pop(&mut self) {
        assert!(!self.stack.is_empty());
        self.stack.pop();
    }

    fn local_pop(&mut self) {
        assert!(!self.locals.is_empty());
        self.locals.last_mut().unwrap().pop();
    }

    fn push_frame(&mut self) {
        self.locals.push(Vec::new());
    }

    fn pop_frame(&mut self) {
        assert!(!self.locals.is_empty());
        self.locals.pop();
    }

    fn jmp(&mut self, code: &[u8]) {
        let offset = code[self.offset] as usize;
        self.offset += offset;
    }

    fn jmp_if_false(&mut self, code: &[u8]) {
        assert!(!self.stack.is_empty());
        let cond = self.stack.pop().unwrap();

        if !is_truthy(&cond) {
            self.offset += code[self.offset] as usize;
        } else {
            self.offset += 1;
        }
    }

    fn read_input(&mut self, code: &[u8]) {
        let read_type = code[self.offset];
        self.offset += 1;

        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();

        if read_type == 0 {
            let val = line.trim().parse::<i32>().unwrap_or(0);
            self.stack.push(Value::Integer(val));
        } else if read_type == 1 {
            let val = line.trim().parse::<f32>().unwrap_or(0.0);
            self.stack.push(Value::Float(val));
        } else {
            let mut data_section = VMDataSection {
                objects: &mut self.objects,
                constant_strs: &mut self.constant_strs,
            };

            let idx = data_section.create_constant_str(line.trim()) as usize;
            self.stack.push(Value::Str(self.constant_strs[idx]));
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct TestPrinter {
        strings: Vec<String>,
    }

    impl TestPrinter {
        fn new() -> TestPrinter {
            TestPrinter {
                strings: Vec::new(),
            }
        }
    }

    impl Printer for TestPrinter {
        fn print(&mut self, s: &str) {
            self.strings.push(s.to_string());
        }
    }

    #[test]
    fn print_integer() {
        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            assert_eq!(vm.interpret("print 12345;"), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "12345");
        }

        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            assert_eq!(vm.interpret("print -12345;"), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "-12345");
        }
    }

    #[test]
    fn print_float() {
        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            assert_eq!(vm.interpret("print 3.142;"), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "3.142");
        }

        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            assert_eq!(vm.interpret("print -3.142;"), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "-3.142");
        }
    }

    // Once an api is set in place, it should be used instead of print
    #[test]
    fn integer_arithmetic() {
        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            assert_eq!(vm.interpret("print 1+2;"), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "3");
        }

        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            assert_eq!(vm.interpret("print 1-2;"), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "-1");
        }

        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            assert_eq!(vm.interpret("print 1+-2;"), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "-1");
        }

        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            assert_eq!(vm.interpret("print 2*3;"), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "6");
        }

        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            assert_eq!(vm.interpret("print 6/2;"), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "3");
        }
    }

    #[test]
    fn float_arithmetic() {
        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            assert_eq!(vm.interpret("print 1.2+2.3;"), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "3.5");
        }

        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            assert_eq!(vm.interpret("print 1.5+-2.6;"), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "-1.0999999");
        }

        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            assert_eq!(vm.interpret("print 1.5-2.6;"), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "-1.0999999");
        }

        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            assert_eq!(vm.interpret("print 2.5 * 1.5;"), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "3.75");
        }

        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            assert_eq!(vm.interpret("print 8.4 / 4.2;"), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "2");
        }
    }

    #[test]
    fn print_constant_string() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);
        assert_eq!(vm.interpret("print \"Hello, World\";"), Ok(()));
        assert_eq!(printer.strings.len(), 1);
        assert_eq!(printer.strings[0], "Hello, World");
    }

    #[test]
    fn print_global() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let integer: int = 10;
            print integer;";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 1);
        assert_eq!(printer.strings[0], "10");
    }

    #[test]
    fn set_read_only_global_error() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "let integer: int = 10;";
        assert_eq!(vm.interpret(src), Ok(()));

        let src = "integer = 10;";
        assert!(vm.interpret(src).is_err());
    }

    #[test]
    fn set_mutable_global() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "let mut integer: int = 10;";
        assert_eq!(vm.interpret(src), Ok(()));

        let src = "integer = 10;";
        assert_eq!(vm.interpret(src), Ok(()));
    }

    #[test]
    fn block_expression_return_value() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "let mut integer: int = { 10 }; print integer;";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 1);
        assert_eq!(printer.strings[0], "10");
    }

    #[test]
    fn nested_block_expression_return_value() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "let mut integer: int = {{{ 10 }}}; print integer;";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 1);
        assert_eq!(printer.strings[0], "10");
    }

    #[test]
    fn multiple_locals() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
        {
            let i: int = 10;
            let s: string = \"hello\";
            let i2: int = 20;
            let s2: string = \"world\";
            print i;
            print i2;
            print s;
            print s2;
        }";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 4);
        assert_eq!(printer.strings[0], "10");
        assert_eq!(printer.strings[1], "20");
        assert_eq!(printer.strings[2], "hello");
        assert_eq!(printer.strings[3], "world");
    }

    #[test]
    fn block_declare_local() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "{ let local: int = 10; print local; }";
        assert_eq!(vm.interpret(src), Ok(()));

        let src = "print local;";
        assert!(vm.interpret(src).is_err());

        let src = "
        {
            let mut local: int = 10;
            {
                print local;
                local = 20;

                {
                    let mut local: int = 5;
                    {
                        let mut local: int = 15;
                        print local;
                    }
                    print local;
                }
            }
            print local;
        }";
        assert_eq!(vm.interpret(src), Ok(()));

        assert_eq!(printer.strings.len(), 5);
        assert_eq!(printer.strings[0], "10");

        assert_eq!(printer.strings[1], "10");
        assert_eq!(printer.strings[2], "15");
        assert_eq!(printer.strings[3], "5");
        assert_eq!(printer.strings[4], "20");
    }

    #[test]
    fn if_stmt_true() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
        if 1 {
            print 1;
        }";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 1);
        assert_eq!(printer.strings[0], "1");
    }

    #[test]
    fn if_else_stmt_true() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
        if 1 {
            print 1;
        }
        else {
            print 0;
        }";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 1);
        assert_eq!(printer.strings[0], "1");
    }

    #[test]
    fn if_stmt_false() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
        if 0 {
            print 1;
        }";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 0);
    }

    #[test]
    fn if_else_stmt_false() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
        if 0 {
            print 1;
        }
        else {
            print 0;
        }";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 1);
        assert_eq!(printer.strings[0], "0");
    }

    #[test]
    fn equals() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            print 1 == 1;
            print 3.142 == 3.142;
            print \"hello\" == \"hello\";

            print 1 == 2;
            print 3.142 == 1.42;
            print \"hello\" == \"world\";
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 6);
        assert_eq!(printer.strings[0], "true");
        assert_eq!(printer.strings[1], "true");
        assert_eq!(printer.strings[2], "true");
        assert_eq!(printer.strings[3], "false");
        assert_eq!(printer.strings[4], "false");
        assert_eq!(printer.strings[5], "false");
    }

    #[test]
    fn not_equals() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            print 1 != 1;
            print 3.142 != 3.142;
            print \"hello\" != \"hello\";

            print 1 != 2;
            print 3.142 != 1.42;
            print \"hello\" != \"world\";
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 6);
        assert_eq!(printer.strings[0], "false");
        assert_eq!(printer.strings[1], "false");
        assert_eq!(printer.strings[2], "false");
        assert_eq!(printer.strings[3], "true");
        assert_eq!(printer.strings[4], "true");
        assert_eq!(printer.strings[5], "true");
    }
}
