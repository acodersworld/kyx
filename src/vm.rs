use std::collections::HashMap;
use std::ptr::NonNull;
use std::vec::Vec;

use itertools::Itertools;
use ordered_float::OrderedFloat;

use crate::compiler::{Compiler, DataSection};
use crate::disassembler;
use crate::float;
use crate::opcode;
use crate::value::{GcValue, StringValue, FunctionValue, Value};
use crate::var_len_int;
use crate::chunk::Chunk;

pub trait Printer {
    fn print(&mut self, s: &str);
}

struct Frame {
    function: NonNull<FunctionValue>,
    locals: Vec<Value>,
    pc: usize
}

impl Frame {
    fn next_code(&mut self) -> Option<u8> {
        let code = unsafe { &self.function.as_ref().chunk.code };
        //println!("{}", opcode::to_string(op));
        //println!("{:?}", self.value_stack);
        //
        let pc = self.pc;
        if pc < code.len() {
            self.pc += 1;
            Some(code[pc])
        }
        else {
            None
        }
    }
}

struct FrameStack {
    stack: Vec<Frame>,
    top: Frame
}

impl FrameStack {
    fn push(&mut self, function: NonNull<FunctionValue>) {
        self.stack.push(Frame {
            function: self.top.function,
            locals: std::mem::replace(&mut self.top.locals, Vec::new()),
            pc: self.top.pc
        });

        self.top.function = function;
        self.top.pc = 0;
    }

    fn pop(&mut self) {
        assert!(self.stack.len() > 0);
        self.top = self.stack.pop().unwrap();
    }
}

pub struct VM<'printer> {
    value_stack: Vec<Value>,
    objects: Vec<GcValue>,
    constant_strs: Vec<NonNull<StringValue>>,
    globals: HashMap<NonNull<StringValue>, Value>,
    break_loop_flag: bool,

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
        Value::Vector(v) => unsafe { v.as_ref().len() > 0 },
        Value::HashMap(h) => unsafe { h.as_ref().len() > 0 },
        Value::Function(_) => true,
    }
}

macro_rules! bin_op {
    ($self:ident, $op:tt) => {
        {
            let st = &mut $self.value_stack;

            let right = &st.pop().unwrap();
            let left = st.last().unwrap();

            let result = match (left, right) {
                (Value::Integer(l), Value::Integer(r)) => Value::Integer(l $op r),
                (Value::Float(l), Value::Float(r)) => Value::Float(*l $op *r),
                _ => panic!("")
            };

            *st.last_mut().unwrap() = result;
        }
    }
}

macro_rules! bin_op_equality {
    ($self:ident, $op:tt) => {
        {
            let st = &mut $self.value_stack;

            let right = &st.pop().unwrap();
            let left = st.last().unwrap();

            let result = match (left, right) {
                (Value::Integer(l), Value::Integer(r)) => Value::Bool(l $op r),
                (Value::Float(l), Value::Float(r)) => Value::Bool(l $op r),
                (Value::Str(l), Value::Str(r)) => Value::Bool(l $op r),
                _ => panic!("")
            };

            *st.last_mut().unwrap() = result;
        }
    }
}

macro_rules! bin_op_comparison {
    ($self:ident, $op:tt) => {
        {
            let st = &mut $self.value_stack;

            let right = &st.pop().unwrap();
            let left = st.last().unwrap();

            let result = match (left, right) {
                (Value::Integer(l), Value::Integer(r)) => Value::Bool(l $op r),
                (Value::Float(l), Value::Float(r)) => Value::Bool(l $op r),
                _ => panic!("")
            };

            *st.last_mut().unwrap() = result;
        }
    }
}

impl<'printer> VM<'printer> {
    pub fn new(printer: &'printer mut dyn Printer) -> VM<'printer> {
        VM {
            value_stack: Vec::new(),
            objects: Vec::new(),
            constant_strs: Vec::new(),
            globals: HashMap::new(),
            break_loop_flag: false,
            compiler: Compiler::new(),
            printer,
        }
    }

    fn run(&mut self, chunk: Chunk) {

        let mut frame_stack = FrameStack {
            stack: Vec::new(),
            top: Frame {
                function: self.function(chunk),
                locals: Vec::new(),
                pc: 0
            }
        };

        while let Some(op) = frame_stack.top.next_code() {
            match op {
                opcode::CONSTANT_INTEGER => self.push_integer(&mut frame_stack.top),
                opcode::CONSTANT_FLOAT => self.push_float(&mut frame_stack.top),
                opcode::CONSTANT_STRING => self.push_constant_string(&mut frame_stack.top),
                opcode::CONSTANT_BOOL => self.push_constant_bool(&mut frame_stack.top),
                opcode::CREATE_VEC => self.create_vec(&mut frame_stack.top),
                opcode::INDEX_VEC => self.index_vec(),
                opcode::SET_VEC => self.set_vec(),
                opcode::CREATE_HASH_MAP => self.create_hash_map(&mut frame_stack.top),
                opcode::INDEX_HASH_MAP => self.index_hash_map(),
                opcode::SET_HASH_MAP => self.set_hash_map(),
                opcode::SET_GLOBAL => self.set_global(&mut frame_stack.top),
                opcode::SET_LOCAL => self.set_local(&mut frame_stack.top),
                opcode::PUSH_GLOBAL => self.push_global(&mut frame_stack.top),
                opcode::PUSH_LOCAL => self.push_local(&mut frame_stack.top),
                opcode::DEFINE_GLOBAL => self.define_global(&mut frame_stack.top),
                opcode::DEFINE_LOCAL => self.define_local(&mut frame_stack.top),
                opcode::ADD => self.add(),
                opcode::SUB => self.sub(),
                opcode::MUL => self.mul(),
                opcode::DIV => self.div(),
                opcode::EQ => self.equals(),
                opcode::NEQ => self.not_equals(),
                opcode::LESS => self.less(),
                opcode::LESS_EQUAL => self.less_equal(),
                opcode::GREATER => self.greater(),
                opcode::GREATER_EQUAL => self.greater_equal(),
                opcode::PRINT => self.print(),
                opcode::POP => self.pop(),
                opcode::LOCAL_POP => self.local_pop(&mut frame_stack.top),
                opcode::PUSH_FRAME => self.push_frame(&mut frame_stack.top),
                opcode::POP_FRAME => self.pop_frame(),
                opcode::LOOP => self.jmp_loop(&mut frame_stack.top),
                opcode::BREAK => self.break_loop(&mut frame_stack.top),
                opcode::JMP => self.jmp(&mut frame_stack.top),
                opcode::JMP_IF_FALSE => self.jmp_if_false(&mut frame_stack.top),
                opcode::READ_INPUT => self.read_input(&mut frame_stack.top),
                opcode::CALL => self.call(&mut frame_stack),
                opcode::RETURN => self.do_return(&mut frame_stack),
                _ => {
                    panic!(
                        "Unknown instruction: {} @ {}",
                        op,
                        frame_stack.top.pc - 1
                    )
                }
            }
        }
    }

    pub fn function(&mut self, chunk: Chunk) -> NonNull<FunctionValue> {
        let mut function_val = Box::new(FunctionValue { chunk });
        let ptr = unsafe { NonNull::new_unchecked(function_val.as_mut() as *mut _) };

        self.objects.push(GcValue::Function(function_val));
        ptr
    }

    pub fn interpret(&mut self, src: &str) -> Result<(), String> {
        let mut data_section = VMDataSection {
            objects: &mut self.objects,
            constant_strs: &mut self.constant_strs,
        };

        let (chunk, functions) = self.compiler.compile(&mut data_section, src)?;

        for (fname, fchunk) in functions {
            let function_value = self.function(fchunk);
            let mut data_section = VMDataSection {
                objects: &mut self.objects,
                constant_strs: &mut self.constant_strs,
            };
            let name_idx = data_section.create_constant_str(&fname) as usize;
            self.globals.insert(self.constant_strs[name_idx], Value::Function(function_value));
        }

        let mut ds = disassembler::Disassembler::new(&chunk.code);
        ds.disassemble();

        self.run(chunk);

        Ok(())
    }

    fn add(&mut self) {
        bin_op!(self, +);
    }

    fn sub(&mut self) {
        bin_op!(self, -);
    }

    fn mul(&mut self) {
        bin_op!(self, *);
    }

    fn div(&mut self) {
        bin_op!(self, /);
    }

    fn equals(&mut self) {
        bin_op_equality!(self, ==);
    }

    fn not_equals(&mut self) {
        bin_op_equality!(self, !=);
    }

    fn less(&mut self) {
        bin_op_comparison!(self, <);
    }

    fn less_equal(&mut self) {
        bin_op_comparison!(self, <=);
    }

    fn greater(&mut self) {
        bin_op_comparison!(self, >);
    }

    fn greater_equal(&mut self) {
        bin_op_comparison!(self, >=);
    }

    fn push_integer(&mut self, frame: &mut Frame) {
        let mut decoder = var_len_int::Decoder::new();
        while !decoder.step_decode(frame.next_code().unwrap()) {
        }

        self.value_stack.push(Value::Integer(decoder.val()));
    }

    fn push_float(&mut self, frame: &mut Frame) {
        let code = unsafe { &frame.function.as_ref().chunk.code };

        let value = float::decode(&code[frame.pc..frame.pc + 4].try_into().unwrap());
        frame.pc += 4;

        self.value_stack.push(Value::Float(OrderedFloat(value)));
    }

    fn push_constant_string(&mut self, frame: &mut Frame) {
        let idx = frame.next_code().unwrap() as usize;

        self.value_stack.push(Value::Str(self.constant_strs[idx]));
    }

    fn push_constant_bool(&mut self, frame: &mut Frame) {
        let value = frame.next_code().unwrap() != 0;

        self.value_stack.push(Value::Bool(value));
    }

    fn create_vec(&mut self, frame: &mut Frame) {
        let arg_count = frame.next_code().unwrap() as usize;

        assert!(self.value_stack.len() >= arg_count);
        let vector: Vec<Value> = self.value_stack.drain((self.value_stack.len() - arg_count)..).collect();

        let mut vec_val = Box::new(vector);
        let ptr = unsafe { NonNull::new_unchecked(vec_val.as_mut() as *mut _) };
        self.objects.push(GcValue::Vector(vec_val));

        self.value_stack.push(Value::Vector(ptr));
    }

    fn index_vec(&mut self) {
        assert!(self.value_stack.len() > 1);
        let index = match self.value_stack.pop().unwrap() {
            Value::Integer(i) => i as usize,
            _ => panic!("Bad index value"),
        };

        let vector = match self.value_stack.pop().unwrap() {
            Value::Vector(v) => unsafe { v.as_ref() },
            _ => panic!("Not a vector"),
        };

        self.value_stack.push(vector[index]);
    }

    fn set_vec(&mut self) {
        assert!(self.value_stack.len() > 2);
        let new_value = self.value_stack.pop().unwrap();
        let index = match self.value_stack.pop().unwrap() {
            Value::Integer(i) => i as usize,
            _ => panic!("Bad index value"),
        };

        let vector = match self.value_stack.pop().unwrap() {
            Value::Vector(mut v) => unsafe { v.as_mut() },
            _ => panic!("Not a vector"),
        };

        vector[index] = new_value;
        self.value_stack.push(new_value);
    }

    fn create_hash_map(&mut self, frame: &mut Frame) {
        let arg_count = frame.next_code().unwrap() as usize * 2;

        assert!(self.value_stack.len() >= arg_count);
        let args: Vec<Value> = self.value_stack.drain((self.value_stack.len() - arg_count)..).collect();

        let mut hash_map = HashMap::<Value, Value>::new();

        for kv in args.chunks(2) {
            hash_map.insert(kv[0], kv[1]);
        }

        let mut hash_map_val = Box::new(hash_map);
        let ptr = unsafe { NonNull::new_unchecked(hash_map_val.as_mut() as *mut _) };
        self.objects.push(GcValue::HashMap(hash_map_val));

        self.value_stack.push(Value::HashMap(ptr));
    }

    fn index_hash_map(&mut self) {
        assert!(self.value_stack.len() > 1);
        let index = self.value_stack.pop().unwrap();

        let hash_map = match self.value_stack.pop().unwrap() {
            Value::HashMap(h) => unsafe { h.as_ref() },
            _ => panic!("Not a hash map"),
        };

        self.value_stack.push(hash_map[&index]);
    }

    fn set_hash_map(&mut self) {
        assert!(self.value_stack.len() > 2);
        let new_value = self.value_stack.pop().unwrap();
        let index = self.value_stack.pop().unwrap();
        let hash_map = match self.value_stack.pop().unwrap() {
            Value::HashMap(mut h) => unsafe { h.as_mut() },
            _ => panic!("Not a vector"),
        };

        hash_map.insert(index, new_value);
        self.value_stack.push(new_value);
    }

    fn set_global(&mut self, frame: &mut Frame) {
        let idx = frame.next_code().unwrap() as usize;

        let name = self.constant_strs[idx];
        assert!(self.globals.contains_key(&name));
        self.globals.insert(name, *self.value_stack.last().unwrap());
    }

    fn set_local(&mut self, frame: &mut Frame) {
        let idx = frame.next_code().unwrap() as usize;

        assert!(!self.value_stack.is_empty());
        let local = &mut frame.locals[idx];
        *local = self.value_stack.pop().unwrap();
    }

    fn push_global(&mut self, frame: &mut Frame) {
        let idx = frame.next_code().unwrap() as usize;

        let name = self.constant_strs[idx];
        assert!(self.globals.contains_key(&name));
        self.value_stack.push(self.globals[&name]);
    }

    fn push_local(&mut self, frame: &mut Frame) {
        let idx = frame.next_code().unwrap() as usize;

        let locals = &frame.locals;
        assert!(!locals.is_empty());
        self.value_stack.push(locals[idx]);
    }

    fn define_global(&mut self, frame: &mut Frame) {
        let idx = frame.next_code().unwrap() as usize;

        let name = self.constant_strs[idx];
        let st = &mut self.value_stack;
        assert!(!self.globals.contains_key(&name));
        assert!(!st.is_empty());

        let value = st.pop().unwrap();
        self.globals.insert(name, value);
    }

    fn define_local(&mut self, frame: &mut Frame) {
        let st = &mut self.value_stack;
        assert!(!st.is_empty());

        let locals = &mut frame.locals;
        let value = st.pop().unwrap();
        locals.push(value);
    }

    fn format_vec(vector: &Vec<Value>) -> String {
        let mut s = "vec{".to_owned();

        let values = vector
            .iter()
            .map(|v| Self::format_value(v))
            .collect::<Vec<String>>();

        s += &(values.join(&",") + "}");
        s
    }

    fn format_hash_map(hash_map: &HashMap<Value, Value>) -> String {
        let mut s = "hash_map{".to_owned();

        let values = hash_map
            .iter()
            .sorted()
            .map(|(k, v)| format!("{}: {}", Self::format_value(k), Self::format_value(v)))
            .collect::<Vec<String>>();

        s += &(values.join(&",") + "}");
        s
    }

    fn format_value(value: &Value) -> String {
        match value {
            Value::Float(f) => format!("{}", f),
            Value::Integer(i) => format!("{}", i),
            Value::Str(s) => unsafe { &s.as_ref().val }.to_string(),
            Value::Bool(b) => format!("{}", b),
            Value::Vector(v) => Self::format_vec(unsafe { v.as_ref() }),
            Value::HashMap(h) => Self::format_hash_map(unsafe { h.as_ref() }),
            Value::Function(f) => format!("function<0x{:x}>", f.as_ptr() as usize),
        }
    }

    fn print(&mut self) {
        let value = self.value_stack.pop().unwrap();
        let s = Self::format_value(&value);
        self.printer.print(&s);
    }

    fn pop(&mut self) {
        assert!(!self.value_stack.is_empty());
        self.value_stack.pop();
    }

    fn local_pop(&mut self, frame: &mut Frame) {
        frame.locals.pop();
    }

    fn push_frame(&mut self, frame: &mut Frame) {
        frame.locals.clear();
 
//        assert!(!self.frame_stack.is_empty());
//        let (function, pc) = {
//            let frame = self.frame_stack.last().unwrap();
//            (frame.function, frame.pc)
//        };
//
//        self.frame_stack.push(StackFrame {
//            function,
//            locals: Vec::new(),
//            pc
//        });
    }

    fn pop_frame(&mut self) {
//        assert!(!self.frame_stack.is_empty());
//        self.frame_stack.pop();
    }

    fn jmp_loop(&mut self, frame: &mut Frame) {

        let do_loop = !self.break_loop_flag;
        self.break_loop_flag = false;
        if do_loop {
            let code = unsafe { &frame.function.as_ref().chunk.code };
            let offset = code[frame.pc] as usize;
            frame.pc -= offset;
        } else {
            frame.pc += 1;
        }
    }

    fn break_loop(&mut self, frame: &mut Frame) {
        let code = unsafe { &frame.function.as_ref().chunk.code };
        let offset = code[frame.pc] as usize;

        frame.pc += offset;
        self.break_loop_flag = true;
    }

    fn jmp(&mut self, frame: &mut Frame) {
        let code = unsafe { &frame.function.as_ref().chunk.code };
        let offset = code[frame.pc] as usize;

        frame.pc += offset;
    }

    fn jmp_if_false(&mut self, frame: &mut Frame) {
        assert!(!self.value_stack.is_empty());
        let cond = self.value_stack.pop().unwrap();

        let code = unsafe { &frame.function.as_ref().chunk.code };
        let offset = code[frame.pc] as usize;

        if !is_truthy(&cond) {
            frame.pc += offset as usize;
        } else {
            frame.pc += 1;
        }
    }

    fn read_input(&mut self, frame: &mut Frame) {
        let read_type = frame.next_code().unwrap();

        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();

        if read_type == 0 {
            let val = line.trim().parse::<i32>().unwrap_or(0);
            self.value_stack.push(Value::Integer(val));
        } else if read_type == 1 {
            let val = line.trim().parse::<f32>().unwrap_or(0.0);
            self.value_stack.push(Value::Float(OrderedFloat(val)));
        } else {
            let mut data_section = VMDataSection {
                objects: &mut self.objects,
                constant_strs: &mut self.constant_strs,
            };

            let idx = data_section.create_constant_str(line.trim()) as usize;
            self.value_stack.push(Value::Str(self.constant_strs[idx]));
        }
    }

    fn call(&mut self, frame_stack: &mut FrameStack) {
        let idx = frame_stack.top.next_code().unwrap() as usize;

        let name = self.constant_strs[idx];
        assert!(self.globals.contains_key(&name));
        match self.globals[&name] {
            Value::Function(f) => {
                frame_stack.push(f);
                std::mem::swap(&mut frame_stack.top.locals, &mut self.value_stack);
            }
            _ => {}
        }
    }

    fn do_return(&mut self, frame_stack: &mut FrameStack) {
        frame_stack.pop();
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

    #[test]
    fn comparison() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            print 1 <= 1;
            print 1 >= 1;

            print 1 < 2;
            print 1 <= 2;
            print 2 > 1;
            print 2 >= 1;

            print 1 > 2;
            print 1 >= 2;
            print 2 < 1;
            print 2 <= 1;

            print 1 < 2;
            print 1 <= 2;
            print 2 > 1;
            print 2 >= 1;

            print 1 > 2;
            print 1 >= 2;
            print 2 < 1;
            print 2 <= 1;
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 18);
        assert_eq!(printer.strings[0], "true");
        assert_eq!(printer.strings[1], "true");

        assert_eq!(printer.strings[2], "true");
        assert_eq!(printer.strings[3], "true");
        assert_eq!(printer.strings[4], "true");
        assert_eq!(printer.strings[5], "true");

        assert_eq!(printer.strings[6], "false");
        assert_eq!(printer.strings[7], "false");
        assert_eq!(printer.strings[8], "false");
        assert_eq!(printer.strings[9], "false");

        assert_eq!(printer.strings[6], "false");
        assert_eq!(printer.strings[7], "false");
        assert_eq!(printer.strings[8], "false");
        assert_eq!(printer.strings[9], "false");

        assert_eq!(printer.strings[2], "true");
        assert_eq!(printer.strings[3], "true");
        assert_eq!(printer.strings[4], "true");
        assert_eq!(printer.strings[5], "true");
    }

    #[test]
    fn while_loop() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let mut i: int = 0;
            while i < 5 {
                print i;
                i = i + 1;
            }
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 5);
        assert_eq!(printer.strings[0], "0");
        assert_eq!(printer.strings[1], "1");
        assert_eq!(printer.strings[2], "2");
        assert_eq!(printer.strings[3], "3");
        assert_eq!(printer.strings[4], "4");
    }

    #[test]
    fn for_loop() {
        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);

            let src = "
                for i : 0..4 {
                    print i;
                }
            ";
            assert_eq!(vm.interpret(src), Ok(()));
            assert_eq!(printer.strings.len(), 4);
            assert_eq!(printer.strings[0], "0");
            assert_eq!(printer.strings[1], "1");
            assert_eq!(printer.strings[2], "2");
            assert_eq!(printer.strings[3], "3");
        }

        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);

            let src = "
                for i : 0..=4 {
                    print i;
                }
            ";
            assert_eq!(vm.interpret(src), Ok(()));
            assert_eq!(printer.strings.len(), 5);
            assert_eq!(printer.strings[0], "0");
            assert_eq!(printer.strings[1], "1");
            assert_eq!(printer.strings[2], "2");
            assert_eq!(printer.strings[3], "3");
            assert_eq!(printer.strings[4], "4");
        }

        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);

            let src = "
                for i : 0..=4 {
                    let j: int = i;
                    print j;
                }
            ";
            assert_eq!(vm.interpret(src), Ok(()));
            assert_eq!(printer.strings.len(), 5);
            assert_eq!(printer.strings[0], "0");
            assert_eq!(printer.strings[1], "1");
            assert_eq!(printer.strings[2], "2");
            assert_eq!(printer.strings[3], "3");
            assert_eq!(printer.strings[4], "4");
        }
    }

    #[test]
    fn while_loop_break() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let mut i: int = 0;
            while i < 5 {
                if i > 2 {
                    break;
                }

                print i;
                i = i + 1;
            }
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 3);
        assert_eq!(printer.strings[0], "0");
        assert_eq!(printer.strings[1], "1");
        assert_eq!(printer.strings[2], "2");
    }

    #[test]
    fn while_loop_nested_break() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let mut i: int = 0;
            while i < 5 {
                if i > 2 {
                    break;
                }

                print i;
                i = i + 1;

                let mut j: int = 0;
                while j < 5 {
                    if j > 3 {
                        break;
                    }
                    print j;
                    j = j + 1;
                }

            }
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        println!("{:?}", printer.strings);
        assert_eq!(printer.strings.len(), 15);
        assert_eq!(printer.strings[0], "0");
        assert_eq!(printer.strings[1], "0");
        assert_eq!(printer.strings[2], "1");
        assert_eq!(printer.strings[3], "2");
        assert_eq!(printer.strings[4], "3");

        assert_eq!(printer.strings[5], "1");
        assert_eq!(printer.strings[6], "0");
        assert_eq!(printer.strings[7], "1");
        assert_eq!(printer.strings[8], "2");
        assert_eq!(printer.strings[9], "3");

        assert_eq!(printer.strings[10], "2");
        assert_eq!(printer.strings[11], "0");
        assert_eq!(printer.strings[12], "1");
        assert_eq!(printer.strings[13], "2");
        assert_eq!(printer.strings[14], "3");
    }

    #[test]
    fn while_loop_continue() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let mut i: int = 0;
            while i < 5 {
                if i < 2 {
                    i = i + 1;
                    continue;
                }

                print i;
                i = i + 1;
            }
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 3);
        assert_eq!(printer.strings[0], "2");
        assert_eq!(printer.strings[1], "3");
        assert_eq!(printer.strings[2], "4");
    }

    #[test]
    fn while_loop_nested_continue() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let mut i: int = 0;
            while i < 5 {
                if i < 2 {
                    i = i + 1;
                    continue;
                }

                print i;
                i = i + 1;

                let mut j: int = 0;
                while j < 5 {
                    if j < 3 {
                        j = j + 1;
                        continue;
                    }

                    print j;
                    j = j + 1;
                }
            }
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 9);
        assert_eq!(printer.strings[0], "2");
        assert_eq!(printer.strings[1], "3");
        assert_eq!(printer.strings[2], "4");

        assert_eq!(printer.strings[3], "3");
        assert_eq!(printer.strings[4], "3");
        assert_eq!(printer.strings[5], "4");

        assert_eq!(printer.strings[6], "4");
        assert_eq!(printer.strings[7], "3");
        assert_eq!(printer.strings[8], "4");
    }

    #[test]
    fn construct_vector() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let v: [int] = vec<int>{10,20,30,40};
            print v;
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 1);
        assert_eq!(printer.strings[0], "vec{10,20,30,40}");
    }

    #[test]
    fn vector_index() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let v: [int] = vec<int>{10,20,30,40};
            print v[0];
            print v[3];
            print v[2];
            print v[1];
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 4);
        assert_eq!(printer.strings[0], "10");
        assert_eq!(printer.strings[1], "40");
        assert_eq!(printer.strings[2], "30");
        assert_eq!(printer.strings[3], "20");
    }

    #[test]
    fn vector_nested_index() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let v: [[int]] = vec<[int]>{vec<int>{1,2,3}, vec<int>{40,50,60}, vec<int>{700,800,900}};

            for i : 0..3 {
                print \"row\";
                for j : 0..3 {
                    print v[i][j];
                }
            }
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        println!("{:?}", printer.strings);
        assert_eq!(printer.strings.len(), 12);
        assert_eq!(printer.strings[0], "row");
        assert_eq!(printer.strings[1], "1");
        assert_eq!(printer.strings[2], "2");
        assert_eq!(printer.strings[3], "3");

        assert_eq!(printer.strings[4], "row");
        assert_eq!(printer.strings[5], "40");
        assert_eq!(printer.strings[6], "50");
        assert_eq!(printer.strings[7], "60");

        assert_eq!(printer.strings[8], "row");
        assert_eq!(printer.strings[9], "700");
        assert_eq!(printer.strings[10], "800");
        assert_eq!(printer.strings[11], "900");
    }

    #[test]
    fn vector_set() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let v: [int] = vec<int>{10,20,30,40};
            print v[0];
            print v[3];
            print v[2];
            print v[1];

            v[1] = 100;
            print v[1];
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 5);
        assert_eq!(printer.strings[0], "10");
        assert_eq!(printer.strings[1], "40");
        assert_eq!(printer.strings[2], "30");
        assert_eq!(printer.strings[3], "20");
        assert_eq!(printer.strings[4], "100");
    }

    #[test]
    fn construct_hash_map() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let h: [int: string] = hash_map<int, string>{
                10: \"Hello\",
                20: \"World\",
                30: \"Kyx\"
            };
            print h;
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 1);
        assert_eq!(printer.strings[0], "hash_map{10: Hello,20: World,30: Kyx}");
    }

    #[test]
    fn index_hash_map() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let h: [int: string] = hash_map<int, string>{
                10: \"Hello\",
                20: \"World\",
                30: \"Kyx\"
            };
            print h[20];
            print h[10];
            print h[30];
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 3);
        assert_eq!(printer.strings[0], "World");
        assert_eq!(printer.strings[1], "Hello");
        assert_eq!(printer.strings[2], "Kyx");
    }

    #[test]
    fn hash_map_set() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let h: [string: string] = hash_map<string, string>{\"hello\": \"world\", \"kyx\": \"lang\"};
            print h[\"kyx\"];
            print h[\"hello\"];

            h[\"kyx\"] = \"awesome\";
            print h[\"kyx\"];
            print h[\"hello\"];
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 4);
        assert_eq!(printer.strings[0], "lang");
        assert_eq!(printer.strings[1], "world");
        assert_eq!(printer.strings[2], "awesome");
        assert_eq!(printer.strings[3], "world");
    }

    #[test]
    fn function_call() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            fn test() {
                print \"test function\";
            }

            test();
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 1);
        assert_eq!(printer.strings[0], "test function");
    }

    #[test]
    fn function_call_return_value() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            fn test() -> int {
                return 10;
            }

            print test();
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 1);
        assert_eq!(printer.strings[0], "10");
    }

    #[test]
    fn function_call_parameters() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            fn test(i: int, f: float, s: string) {
                print i;
                print f;
                print s;
            }

            test(20, 1.23, \"hello\");
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 3);
        assert_eq!(printer.strings[0], "20");
        assert_eq!(printer.strings[1], "1.23");
        assert_eq!(printer.strings[2], "hello");
    }

    #[test]
    fn function_call_parameters_with_return_value() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            fn test(i: int, f: float, s: string) -> int {
                print i;
                print f;
                print s;

                return 1;
            }

            print test(20, 1.23, \"hello\");
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 4);
        assert_eq!(printer.strings[0], "20");
        assert_eq!(printer.strings[1], "1.23");
        assert_eq!(printer.strings[2], "hello");
        assert_eq!(printer.strings[3], "1");
    }
}
