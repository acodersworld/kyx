use std::collections::HashMap;
use std::ptr::NonNull;
use std::vec::Vec;

use itertools::Itertools;
use ordered_float::OrderedFloat;

use crate::builtin_functions;
use crate::chunk::Chunk;
use crate::compiler::{Compiler, DataSection};
use crate::disassembler;
use crate::float;
use crate::opcode;
use crate::rust_function_ctx::{RustFunctionCtx, RustValue};
use crate::value::{
    FunctionValue, GcValue, RustFunctionValue, StringValue, StructValue, UnionValue, Value,
};
use crate::var_len_int;

pub trait Printer {
    fn print(&mut self, s: &str);
}

struct Frame {
    function: NonNull<FunctionValue>,
    locals: Vec<Value>,
    pc: usize,
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
        } else {
            None
        }
    }
}

struct FrameStack {
    stack: Vec<Frame>,
    top: Frame,
}

impl FrameStack {
    fn push(&mut self, function: NonNull<FunctionValue>) {
        self.stack.push(Frame {
            function: self.top.function,
            locals: std::mem::replace(&mut self.top.locals, Vec::new()),
            pc: self.top.pc,
        });

        self.top.function = function;
        self.top.pc = 0;
    }

    fn pop(&mut self) {
        assert!(self.stack.len() > 0);
        self.top = self.stack.pop().unwrap();
    }
}

struct RustFunctionCtxImpl<'printer> {
    parameters: Vec<RustValue>,
    result: Option<RustValue>,

    printer: &'printer mut dyn Printer,
}

impl RustFunctionCtx for RustFunctionCtxImpl<'_> {
    fn get_parameter(&self, idx: usize) -> Option<RustValue> {
        match self.parameters.get(idx) {
            Some(x) => Some(x.clone()),
            None => None,
        }
    }

    fn get_parameter_float(&self, idx: usize) -> Option<f32> {
        match self.parameters.get(idx) {
            Some(RustValue::Float(f)) => Some(*f),
            _ => None,
        }
    }

    fn get_parameter_integer(&self, idx: usize) -> Option<i32> {
        match self.parameters.get(idx) {
            Some(RustValue::Integer(i)) => Some(*i),
            _ => None,
        }
    }

    fn get_parameter_bool(&self, idx: usize) -> Option<bool> {
        match self.parameters.get(idx) {
            Some(RustValue::Bool(b)) => Some(*b),
            _ => None,
        }
    }

    fn get_parameter_string(&self, idx: usize) -> Option<String> {
        match self.parameters.get(idx) {
            Some(RustValue::Str(s)) => Some(s.clone()),
            _ => None,
        }
    }

    fn set_result(&mut self, value: RustValue) {
        self.result = Some(value);
    }

    fn print(&mut self, s: &str) {
        self.printer.print(s);
    }
}

pub struct VM<'printer> {
    value_stack: Vec<Value>,
    objects: Vec<GcValue>,
    constant_strs: Vec<NonNull<StringValue>>,
    globals: HashMap<NonNull<StringValue>, Value>,
    method_functions: HashMap<usize, NonNull<FunctionValue>>,
    break_loop_flag: bool,

    compiler: Compiler,
    printer: &'printer mut dyn Printer,

    disassemble: bool,
}

struct VMDataSection<'a> {
    objects: &'a mut Vec<GcValue>,
    constant_strs: &'a mut Vec<NonNull<StringValue>>,
}

impl DataSection for VMDataSection<'_> {
    fn create_constant_str(&mut self, s: &str) -> u16 {
        for (idx, string) in self.constant_strs.iter().enumerate() {
            let val = unsafe { &string.as_ref().val };
            if val == s {
                return idx as u16;
            }
        }

        let mut str_val = Box::new(StringValue {
            val: s.to_string(),
            hash: 0,
        });
        let ptr = unsafe { NonNull::new_unchecked(str_val.as_mut() as *mut _) };
        self.objects.push(GcValue::Str(str_val));

        let idx = self.constant_strs.len() as u16;
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
        Value::Char(_) => true,
        Value::Vector(v) => unsafe { v.as_ref().len() > 0 },
        Value::HashMap(h) => unsafe { h.as_ref().len() > 0 },
        Value::Function(_) => true,
        Value::Struct(_) => true,
        Value::RustFunction(_) => true,
        Value::Union(_) => true,
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
                (Value::Char(l), Value::Char(r)) => Value::Bool(l $op r),
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
            method_functions: HashMap::new(),
            break_loop_flag: false,
            compiler: Compiler::new(),
            printer,
            disassemble: false,
        }
    }

    pub fn create_function(
        &mut self,
        signature: &str,
        func: &'static dyn Fn(&mut dyn RustFunctionCtx),
    ) -> Result<(), String> {
        let mut f = Box::new(RustFunctionValue { func });
        let ptr = unsafe { NonNull::new_unchecked(f.as_mut() as *mut _) };
        self.objects.push(GcValue::RustFunction(f));

        let mut data_section = VMDataSection {
            objects: &mut self.objects,
            constant_strs: &mut self.constant_strs,
        };

        let name = self
            .compiler
            .create_function(&mut data_section, signature)?;

        let v = Value::RustFunction(ptr);

        let idx = {
            let mut data_section = VMDataSection {
                objects: &mut self.objects,
                constant_strs: &mut self.constant_strs,
            };

            data_section.create_constant_str(&name) as usize
        };

        let n = self.constant_strs[idx];

        self.globals.insert(n, v);
        Ok(())
    }

    fn run(&mut self, chunk: Chunk) {
        let mut frame_stack = FrameStack {
            stack: Vec::new(),
            top: Frame {
                function: self.function(chunk),
                locals: Vec::new(),
                pc: 0,
            },
        };

        let mut pc = 0;
        while let Some(op) = frame_stack.top.next_code() {
            if self.disassemble {
                let mut ds = disassembler::Disassembler::new(unsafe {
                    &frame_stack.top.function.as_ref().chunk.code
                });
                ds.set_offset(pc);
                ds.disassemble_one();
            }

            match op {
                opcode::CONSTANT_INTEGER => self.push_integer(&mut frame_stack.top),
                opcode::CONSTANT_FLOAT => self.push_float(&mut frame_stack.top),
                opcode::CONSTANT_STRING => self.push_constant_string(&mut frame_stack.top),
                opcode::CONSTANT_BOOL => self.push_constant_bool(&mut frame_stack.top),
                opcode::CONSTANT_CHAR => self.push_constant_char(&mut frame_stack.top),
                opcode::CREATE_VEC => self.create_vec(&mut frame_stack.top),
                opcode::CREATE_HASH_MAP => self.create_hash_map(&mut frame_stack.top),
                opcode::CREATE_TUPLE => self.create_tuple(&mut frame_stack.top),
                opcode::CREATE_STRUCT => self.create_struct(&mut frame_stack.top),
                opcode::CREATE_UNION => self.create_union(&mut frame_stack.top),
                opcode::GET_INDEX => self.get_index(),
                opcode::SET_INDEX => self.set_index(),
                opcode::GET_FIELD => self.get_field(&mut frame_stack.top),
                opcode::SET_FIELD => self.set_field(&mut frame_stack.top),
                opcode::SET_GLOBAL => self.set_global(&mut frame_stack.top),
                opcode::SET_LOCAL => self.set_local(&mut frame_stack.top),
                opcode::PUSH_GLOBAL => self.push_global(&mut frame_stack.top),
                opcode::PUSH_LOCAL => self.push_local(&mut frame_stack.top),
                opcode::PUSH_METHOD => self.push_method(&mut frame_stack.top),
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
                opcode::JMP_IF_DETERMINANT_MISMATCH => {
                    self.jmp_if_determinant_mismatch(&mut frame_stack.top)
                }
                opcode::READ_INPUT => self.read_input(&mut frame_stack.top),
                opcode::CALL => self.call(&mut frame_stack),
                opcode::CALL_INTERFACE => self.call_interface(&mut frame_stack),
                opcode::CALL_BUILTIN => self.call_builtin(&mut frame_stack),
                opcode::RETURN => self.do_return(&mut frame_stack),
                _ => {
                    panic!("Unknown instruction: {} @ {}", op, frame_stack.top.pc - 1)
                }
            }
            pc = frame_stack.top.pc;

            if self.disassemble {
                println!("STACK ({}): {:?}", pc, self.value_stack);
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

        let (chunk, global_functions, method_functions) =
            self.compiler.compile(&mut data_section, src)?;

        for (fname, fchunk) in global_functions {
            if self.disassemble {
                let mut ds = disassembler::Disassembler::new(&fchunk.code);
                println!("-- Function: {} --", fname);
                ds.disassemble_all();
            }

            let function_value = self.function(fchunk);
            let mut data_section = VMDataSection {
                objects: &mut self.objects,
                constant_strs: &mut self.constant_strs,
            };
            let name_idx = data_section.create_constant_str(&fname) as usize;
            self.globals.insert(
                self.constant_strs[name_idx],
                Value::Function(function_value),
            );
        }

        for (fidx, fchunk) in method_functions {
            if self.disassemble {
                let mut ds = disassembler::Disassembler::new(&fchunk.code);
                println!("-- Function: {} --", fidx);
                ds.disassemble_all();
            }

            let function_value = self.function(fchunk);
            self.method_functions.insert(fidx, function_value);
        }

        if self.disassemble {
            println!("-- Main --");
            let mut ds = disassembler::Disassembler::new(&chunk.code);
            ds.disassemble_all();
            println!("\n\n");
        }

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
        while !decoder.step_decode(frame.next_code().unwrap()) {}

        self.value_stack.push(Value::Integer(decoder.val()));
    }

    fn push_float(&mut self, frame: &mut Frame) {
        let code = unsafe { &frame.function.as_ref().chunk.code };

        let value = float::decode(&code[frame.pc..frame.pc + 4].try_into().unwrap());
        frame.pc += 4;

        self.value_stack.push(Value::Float(OrderedFloat(value)));
    }

    fn push_constant_string(&mut self, frame: &mut Frame) {
        let mut decoder = var_len_int::Decoder::new();
        while !decoder.step_decode(frame.next_code().unwrap()) {}

        self.value_stack
            .push(Value::Str(self.constant_strs[decoder.val() as usize]));
    }

    fn push_constant_bool(&mut self, frame: &mut Frame) {
        let value = frame.next_code().unwrap() != 0;

        self.value_stack.push(Value::Bool(value));
    }

    fn push_constant_char(&mut self, frame: &mut Frame) {
        let len = frame.next_code().unwrap() as usize;

        let mut buf: [u8; 4] = [0; 4];
        for i in 0..len {
            buf[i] = frame.next_code().unwrap();
        }

        let c = std::str::from_utf8(&buf)
            .expect("Invalid character encoding!")
            .chars()
            .next()
            .expect("Invalid character encoding!");
        self.value_stack.push(Value::Char(c));
    }

    fn create_vec(&mut self, frame: &mut Frame) {
        let arg_count = frame.next_code().unwrap() as usize;

        assert!(self.value_stack.len() >= arg_count);
        let vector: Vec<Value> = self
            .value_stack
            .drain((self.value_stack.len() - arg_count)..)
            .collect();

        let mut vec_val = Box::new(vector);
        let ptr = unsafe { NonNull::new_unchecked(vec_val.as_mut() as *mut _) };
        self.objects.push(GcValue::Vector(vec_val));

        self.value_stack.push(Value::Vector(ptr));
    }

    fn get_index(&mut self) {
        assert!(self.value_stack.len() > 1);
        let index = self.value_stack.pop().unwrap();

        match self.value_stack.pop().unwrap() {
            Value::HashMap(h) => {
                let hash_map = unsafe { h.as_ref() };
                self.value_stack.push(hash_map[&index]);
            }
            Value::Vector(v) => {
                let vector = unsafe { v.as_ref() };
                let index = match index {
                    Value::Integer(i) => i as usize,
                    _ => panic!("Bad index value"),
                };
                self.value_stack.push(vector[index]);
            }
            Value::Str(s) => {
                let string = &unsafe { s.as_ref() }.val;
                let index = match index {
                    Value::Integer(i) => i as usize,
                    _ => panic!("Bad index value"),
                };

                // Really slow! Just getting this working
                self.value_stack
                    .push(Value::Char(string.chars().nth(index).unwrap()));
            }
            _ => panic!("Not a hash map"),
        };
    }

    fn set_index(&mut self) {
        assert!(self.value_stack.len() > 2);
        let new_value = self.value_stack.pop().unwrap();
        let index = self.value_stack.pop().unwrap();
        match self.value_stack.pop().unwrap() {
            Value::HashMap(mut h) => {
                let hash_map = unsafe { h.as_mut() };
                hash_map.insert(index, new_value);
                self.value_stack.push(new_value);
            }
            Value::Vector(mut v) => {
                let vector = unsafe { v.as_mut() };
                let index = match index {
                    Value::Integer(i) => i as usize,
                    _ => panic!("Bad index value"),
                };
                vector[index] = new_value;
                self.value_stack.push(new_value);
            }
            _ => panic!("Not a vector"),
        };
    }

    fn get_field(&mut self, frame: &mut Frame) {
        assert!(self.value_stack.len() > 0);
        let index = frame.next_code().unwrap() as usize;

        match self.value_stack.pop().unwrap() {
            Value::Struct(s) => {
                let struct_value = unsafe { s.as_ref() };
                self.value_stack.push(struct_value.members[index]);
            }
            _ => panic!("Not a struct"),
        };
    }

    fn set_field(&mut self, frame: &mut Frame) {
        assert!(self.value_stack.len() > 1);
        let index = frame.next_code().unwrap() as usize;

        let value = self.value_stack.pop().unwrap();

        match self.value_stack.pop().unwrap() {
            Value::Struct(mut s) => {
                let struct_value = unsafe { s.as_mut() };
                struct_value.members[index] = value;
            }
            _ => panic!("Not a struct"),
        };
    }

    fn create_hash_map(&mut self, frame: &mut Frame) {
        let arg_count = frame.next_code().unwrap() as usize * 2;

        assert!(self.value_stack.len() >= arg_count);
        let args: Vec<Value> = self
            .value_stack
            .drain((self.value_stack.len() - arg_count)..)
            .collect();

        let mut hash_map = HashMap::<Value, Value>::new();

        for kv in args.chunks(2) {
            hash_map.insert(kv[0], kv[1]);
        }

        let mut hash_map_val = Box::new(hash_map);
        let ptr = unsafe { NonNull::new_unchecked(hash_map_val.as_mut() as *mut _) };
        self.objects.push(GcValue::HashMap(hash_map_val));

        self.value_stack.push(Value::HashMap(ptr));
    }

    fn create_tuple(&mut self, frame: &mut Frame) {
        let elem_count = frame.next_code().unwrap() as usize;
        assert!(self.value_stack.len() >= elem_count);

        let elements: Vec<Value> = self
            .value_stack
            .drain((self.value_stack.len() - elem_count)..)
            .collect();

        let mut struct_val = Box::new(StructValue { members: elements });
        let ptr = unsafe { NonNull::new_unchecked(struct_val.as_mut() as *mut _) };
        self.objects.push(GcValue::Struct(struct_val));

        self.value_stack.push(Value::Struct(ptr));
    }

    fn create_struct(&mut self, frame: &mut Frame) {
        let member_count = frame.next_code().unwrap() as usize;

        let mut members = vec![Value::Integer(0); member_count];

        for _ in 0..member_count {
            let member_idx = frame.next_code().unwrap() as usize;
            let value = self.value_stack.pop().unwrap();

            members[member_idx] = value;
        }

        let mut struct_val = Box::new(StructValue { members });
        let ptr = unsafe { NonNull::new_unchecked(struct_val.as_mut() as *mut _) };
        self.objects.push(GcValue::Struct(struct_val));

        self.value_stack.push(Value::Struct(ptr));
    }

    fn create_union(&mut self, frame: &mut Frame) {
        let member_count = frame.next_code().unwrap() as usize;
        let determinant = frame.next_code().unwrap() as usize;

        let mut members = vec![Value::Integer(0); member_count];

        for idx in (0..member_count).rev() {
            let value = self.value_stack.pop().unwrap();
            members[idx] = value;
        }

        let mut union_val = Box::new(UnionValue {
            determinant,
            members,
        });
        let ptr = unsafe { NonNull::new_unchecked(union_val.as_mut() as *mut _) };
        self.objects.push(GcValue::Union(union_val));

        self.value_stack.push(Value::Union(ptr));
    }

    fn set_global(&mut self, frame: &mut Frame) {
        let idx = frame.next_code().unwrap() as usize;

        let name = self.constant_strs[idx];
        assert!(self.globals.contains_key(&name));
        assert!(!self.value_stack.is_empty());
        self.globals.insert(name, self.value_stack.pop().unwrap());
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

    fn push_method(&mut self, frame: &mut Frame) {
        let mut idx = frame.next_code().unwrap() as usize;
        idx |= (frame.next_code().unwrap() as usize) << 8;

        assert!(self.method_functions.contains_key(&idx));
        self.value_stack
            .push(Value::Function(self.method_functions[&idx]));
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
            Value::Char(c) => format!("{}", c),
            Value::Vector(v) => Self::format_vec(unsafe { v.as_ref() }),
            Value::HashMap(h) => Self::format_hash_map(unsafe { h.as_ref() }),
            Value::Function(f) => format!("function<0x{:x}>", f.as_ptr() as usize),
            Value::Struct(s) => format!("struct<0x{:x}>", s.as_ptr() as usize),
            Value::RustFunction(_) => format!("rust_function"),
            Value::Union(u) => format!("union<0x{:x}> {:?}", u.as_ptr() as usize, unsafe {
                u.as_ref()
            }),
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

    fn jmp_if_determinant_mismatch(&mut self, frame: &mut Frame) {
        assert!(!self.value_stack.is_empty());
        let top_value = self.value_stack.pop().unwrap();

        let code = unsafe { &frame.function.as_ref().chunk.code };
        let check_determinant = code[frame.pc] as usize;
        frame.pc += 1;

        let union_value = match top_value {
            Value::Union(u) => unsafe { u.as_ref() },
            x => panic!("Unexpected non-union! {:?}", x),
        };

        let offset = code[frame.pc] as usize;
        if union_value.determinant == check_determinant {
            for m in &union_value.members {
                self.value_stack.push(*m);
            }
            frame.pc += 1;
        } else {
            self.value_stack.pop();
            frame.pc += offset;
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
        let arity = frame_stack.top.next_code().unwrap() as usize;

        assert!(!self.value_stack.is_empty());

        let top = self.value_stack.pop().unwrap();
        if let Value::RustFunction(f) = top {
            let mut parameters = vec![];

            let len = self.value_stack.len();
            for p in self.value_stack.drain(len - arity..) {
                parameters.push(match p {
                    Value::Float(f) => RustValue::Float(f.into_inner()),
                    Value::Integer(i) => RustValue::Integer(i),
                    Value::Str(s) => RustValue::Str(unsafe { s.as_ref() }.val.clone()),
                    Value::Bool(b) => RustValue::Bool(b),
                    _ => panic!("Invalid parameter type in rust function"),
                });
            }

            let mut ctx = RustFunctionCtxImpl {
                parameters,
                result: None,
                printer: self.printer,
            };
            unsafe { f.as_ref() }.call(&mut ctx);

            if let Some(r) = ctx.result {
                match r {
                    RustValue::Float(f) => self.value_stack.push(Value::Float(OrderedFloat(f))),
                    RustValue::Integer(i) => self.value_stack.push(Value::Integer(i)),
                    RustValue::Str(s) => {
                        let mut data_section = VMDataSection {
                            objects: &mut self.objects,
                            constant_strs: &mut self.constant_strs,
                        };

                        let idx = data_section.create_constant_str(&s) as usize;
                        self.value_stack.push(Value::Str(self.constant_strs[idx]));
                    }
                    RustValue::Bool(b) => self.value_stack.push(Value::Bool(b)),
                    RustValue::StringVector(v) => {
                        let mut vector: Vec<Value> = vec![];

                        for s in v {
                            let idx = {
                                let mut data_section = VMDataSection {
                                    objects: &mut self.objects,
                                    constant_strs: &mut self.constant_strs,
                                };
                                data_section.create_constant_str(&s) as usize
                            };

                            vector.push(Value::Str(self.constant_strs[idx]));
                        }

                        let mut vec_val = Box::new(vector);
                        let ptr = unsafe { NonNull::new_unchecked(vec_val.as_mut() as *mut _) };
                        self.objects.push(GcValue::Vector(vec_val));

                        self.value_stack.push(Value::Vector(ptr));
                    }
                }
            }
        } else {
            let function = match top {
                Value::Function(f) => f,
                x => panic!("Top of stack is not a function! Got {:?}", x),
            };

            frame_stack.push(function);
            let len = self.value_stack.len();
            frame_stack.top.locals = self.value_stack.drain(len - arity..).collect();
        }
    }

    fn call_interface(&mut self, frame_stack: &mut FrameStack) {
        let arity = frame_stack.top.next_code().unwrap() as usize;
        let method_slot_idx = frame_stack.top.next_code().unwrap() as usize;

        assert!(!self.value_stack.is_empty());
        assert!(self.value_stack.len() >= arity);

        let len = self.value_stack.len();
        let interface_stack_idx = len - arity;

        let interface = match self.value_stack[interface_stack_idx] {
            Value::Struct(s) => s,
            _ => panic!(
                "Expected interface object @ stack idx {}",
                interface_stack_idx
            ),
        };

        let interface = unsafe { interface.as_ref() };

        let function = match interface.members[method_slot_idx] {
            Value::Function(f) => f,
            x => panic!("Interface method slot is not a function! Got {:?}", x),
        };

        frame_stack.push(function);
        frame_stack.top.locals = self.value_stack.drain(len - arity..).collect();
        frame_stack.top.locals[0] = interface.members[0]; // switch 'self' into first local slot
    }

    fn call_builtin(&mut self, frame_stack: &mut FrameStack) {
        let builtin_id = frame_stack.top.next_code().unwrap();
        let arg_count = frame_stack.top.next_code().unwrap() as usize;

        let obj = {
            let len = self.value_stack.len();
            self.value_stack.remove(len - arg_count - 1)
        };

        match obj {
            Value::Vector(v) => match builtin_id {
                builtin_functions::vector::LEN => {
                    self.value_stack
                        .push(Value::Integer(unsafe { v.as_ref() }.len() as i32));
                }
                _ => panic!("Expected vector builtin id {}", builtin_id),
            },
            Value::Str(s) => match builtin_id {
                builtin_functions::string::LEN => {
                    self.value_stack
                        .push(Value::Integer(unsafe { s.as_ref() }.val.len() as i32));
                }
                builtin_functions::string::TO_INTEGER => {
                    self.value_stack.push(Value::Integer(
                        unsafe { s.as_ref() }.val.parse::<i32>().unwrap_or(0),
                    ));
                }
                _ => panic!("Expected string builtin id {}", builtin_id),
            },
            Value::HashMap(h) => match builtin_id {
                builtin_functions::hashmap::CONTAINS_KEY => {
                    let key = self.value_stack.pop().unwrap();
                    self.value_stack
                        .push(Value::Bool(unsafe { h.as_ref() }.contains_key(&key)));
                }
                builtin_functions::hashmap::KEYS => {
                    let mut vector = vec![];

                    for k in unsafe { h.as_ref() }.keys() {
                        vector.push(*k);
                    }

                    let mut vec_val = Box::new(vector);
                    let ptr = unsafe { NonNull::new_unchecked(vec_val.as_mut() as *mut _) };
                    self.objects.push(GcValue::Vector(vec_val));

                    self.value_stack.push(Value::Vector(ptr));
                }
                _ => panic!("Unexpected hash_map builtin id {}", builtin_id),
            },
            x => panic!("Must be a vector, hash_map string. Got {:?}", x),
        };
    }

    fn do_return(&mut self, frame_stack: &mut FrameStack) {
        frame_stack.pop();
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::test::utils::TestPrinter;

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

    #[test]
    fn print_char() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);
        let src = "
            let s: string = \"hello\";
            print(s[0]);
            print(s[3]);
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "h");
        assert_eq!(printer.strings[1], "l");
    }

    #[test]
    fn string_index() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);
        assert_eq!(vm.interpret("print 'a';"), Ok(()));
        assert_eq!(printer.strings.len(), 1);
        assert_eq!(printer.strings[0], "a");
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

    #[test]
    fn function_recursion() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            fn test(i: int) {
                if i > 0 {
                    test(i-1);
                }
                print i;
            }

            test(3);
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 4);
        assert_eq!(printer.strings[0], "0");
        assert_eq!(printer.strings[1], "1");
        assert_eq!(printer.strings[2], "2");
        assert_eq!(printer.strings[3], "3");
    }

    #[test]
    fn fib() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            fn fib(i: int) -> int {
                if i <= 1 {
                    return i;
                }
                return fib(i - 1) + fib(i - 2);
            }

            print fib(10);";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 1);
        assert_eq!(printer.strings[0], "55");
    }

    #[test]
    fn test_use_enum() {
        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);

            let src = "
                enum Enum : int {
                    a = 1,
                    b = 2,
                }

                let mut e: Enum = Enum.a;
                print e;
                e = Enum.b;
                print e;";

            assert_eq!(vm.interpret(src), Ok(()));
            assert_eq!(printer.strings.len(), 2);
            assert_eq!(printer.strings[0], "1");
            assert_eq!(printer.strings[1], "2");
        }

        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);

            let src = "
                enum Enum : float {
                    a = 1.1,
                    b = 2.2,
                }

                let mut e: Enum = Enum.a;
                print e;
                e = Enum.b;
                print e;";

            assert_eq!(vm.interpret(src), Ok(()));
            assert_eq!(printer.strings.len(), 2);
            assert_eq!(printer.strings[0], "1.1");
            assert_eq!(printer.strings[1], "2.2");
        }

        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);

            let src = "
                enum Enum : string {
                    a = \"hello\",
                    b = \"world\",
                }

                let mut e: Enum = Enum.a;
                print e;
                e = Enum.b;
                print e;";

            assert_eq!(vm.interpret(src), Ok(()));
            assert_eq!(printer.strings.len(), 2);
            assert_eq!(printer.strings[0], "hello");
            assert_eq!(printer.strings[1], "world");
        }
    }

    #[test]
    fn test_struct_get_fields() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        {
            let src = "
                struct Struct
                {
                    i: int,
                    f: float,
                    s: string,
                }

                let s: Struct = Struct {
                    f = 2.3,
                    i = 1,
                    s = \"hello world\",
                };";
            assert_eq!(vm.interpret(src), Ok(()));
        }

        {
            let src = "
                print s.s;
                print s.f;
                print s.i;
            ";
            assert_eq!(vm.interpret(src), Ok(()));
        }

        assert_eq!(printer.strings.len(), 3);
        assert_eq!(printer.strings[0], "hello world");
        assert_eq!(printer.strings[1], "2.3");
        assert_eq!(printer.strings[2], "1");
    }

    #[test]
    fn test_struct_set_fields() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        {
            let src = "
                struct Struct
                {
                    i: int,
                    f: float,
                    s: string,
                }

                let mut s: Struct = Struct {
                    f = 2.3,
                    i = 1,
                    s = \"hello world\",
                };";
            assert_eq!(vm.interpret(src), Ok(()));
        }

        {
            let src = "
                s.s = \"new string\";
                s.f = 3.142;
                s.i = 5;
            ";
            assert_eq!(vm.interpret(src), Ok(()));
        }

        {
            let src = "
                print s.s;
                print s.f;
                print s.i;
            ";
            assert_eq!(vm.interpret(src), Ok(()));
        }

        assert_eq!(printer.strings.len(), 3);
        assert_eq!(printer.strings[0], "new string");
        assert_eq!(printer.strings[1], "3.142");
        assert_eq!(printer.strings[2], "5");
    }

    #[test]
    fn test_struct_nested_fields() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        {
            let src = "
                struct Nested {
                    inner: string,
                }

                struct Struct
                {
                    nested: Nested,
                }

                let mut s: Struct = Struct {
                    nested = Nested {
                        inner = \"Nested!\",
                    },
                };";
            assert_eq!(vm.interpret(src), Ok(()));
        }

        {
            let src = "
                print s.nested.inner;
            ";
            assert_eq!(vm.interpret(src), Ok(()));
        }

        {
            let src = "
                s.nested.inner = \"new string\";
            ";
            assert_eq!(vm.interpret(src), Ok(()));
        }

        {
            let src = "
                print s.nested.inner;
            ";
            assert_eq!(vm.interpret(src), Ok(()));
        }

        {
            let src = "
                s.nested = Nested {
                    inner = \"New instance\",
                };
                print s.nested.inner;
            ";
            assert_eq!(vm.interpret(src), Ok(()));
        }

        assert_eq!(printer.strings.len(), 3);
        assert_eq!(printer.strings[0], "Nested!");
        assert_eq!(printer.strings[1], "new string");
        assert_eq!(printer.strings[2], "New instance");
    }

    #[test]
    fn test_struct_with_methods() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
                struct Struct {
                    i: int,
                    f: float,
                }
                impl {
                    fn (self) test() {
                        print \"Test method\";
                        print self.i;
                    }
                    fn (self) test2(s: string) {
                        print \"Test 2\";
                        print self.f;
                        print s;
                    }
                }

                let mut s: Struct = Struct {
                    i = 10,
                    f = 3.142,
                };
                s.test();
                s.i = 20;
                s.test();
                s.test2(\"Hello method\");
                s.f = 2.1;
                s.test2(\"Bye method\");
                ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 10);
        assert_eq!(printer.strings[0], "Test method");
        assert_eq!(printer.strings[1], "10");
        assert_eq!(printer.strings[2], "Test method");
        assert_eq!(printer.strings[3], "20");
        assert_eq!(printer.strings[4], "Test 2");
        assert_eq!(printer.strings[5], "3.142");
        assert_eq!(printer.strings[6], "Hello method");
        assert_eq!(printer.strings[7], "Test 2");
        assert_eq!(printer.strings[8], "2.1");
        assert_eq!(printer.strings[9], "Bye method");
    }

    #[test]
    fn test_union_constructor() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
                union Union
                {
                    I(int), F(float), S(string),
                }

                let mut u: Union = Union.I(9,);
                print(u);

                u = Union.S(\"Hello world\",);
                print(u);

                u = Union.F(3.142,);
                print(u);
                ";

        assert_eq!(vm.interpret(src), Ok(()));
    }

    #[test]
    fn test_if_let() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "union Union
                {
                    I(int),
                    F(float),
                    S(string),
                }

                let mut o: Union = Union.I(10,);
                if let Union.I(x,) = o {
                    print(x);
                }
                else {
                    print(\"Mismatch\");
                }

                if let Union.F(x,) = o {
                    print(x);
                }
                else {
                    print(\"Mismatch\");
                }

                o = Union.F(3.142,);
                if let Union.F(x,) = o {
                    print(x);
                }
                else {
                    print(\"Mismatch\");
                }

                if let Union.I(x,) = o {
                    print(x);
                }
                else {
                    print(\"Mismatch\");
                }
                ";
        assert_eq!(vm.interpret(src), Ok(()));

        assert_eq!(printer.strings.len(), 4);
        assert_eq!(printer.strings[0], "10");
        assert_eq!(printer.strings[1], "Mismatch");
        assert_eq!(printer.strings[2], "3.142");
        assert_eq!(printer.strings[3], "Mismatch");
    }

    #[test]
    fn test_if_let_multiple() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "union Composite
                {
                    Two(int, float),
                    Three(int, float, string),
                }

                let mut c: Composite = Composite.Two(10,1.23,);
                if let Composite.Two(x, y,) = c {
                    print(x);
                    print(y);
                }

                if let Composite.Three(x, y, z,) = c {
                    print(\"SHOULD NOT GET HERE\");
                }
                else {
                    print(\"Mismatch\");
                }

                c = Composite.Three(300, 3.142, \"Hello World\",);
                if let Composite.Three(x, y, z,) = c {
                    print(x);
                    print(y);
                    print(z);
                }

                if let Composite.Two(x, y,) = c {
                    print(\"SHOULD NOT GET HERE\");
                }
                else {
                    print(\"Mismatch\");
                }

                ";
        assert_eq!(vm.interpret(src), Ok(()));

        assert_eq!(printer.strings.len(), 7);
        assert_eq!(printer.strings[0], "10");
        assert_eq!(printer.strings[1], "1.23");
        assert_eq!(printer.strings[2], "Mismatch");
        assert_eq!(printer.strings[3], "300");
        assert_eq!(printer.strings[4], "3.142");
        assert_eq!(printer.strings[5], "Hello World");
        assert_eq!(printer.strings[6], "Mismatch");
    }

    #[test]
    fn test_tuple() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            type_alias MyTuple = (int, float, string,);

            let t: MyTuple = tuple (10, 3.142, \"Hello world\",);
            print(t.0);
            print(t.1);
            print(t.2);
            ";

        assert_eq!(vm.interpret(src), Ok(()));

        assert_eq!(printer.strings.len(), 3);
        assert_eq!(printer.strings[0], "10");
        assert_eq!(printer.strings[1], "3.142");
        assert_eq!(printer.strings[2], "Hello world");
    }

    #[test]
    fn test_union_tuple() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            union Option<T> { Some(T), None, }

            type_alias O = Option<(int, float,),>;
            let o: O = O.Some(tuple(9, 1.23,),);

            if let O.Some(x,) = o {
                print(x.0);
                print(x.1);
            }
            ";

        assert_eq!(vm.interpret(src), Ok(()));

        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "9");
        assert_eq!(printer.strings[1], "1.23");
    }

    #[test]
    fn test_function_pointer() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
                fn function(i:int) {
                    print(\"function\");
                    print(i);
                }

                fn function2(i:int) {
                    print(\"function2\");
                    print(i);
                }

                fn function3(i:int) {
                    print(\"function3\");
                    print(i);
                }

                fn call(local: fn(int), i: int) {
                    print(\"local\");
                    local(i);
                }

                let mut f: fn(int) = function;
                f(10);
                f = function2;
                f(20);
                f = function3;
                f(30);
                call(f, 40);
                ";

        assert_eq!(vm.interpret(src), Ok(()));

        assert_eq!(printer.strings.len(), 9);
        assert_eq!(printer.strings[0], "function");
        assert_eq!(printer.strings[1], "10");
        assert_eq!(printer.strings[2], "function2");
        assert_eq!(printer.strings[3], "20");
        assert_eq!(printer.strings[4], "function3");
        assert_eq!(printer.strings[5], "30");
        assert_eq!(printer.strings[6], "local");
        assert_eq!(printer.strings[7], "function3");
        assert_eq!(printer.strings[8], "40");
    }

    #[test]
    fn test_interface() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
                interface Interface {
                    fn call(i: int) -> int
                    fn call2(f: float) -> float
                }

                struct S {}
                impl {
                    fn (self) call(i: int) -> int { return i; }
                    fn (self) call2(f: float) -> float { return f; }
                }

                struct S2 {}
                impl {
                    fn (self) call(i: int) -> int { return i * 2; }
                    fn (self) call2(f: float) -> float { return f * 2.0; }
                }

                fn call_interface(i: Interface) {
                    print i.call(11);
                    print i.call2(2.7);
                }

                let mut it: Interface = S {};
                print it.call(10);
                print it.call2(3.142);

                it = S2 {};
                print it.call(10);
                print it.call2(3.142);

                call_interface(S{});
                call_interface(S2{});
                ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 8);
        assert_eq!(printer.strings[0], "10");
        assert_eq!(printer.strings[1], "3.142");
        assert_eq!(printer.strings[2], "20");
        assert_eq!(printer.strings[3], "6.284");
        assert_eq!(printer.strings[4], "11");
        assert_eq!(printer.strings[5], "2.7");
        assert_eq!(printer.strings[6], "22");
        assert_eq!(printer.strings[7], "5.4");
    }

    fn test_call_function(ctx: &mut dyn RustFunctionCtx) {
        ctx.print("TEST!");
    }

    #[test]
    fn test_call() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        if let Err(e) = vm.create_function("fn test()", &test_call_function) {
            panic!("{}", e);
        }

        let src = "
            test();
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 1);
        assert_eq!(printer.strings[0], "TEST!");
    }

    fn test_call_return_function_string(ctx: &mut dyn RustFunctionCtx) {
        ctx.set_result(RustValue::Str("TEST RETURN!".to_string()));
    }

    fn test_call_return_function_float(ctx: &mut dyn RustFunctionCtx) {
        ctx.set_result(RustValue::Float(3.142));
    }

    fn test_call_return_function_integer(ctx: &mut dyn RustFunctionCtx) {
        ctx.set_result(RustValue::Integer(1234));
    }

    #[test]
    fn test_call_return() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        if let Err(e) = vm.create_function(
            "fn test_string() -> string",
            &test_call_return_function_string,
        ) {
            panic!("{}", e);
        }

        if let Err(e) =
            vm.create_function("fn test_float() -> float", &test_call_return_function_float)
        {
            panic!("{}", e);
        }

        if let Err(e) = vm.create_function(
            "fn test_integer() -> int",
            &test_call_return_function_integer,
        ) {
            panic!("{}", e);
        }

        let src = "
            print(test_string());
            print(test_float());
            print(test_integer());
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 3);
        assert_eq!(printer.strings[0], "TEST RETURN!");
        assert_eq!(printer.strings[1], "3.142");
        assert_eq!(printer.strings[2], "1234");
    }

    fn test_call_return_vector_function(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        v.push("Hello".to_string());
        v.push("World".to_string());
        v.push("Kyx".to_string());
        ctx.set_result(RustValue::StringVector(v));
    }

    #[test]
    fn test_call_return_vector() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        if let Err(e) =
            vm.create_function("fn test() -> [string]", &test_call_return_vector_function)
        {
            panic!("{}", e);
        }

        let src = "
            let s: [string] = test();
            print(s[0]);
            print(s[1]);
            print(s[2]);
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 3);
        assert_eq!(printer.strings[0], "Hello");
        assert_eq!(printer.strings[1], "World");
        assert_eq!(printer.strings[2], "Kyx");
    }

    fn test_call_parameters_function(ctx: &mut dyn RustFunctionCtx) {
        let f = ctx.get_parameter_float(0).unwrap();
        let i = ctx.get_parameter_integer(1).unwrap();
        let s = ctx.get_parameter_string(2).unwrap();

        ctx.print(&f.to_string());
        ctx.print(&i.to_string());
        ctx.print(&s);
    }

    #[test]
    fn test_call_parameters() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        if let Err(e) = vm.create_function(
            "fn test(float, int, string)",
            &test_call_parameters_function,
        ) {
            panic!("{}", e);
        }

        let src = "
            test(2.14, 4321, \"kyx\");
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 3);
        assert_eq!(printer.strings[0], "2.14");
        assert_eq!(printer.strings[1], "4321");
        assert_eq!(printer.strings[2], "kyx");
    }

    #[test]
    fn test_vector_builtin() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let v: [int] = vec<int>{1,2,3,4,5};
            print(v.len());
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 1);
        assert_eq!(printer.strings[0], "5");
    }

    #[test]
    fn test_string_builtin() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let mut s: string = \"hello\";
            print(s.len());

            print(\"-10\".to_integer());
            print(\"10\".to_integer());

            let a: int = \"2\".to_integer();
            let b: int = \"5\".to_integer();
            print(a + b);
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 4);
        assert_eq!(printer.strings[0], "5");
        assert_eq!(printer.strings[1], "-10");
        assert_eq!(printer.strings[2], "10");
        assert_eq!(printer.strings[3], "7");
    }

    #[test]
    fn test_hashmap_builtin() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let mut h: [int: int] = hash_map<int, int>{};

            print(h.contains_key(10));
            h[10] = 1;
            print(h.contains_key(10));

            h[20] = 3;
            h[21] = 4;

            let k: [int] = h.keys();
            let n: int = k.len();
            let mut i: int = 0;

            while i < n {
                print(k[i]);
                i = i + 1;
            }
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 5);
        assert_eq!(printer.strings[0], "false");
        assert_eq!(printer.strings[1], "true");

        // Keys can be in any order
        printer.strings[2..5].sort();
        assert_eq!(printer.strings[2], "10");
        assert_eq!(printer.strings[3], "20");
        assert_eq!(printer.strings[4], "21");
    }
}
