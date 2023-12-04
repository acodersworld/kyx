use std::collections::HashMap;
use std::ptr::NonNull;
use std::vec::Vec;
use std::cmp::Ordering;

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
    FunctionValue, GcValue, RustFunctionValue, VectorValue, StringValue, StructValue, UnionValue, Value,
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

    fn next_short(&mut self) -> Option<u16> {
        let code = unsafe { &self.function.as_ref().chunk.code };
        //println!("{}", opcode::to_string(op));
        //println!("{:?}", self.value_stack);
        //
        let pc = self.pc;
        if pc < (code.len() + 1) {
            self.pc += 2;
            let mut bytes: [u8; 2] = Default::default();
            bytes.copy_from_slice(&code[pc..pc + 2]);
            Some(u16::from_be_bytes(bytes))
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

    fn get_parameter_float(&self, idx: usize) -> Option<f64> {
        match self.parameters.get(idx) {
            Some(RustValue::Float(f)) => Some(*f),
            _ => None,
        }
    }

    fn get_parameter_integer(&self, idx: usize) -> Option<i64> {
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

        let mut len = 0;
        let mut is_ascii = true;
        for c in s.chars() {
            if is_ascii && !c.is_ascii() {
                is_ascii = false;
            }
            len += 1;
        }

        let mut str_val = Box::new(StringValue {
            val: s.to_string(),
            len,
            is_ascii,
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
                (Value::Bool(l), Value::Bool(r)) => Value::Bool(l $op r),
                _ => panic!("")
            };

            *st.last_mut().unwrap() = result;
        }
    }
}

macro_rules! bin_op_logical {
    ($self:ident, $op:tt) => {
        {
            let st = &mut $self.value_stack;

            let right = st.pop().unwrap();
            let left = st.last().unwrap();

            let result = match (left, right) {
                (Value::Bool(l), Value::Bool(r)) => Value::Bool(*l $op r),
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
                (l, r) => panic!("Expected ints or floats, got {:?} / {:?}", l, r)
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

        self.run_with_frame(&mut frame_stack);
    }

    fn run_with_frame(&mut self, frame_stack: &mut FrameStack) {
        let mut pc = 0;
        let start_stack_size = frame_stack.stack.len();
        while let Some(op) = frame_stack.top.next_code() {
            //println!("PC: {}", pc);
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
                opcode::NOT => self.not(),
                opcode::ADD => self.add(),
                opcode::SUB => self.sub(),
                opcode::MUL => self.mul(),
                opcode::DIV => self.div(),
                opcode::MOD => self.modulus(),
                opcode::LOGICAL_AND => self.logical_and(),
                opcode::LOGICAL_OR => self.logical_or(),
                opcode::EQ => self.equals(),
                opcode::NEQ => self.not_equals(),
                opcode::LESS => self.less(),
                opcode::LESS_EQUAL => self.less_equal(),
                opcode::GREATER => self.greater(),
                opcode::GREATER_EQUAL => self.greater_equal(),
                opcode::PRINT => self.print(),
                opcode::POP => self.pop(),
                opcode::LOCAL_POP => self.local_pop(&mut frame_stack.top),
                opcode::LOCAL_SET_SIZE => self.local_set_size(&mut frame_stack.top),
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
                opcode::CALL => self.call(frame_stack),
                opcode::CALL_INTERFACE => self.call_interface(frame_stack),
                opcode::CALL_BUILTIN => self.call_builtin(frame_stack),
                opcode::RETURN => {
                    let sz = frame_stack.stack.len();
                    self.do_return(frame_stack);
                    if sz <= start_stack_size {
                        return;
                    }
                },
                _ => {
                    panic!("Unknown instruction: {} @ {}", op, frame_stack.top.pc - 1)
                }
            }
            pc = frame_stack.top.pc;

            if self.disassemble {
                println!("STACK ({}): {:?}", pc, self.value_stack);
                println!("LOCALS: {:?}", frame_stack.top.locals);
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

        if true || self.disassemble {
            println!("-- Main --");
            let mut ds = disassembler::Disassembler::new(&chunk.code);
            ds.disassemble_all();
            println!("\n\n");
        }

        self.run(chunk);

        Ok(())
    }

    fn not(&mut self) {
        let v = match self.value_stack.last_mut().unwrap() {
            Value::Bool(b) => b,
            _ => panic!("Expected boolean value for '!'")
        };

        *v = !(*v);
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

    fn modulus(&mut self) {
        bin_op!(self, %);
    }

    fn logical_and(&mut self) {
        bin_op_logical!(self, &&);
    }

    fn logical_or(&mut self) {
        bin_op_logical!(self, ||);
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

        let value = float::decode(
            &code[frame.pc..frame.pc + std::mem::size_of::<f64>()]
                .try_into()
                .unwrap(),
        );
        frame.pc += std::mem::size_of::<f64>();

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
        let init_type = frame.next_code().unwrap() as usize;

        match init_type {
            0 => {
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
            1 => {
                let init_value = self.value_stack.pop().unwrap();
                let init_size = match self.value_stack.pop().unwrap() {
                    Value::Integer(i) => {
                        if i < 0 {
                            panic!("Vector size init value is less than 0")
                        }

                        i as usize
                    },
                    _ => panic!("Expected integer for vector init size")
                };

                let vector = vec![init_value; init_size];
                let mut vec_val = Box::new(vector);
                let ptr = unsafe { NonNull::new_unchecked(vec_val.as_mut() as *mut _) };
                self.objects.push(GcValue::Vector(vec_val));

                self.value_stack.push(Value::Vector(ptr));
            }
            x => panic!("Unknown vector init type {}", x)
        }
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

                if index >= vector.len() {
                    // TODO: Proper kyx panic
                    panic!("Vector index out of bounds");
                }

                self.value_stack.push(vector[index]);
            }
            Value::Str(s) => {
                let index = match index {
                    Value::Integer(i) => i as usize,
                    _ => panic!("Bad index value"),
                };

                let string = unsafe { s.as_ref() };
                if index >= string.len {
                    // TODO: Proper kyx panic
                    panic!("String index out of bounds");
                }

                if string.is_ascii {
                    let bytes = string.val.as_bytes(); 
                    self.value_stack.push(Value::Char(bytes[index] as char));
                }
                else {
                    self.value_stack
                        .push(Value::Char(string.val.chars().nth(index).unwrap()));
                }
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
//        eprintln!("{:?}", locals);
        self.value_stack.push(locals[idx]);
    }

    fn push_method(&mut self, frame: &mut Frame) {
        let idx = frame.next_short().unwrap() as usize;

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

    fn local_set_size(&mut self, frame: &mut Frame) {
        let size = frame.next_code().unwrap() as usize;
        assert!(size <= frame.locals.len());

        frame.locals.drain(size..);
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
            let offset =
                u16::from_be_bytes(code[frame.pc..frame.pc + 2].try_into().unwrap()) as usize;
            frame.pc -= offset;
        } else {
            frame.pc += 2;
        }
    }

    fn break_loop(&mut self, frame: &mut Frame) {
        let code = unsafe { &frame.function.as_ref().chunk.code };
        let offset = u16::from_be_bytes(code[frame.pc..frame.pc + 2].try_into().unwrap()) as usize;

        frame.pc += offset;
        self.break_loop_flag = true;
    }

    fn jmp(&mut self, frame: &mut Frame) {
        let code = unsafe { &frame.function.as_ref().chunk.code };
        let offset = u16::from_be_bytes(code[frame.pc..frame.pc + 2].try_into().unwrap()) as usize;

        frame.pc += offset;
    }

    fn jmp_if_false(&mut self, frame: &mut Frame) {
        assert!(!self.value_stack.is_empty());
        let cond = self.value_stack.pop().unwrap();

        let code = unsafe { &frame.function.as_ref().chunk.code };
        let offset = u16::from_be_bytes(code[frame.pc..frame.pc + 2].try_into().unwrap()) as usize;

        if !is_truthy(&cond) {
            frame.pc += offset as usize;
        } else {
            frame.pc += 2;
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

        let offset = u16::from_be_bytes(code[frame.pc..frame.pc + 2].try_into().unwrap()) as usize;
        if union_value.determinant == check_determinant {
            for m in &union_value.members {
                self.value_stack.push(*m);
            }
            frame.pc += 2;
        } else {
            frame.pc += offset;
        }
    }

    fn read_input(&mut self, frame: &mut Frame) {
        let read_type = frame.next_code().unwrap();

        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();

        if read_type == 0 {
            let val = line.trim().parse::<i64>().unwrap_or(0);
            self.value_stack.push(Value::Integer(val));
        } else if read_type == 1 {
            let val = line.trim().parse::<f64>().unwrap_or(0.0);
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

    fn sort_vector(&mut self, frame_stack: &mut FrameStack, vector: &mut VectorValue, pred: Option<NonNull<FunctionValue>>) {
        if vector.is_empty() {
            return;
        }

        match vector[0] {
            Value::Integer(_) | Value::Float(_) | Value::Str(_) | Value::Bool(_) | Value::Char(_) => {
                vector.sort_by(|a, b| {
                    match (a, b) {
                        (Value::Integer(x), Value::Integer(y)) => x.cmp(y),
                        (Value::Float(x), Value::Float(y)) => x.cmp(y),
                        (Value::Str(x), Value::Str(y)) => unsafe { x.as_ref() }.val.cmp(&unsafe { y.as_ref() }.val),
                        (Value::Bool(x), Value::Bool(y)) => x.cmp(y),
                        (Value::Char(x), Value::Char(y)) => x.cmp(y),
                        _ => panic!("Unexpected values in vector during sort! {:?} / {:?}", a, b)
                    }
                });
            }
            _ => {
                let pred = match pred {
                    Some(f) => f,
                    None => panic!("Predicate not a function!")
                };

                vector.sort_by(|a, b| {
                    let start_stack_size = self.value_stack.len();

                    frame_stack.push(pred);
                    frame_stack.top.locals.push(*a);
                    frame_stack.top.locals.push(*b);
                    self.run_with_frame(frame_stack);

                    // Predicate should always return a boolean
                    assert_eq!(start_stack_size + 1, self.value_stack.len());
                    let is_less = match self.value_stack.pop().unwrap() {
                        Value::Bool(b) => b,
                        _ => panic!("Return value of predicate must be a boolean") 
                    };

                    if is_less {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    }
                });
            }
        }
    }

    fn string_index(s: &str, is_ascii: bool, idx: usize) -> usize {
        if is_ascii {
            return idx;
        }

        let real_index =
            s
            .chars()
            .enumerate()
            .nth(idx);

        match real_index {
            Some(i) => i.0,
            None => s.len()
        }
    }

    fn call_builtin(&mut self, frame_stack: &mut FrameStack) {
        let builtin_id = frame_stack.top.next_code().unwrap();
        let arg_count = frame_stack.top.next_code().unwrap() as usize;

        let obj = {
            let len = self.value_stack.len();
            self.value_stack.remove(len - arg_count - 1)
        };

        match obj {
            Value::Integer(i) => match builtin_id {
                builtin_functions::integer::ABS => {
                    self.value_stack
                        .push(Value::Integer(i.abs()));
                }
                _ => panic!("Unexpected integer builtin id {}", builtin_id),
            }
            Value::Char(c) => match builtin_id {
                builtin_functions::ch::TO_LOWERCASE => {
                    self.value_stack
                        .push(Value::Char(c.to_ascii_lowercase()));
                }
                builtin_functions::ch::TO_UPPERCASE => {
                    self.value_stack
                        .push(Value::Char(c.to_ascii_uppercase()));
                }
                builtin_functions::ch::TO_INTEGER => {
                    self.value_stack.push(Value::Integer(
                        c.to_digit(10).unwrap_or(0) as i64
                    ));
                }
                builtin_functions::ch::IS_DIGIT => {
                    self.value_stack.push(Value::Bool(
                        c.is_ascii_digit()
                    ));
                }
                _ => panic!("Unexpected char builtin id {}", builtin_id),
            }
            Value::Vector(mut v) => match builtin_id {
                builtin_functions::vector::LEN => {
                    self.value_stack
                        .push(Value::Integer(unsafe { v.as_ref() }.len() as i64));
                }
                builtin_functions::vector::PUSH => {
                    unsafe { v.as_mut() }.push(self.value_stack.pop().unwrap());
                }
                builtin_functions::vector::POP => {
                    let vector = unsafe { v.as_mut() };
                    if vector.is_empty() {
                        panic!("Vector is empty, cannot pop");
                    }

                    self.value_stack.push(vector.pop().unwrap());
                }
                builtin_functions::vector::SORT => {
                    match arg_count {
                        0 => self.sort_vector(frame_stack, unsafe { v.as_mut() }, None),
                        1 => {
                            let pred = match self.value_stack.pop().unwrap() {
                                Value::Function(f) => f,
                                _ => panic!("Sort received unexpect argument")
                            };
                            self.sort_vector(frame_stack, unsafe { v.as_mut() }, Some(pred));
                        }
                        _ => panic!("Sort received unexpected argument count")
                    }
                }
                builtin_functions::vector::CLEAR => {
                    unsafe { v.as_mut() }.clear();
                }
                builtin_functions::vector::CLONE => {
                    let src = unsafe { v.as_mut() };
                    let mut vec_val = Box::new(src.clone());
                    let ptr = unsafe { NonNull::new_unchecked(vec_val.as_mut() as *mut _) };
                    self.objects.push(GcValue::Vector(vec_val));
                    self.value_stack.push(Value::Vector(ptr));
                }
                builtin_functions::vector::REMOVE => {
                    let val = self.value_stack.pop().unwrap();
                    match val {
                        Value::Char(_) | Value::Integer(_) | Value::Float(_) | Value::Str(_) => {},
                        _ => unimplemented!()
                    }

                    unsafe { v.as_mut() }.retain(|x| *x != val);
                }
                builtin_functions::vector::CONTAINS => {
                    let val = self.value_stack.pop().unwrap();
                    match val {
                        Value::Char(_) | Value::Integer(_) | Value::Float(_) | Value::Str(_) => {},
                        _ => unimplemented!()
                    }

                    self.value_stack.push(Value::Bool(unsafe { v.as_mut() }.contains(&val)));
                }
                _ => panic!("Unexpected vector builtin id {}", builtin_id),
            },
            Value::Str(s) => match builtin_id {
                builtin_functions::string::LEN => {
                    self.value_stack
                        .push(Value::Integer(unsafe { s.as_ref() }.len as i64));
                }
                builtin_functions::string::TO_INTEGER => {
                    self.value_stack.push(Value::Integer(
                        unsafe { s.as_ref() }.val.parse::<i64>().unwrap_or(0),
                    ));
                }
                builtin_functions::string::SUBSTR
                | builtin_functions::string::SUBSTR_FROM_START
                | builtin_functions::string::SUBSTR_TO_END => {
                    let is_ascii = unsafe { s.as_ref() }.is_ascii;
                    let s = &unsafe { s.as_ref() }.val;

                    let pop_index = |value_stack: &mut Vec<Value>| match value_stack.pop().unwrap()
                    {
                        Value::Integer(i) => {
                            if i < 0 {
                                panic!("Index less than 0");
                            }
                            i as usize
                        }
                        x => panic!("Expected integer, got {:?}", x),
                    };

                    let substr = match builtin_id {
                        builtin_functions::string::SUBSTR => {
                            let iend = Self::string_index(s, is_ascii, pop_index(&mut self.value_stack));
                            let istart = Self::string_index(s, is_ascii, pop_index(&mut self.value_stack));
                            &s[istart..iend]
                        }
                        builtin_functions::string::SUBSTR_FROM_START => {
                            let iend = Self::string_index(s, is_ascii, pop_index(&mut self.value_stack));
                            &s[..iend]
                        }
                        builtin_functions::string::SUBSTR_TO_END => {
                            let istart = Self::string_index(s, is_ascii, pop_index(&mut self.value_stack));
                            if istart == s.len() {
                                ""
                            }
                            else {
                                &s[istart..]
                            }
                        }
                        x => panic!("Unexpected SUBSTR builtin id: {}", x),
                    };

                    let mut data_section = VMDataSection {
                        objects: &mut self.objects,
                        constant_strs: &mut self.constant_strs,
                    };

                    let idx = data_section.create_constant_str(substr);
                    self.value_stack
                        .push(Value::Str(self.constant_strs[idx as usize]));
                }
                builtin_functions::string::SPLIT => {
                    let s = &unsafe { s.as_ref() }.val;

                    let delimiter = match self.value_stack.pop().unwrap() {
                        Value::Char(c) => c,
                        _ => panic!("Expected char"),
                    };

                    let mut vector = vec![];
                    for substr in s.split(delimiter) {
                        let mut data_section = VMDataSection {
                            objects: &mut self.objects,
                            constant_strs: &mut self.constant_strs,
                        };

                        let idx = data_section.create_constant_str(substr);
                        vector.push(Value::Str(self.constant_strs[idx as usize]));
                    }

                    let mut vec_val = Box::new(vector);
                    let ptr = unsafe { NonNull::new_unchecked(vec_val.as_mut() as *mut _) };
                    self.objects.push(GcValue::Vector(vec_val));

                    self.value_stack.push(Value::Vector(ptr));
                }
                builtin_functions::string::TRIM
                | builtin_functions::string::TRIM_START
                | builtin_functions::string::TRIM_END => {
                    let s = &unsafe { s.as_ref() }.val;

                    let mut data_section = VMDataSection {
                        objects: &mut self.objects,
                        constant_strs: &mut self.constant_strs,
                    };

                    let idx = data_section.create_constant_str(match builtin_id {
                        builtin_functions::string::TRIM => s.trim(),
                        builtin_functions::string::TRIM_START => s.trim_start(),
                        builtin_functions::string::TRIM_END => s.trim_end(),
                        _ => unreachable!(),
                    });

                    self.value_stack
                        .push(Value::Str(self.constant_strs[idx as usize]));
                }
                builtin_functions::string::STARTS_WITH => {
                    let substr = match self.value_stack.pop().unwrap() {
                        Value::Str(s) => &unsafe { s.as_ref() }.val,
                        _ => panic!("Expected string"),
                    };
                    let s = &unsafe { s.as_ref() }.val;
                    self.value_stack.push(Value::Bool(s.starts_with(substr)));
                }
                builtin_functions::string::ENDS_WITH => {
                    let substr = match self.value_stack.pop().unwrap() {
                        Value::Str(s) => &unsafe { s.as_ref() }.val,
                        _ => panic!("Expected string"),
                    };
                    let s = &unsafe { s.as_ref() }.val;
                    self.value_stack.push(Value::Bool(s.ends_with(substr)));
                }
                builtin_functions::string::CONTAINS => {
                    let substr = match self.value_stack.pop().unwrap() {
                        Value::Str(s) => &unsafe { s.as_ref() }.val,
                        _ => panic!("Expected string"),
                    };
                    let s = &unsafe { s.as_ref() }.val;
                    self.value_stack.push(Value::Bool(s.contains(substr)));
                }
                _ => panic!("Unexpected string builtin id {}", builtin_id),
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
    fn test_escape_characters() {
        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            assert_eq!(vm.interpret("print(\"\\n\\t\");"), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "\n\t");
        }
    }

    #[test]
    fn test_not() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);
        let src = "
            let init: bool = false;
            let not: bool = !init;
            print(not);

            let not2: bool = !not;
            print(not2);
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "true");
        assert_eq!(printer.strings[1], "false");
    }

    #[test]
    fn char_builting() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);
        let src = "
            let c: char = 'a';
            print(c.to_uppercase());

            let C: char = 'A';
            print(c.to_lowercase());
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "A");
        assert_eq!(printer.strings[1], "a");
    }

    #[test]
    fn string_index() {
        {
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
        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            let src = "
                let s: string = \"hello你好こんにちは\";
                for i : 0..s.len() {
                    print(s[i]);
                }
            ";

            assert_eq!(vm.interpret(src), Ok(()));
            assert_eq!(printer.strings.len(), 12);
            assert_eq!(printer.strings[0], "h");
            assert_eq!(printer.strings[1], "e");
            assert_eq!(printer.strings[2], "l");
            assert_eq!(printer.strings[3], "l");
            assert_eq!(printer.strings[4], "o");

            assert_eq!(printer.strings[5], "你");
            assert_eq!(printer.strings[6], "好");

            assert_eq!(printer.strings[7], "こ");
            assert_eq!(printer.strings[8], "ん");
            assert_eq!(printer.strings[9], "に");
            assert_eq!(printer.strings[10], "ち");
            assert_eq!(printer.strings[11], "は");
        }
    }

    #[test]
    fn string_index_range() {
        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            let src = "
                let s: string = \"hello\";
                print(s[1..4]);
            ";

            assert_eq!(vm.interpret(src), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "ell");
        }

        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            let src = "
                let s: string = \"hello\";
                print(s[1..]);
            ";

            assert_eq!(vm.interpret(src), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "ello");
        }

        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            let src = "
                let s: string = \"hello\";
                print(s[..4]);
            ";

            assert_eq!(vm.interpret(src), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "hell");
        }
    }

    #[test]
    fn print_char() {
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

        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            assert_eq!(vm.interpret("print 70%60;"), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "10");
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
            assert_eq!(printer.strings[0], "-1.1");
        }

        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);
            assert_eq!(vm.interpret("print 1.5-2.6;"), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "-1.1");
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
    fn logical_and() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            print true && false;
            print false && false;
            print true && true;

            print 1 == 1 && 10 == 10;
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 4);
        assert_eq!(printer.strings[0], "false");
        assert_eq!(printer.strings[1], "false");
        assert_eq!(printer.strings[2], "true");
        assert_eq!(printer.strings[3], "true");
    }

    #[test]
    fn logical_or() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            print true || false;
            print false || false;
            print true || true;

            print 1 == 1 || 10 == 10;
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 4);
        assert_eq!(printer.strings[0], "true");
        assert_eq!(printer.strings[1], "false");
        assert_eq!(printer.strings[2], "true");
        assert_eq!(printer.strings[3], "true");
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

        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);

            let src = "
                let start: int = 1;
                let s: string = \"hello\";
                for i : start..s.len() {
                    print s[i];
                }
            ";
            assert_eq!(vm.interpret(src), Ok(()));
            assert_eq!(printer.strings.len(), 4);
            assert_eq!(printer.strings[0], "e");
            assert_eq!(printer.strings[1], "l");
            assert_eq!(printer.strings[2], "l");
            assert_eq!(printer.strings[3], "o");
        }
    }

    #[test]
    fn for_loop_break() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            for i : 0..5 {
                if i > 2 {
                    break;
                }

                print i;
            }
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 3);
        assert_eq!(printer.strings[0], "0");
        assert_eq!(printer.strings[1], "1");
        assert_eq!(printer.strings[2], "2");
    }

    #[test]
    fn for_loop_nested_break() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            for i : 0..5 {
                if i > 2 {
                    break;
                }

                print i;

                for j : 0..5 {
                    if j > 3 {
                        break;
                    }
                    print j;
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
    fn for_loop_continue() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            for i : 0..5 {
                if i < 2 {
                    continue;
                }

                print i;
            }
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 3);
        assert_eq!(printer.strings[0], "2");
        assert_eq!(printer.strings[1], "3");
        assert_eq!(printer.strings[2], "4");
    }

    #[test]
    fn for_loop_nested_continue() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            for i : 0..5 {
                if i < 2 {
                    continue;
                }

                print i;

                for j : 0..5 {
                    if j < 3 {
                        continue;
                    }

                    print j;
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
    fn for_loop_step() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            for i : 0..=10:2 {
                print i;
            }
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 6);
        assert_eq!(printer.strings[0], "0");
        assert_eq!(printer.strings[1], "2");
        assert_eq!(printer.strings[2], "4");
        assert_eq!(printer.strings[3], "6");
        assert_eq!(printer.strings[4], "8");
        assert_eq!(printer.strings[5], "10");
    }

    #[test]
    fn for_loop_rev() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            for i : 5..=>0 {
                print i;
            }
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 6);
        assert_eq!(printer.strings[0], "5");
        assert_eq!(printer.strings[1], "4");
        assert_eq!(printer.strings[2], "3");
        assert_eq!(printer.strings[3], "2");
        assert_eq!(printer.strings[4], "1");
        assert_eq!(printer.strings[5], "0");
    }

    #[test]
    fn for_loop_step_rev() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            for i : 10..=>0:-2 {
                print i;
            }
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 6);
        assert_eq!(printer.strings[0], "10");
        assert_eq!(printer.strings[1], "8");
        assert_eq!(printer.strings[2], "6");
        assert_eq!(printer.strings[3], "4");
        assert_eq!(printer.strings[4], "2");
        assert_eq!(printer.strings[5], "0");
    }

    #[test]
    fn for_loop_container() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let v = vec<int>{10, 20, 30};
            for i : v {
                print i;
            }
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 3);
        assert_eq!(printer.strings[0], "10");
        assert_eq!(printer.strings[1], "20");
        assert_eq!(printer.strings[2], "30");
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
        {
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
        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);

            let src = "
                let v: [int] = vec<int>[5]{11};
                print v;
            ";
            assert_eq!(vm.interpret(src), Ok(()));
            assert_eq!(printer.strings.len(), 1);
            assert_eq!(printer.strings[0], "vec{11,11,11,11,11}");
        }
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
    fn test_vector_push() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let mut v: [int] = vec<int>{1,2,3,4,5};
            print(v.len());

            v.push(60);
            print(v.len());
            v.push(70);
            print(v.len());
            print(v[5]);
            print(v[6]);
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 5);
        assert_eq!(printer.strings[0], "5");
        assert_eq!(printer.strings[1], "6");
        assert_eq!(printer.strings[2], "7");
        assert_eq!(printer.strings[3], "60");
        assert_eq!(printer.strings[4], "70");
    }

    #[test]
    fn test_vector_pop() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let mut v: [int] = vec<int>{10,20,30,40,50};
            print(v.len());
            print(v.pop());
            print(v.len());
            print(v.pop());

            for i : 0..v.len() {
                print(v[i]);
            }
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 7);
        assert_eq!(printer.strings[0], "5");
        assert_eq!(printer.strings[1], "50");
        assert_eq!(printer.strings[2], "4");
        assert_eq!(printer.strings[3], "40");
        assert_eq!(printer.strings[4], "10");
        assert_eq!(printer.strings[5], "20");
        assert_eq!(printer.strings[6], "30");
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
    fn vector_clear() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let mut v: [int] = vec<int>{10,20,30,40};
            print(v.len());
            v.clear();
            print(v.len());
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "4");
        assert_eq!(printer.strings[1], "0");
    }

    #[test]
    fn vector_clone() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let mut v: [int] = vec<int>{10,20,30,40};
            let v2: [int] = v.clone();
            v.clear();
            print(v.len());
            for i : 0..v2.len() {
                print(v2[i]);
            }
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 5);
        assert_eq!(printer.strings[0], "0");
        assert_eq!(printer.strings[1], "10");
        assert_eq!(printer.strings[2], "20");
        assert_eq!(printer.strings[3], "30");
        assert_eq!(printer.strings[4], "40");
    }

    #[test]
    fn vector_remove() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let mut v: [int] = vec<int>{10,20,30,10,40};
            v.remove(10);
            print(v.len());

            for i : 0..v.len() {
                print(v[i]);
            }

            v.remove(1000);
            print(v.len());
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 5);
        assert_eq!(printer.strings[0], "3");
        assert_eq!(printer.strings[1], "20");
        assert_eq!(printer.strings[2], "30");
        assert_eq!(printer.strings[3], "40");
        assert_eq!(printer.strings[4], "3");
    }

    #[test]
    fn vector_contains() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let mut v: [int] = vec<int>{10,20,30};
            print(v.contains(10));
            print(v.contains(20));
            print(v.contains(11));
            print(v.contains(40));
            print(v.contains(30));
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 5);
        assert_eq!(printer.strings[0], "true");
        assert_eq!(printer.strings[1], "true");
        assert_eq!(printer.strings[2], "false");
        assert_eq!(printer.strings[3], "false");
        assert_eq!(printer.strings[4], "true");
    }

    #[test]
    fn vector_sort() {
        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);

            let src = "
                let mut v: [int] = vec<int>{20, 10, 40, 30};
                v.sort();

                for i : 0..v.len() {
                    print(v[i]);
                }
            ";
            assert_eq!(vm.interpret(src), Ok(()));
            assert_eq!(printer.strings.len(), 4);
            assert_eq!(printer.strings[0], "10");
            assert_eq!(printer.strings[1], "20");
            assert_eq!(printer.strings[2], "30");
            assert_eq!(printer.strings[3], "40");
        }
        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);

            let src = "
                let mut v: [float] = vec<float>{3.4, 1.2, 8.7, 11.9};
                v.sort();

                for i : 0..v.len() {
                    print(v[i]);
                }
            ";
            assert_eq!(vm.interpret(src), Ok(()));
            assert_eq!(printer.strings.len(), 4);
            assert_eq!(printer.strings[0], "1.2");
            assert_eq!(printer.strings[1], "3.4");
            assert_eq!(printer.strings[2], "8.7");
            assert_eq!(printer.strings[3], "11.9");
        }
        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);

            let src = "
                let mut v: [bool] = vec<bool>{true, false, false, true};
                v.sort();

                for i : 0..v.len() {
                    print(v[i]);
                }
            ";
            assert_eq!(vm.interpret(src), Ok(()));
            assert_eq!(printer.strings.len(), 4);
            assert_eq!(printer.strings[0], "false");
            assert_eq!(printer.strings[1], "false");
            assert_eq!(printer.strings[2], "true");
            assert_eq!(printer.strings[3], "true");
        }
        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);

            let src = "
                let mut v: [string] = vec<string>{\"hello\", \"world\", \"kyx\", \"zebra\"};
                v.sort();

                for i : 0..v.len() {
                    print(v[i]);
                }
            ";
            assert_eq!(vm.interpret(src), Ok(()));
            assert_eq!(printer.strings.len(), 4);
            assert_eq!(printer.strings[0], "hello");
            assert_eq!(printer.strings[1], "kyx");
            assert_eq!(printer.strings[2], "world");
            assert_eq!(printer.strings[3], "zebra");
        }

        {
            let mut printer = TestPrinter::new();
            let mut vm = VM::new(&mut printer);

            let src = "
                struct S {
                    i: int,
                    s: string,
                }

                fn pred(a: S, b: S) -> bool {
                    return a.i < b.i;
                }

                let mut v: [S] = vec<S>{};
                v.push(S {
                    i = 10,
                    s = \"something\",
                });
                v.push(S {
                    i = 5,
                    s = \"something else\",
                });
                v.push(S {
                    i = 7,
                    s = \"seven eleven\",
                });
                v.push(S {
                    i = 11,
                    s = \"eleven seven\",
                });
                v.sort(pred);
                for i : 0..v.len() {
                    print(v[i].i);
                    print(v[i].s);
                }
                ";

            assert_eq!(vm.interpret(src), Ok(()));
            assert_eq!(printer.strings.len(), 8);
            assert_eq!(printer.strings[0], "5");
            assert_eq!(printer.strings[1], "something else");
            assert_eq!(printer.strings[2], "7");
            assert_eq!(printer.strings[3], "seven eleven");
            assert_eq!(printer.strings[4], "10");
            assert_eq!(printer.strings[5], "something");
            assert_eq!(printer.strings[6], "11");
            assert_eq!(printer.strings[7], "eleven seven");
        }
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
                    i: mut int,
                    f: mut float,
                    s: mut string,
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
                    inner: mut string,
                }

                struct Struct
                {
                    nested: mut Nested,
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
                    i: mut int,
                    f: mut float,
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

                let mut u: Union = Union.I(9);
                print(u);

                u = Union.S(\"Hello world\");
                print(u);

                u = Union.F(3.142);
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

                let mut o: Union = Union.I(10);
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

                o = Union.F(3.142);
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
    fn test_if_let_loop() {
        // Test that the value & local stacks are correct using loops.
        // The if let block must correctly pop the unwraped value off the locals stack.
        // If there is a misalignment due to the local stack continually grown (from a missing pop)
        // then 'v' will not be a union and the vm will crash
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
                union Union
                {
                    Some(char),
                    None,
                }

                let str = \"a1bc2def3ghi45jkl9mn4\";

                fn test() {
                    for i : 0..str.len() {
                        let s = str;
                        let v = Union.Some(s[i]);
                        if let Union.Some(c,) = v {
                        }
                    }
                }
                test();
                ";
        assert_eq!(vm.interpret(src), Ok(()));
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

                let mut c: Composite = Composite.Two(10,1.23);
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

                c = Composite.Three(300, 3.142, \"Hello World\");
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
            let o: O = O.Some(tuple(9, 1.23,));

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
    fn test_string_to_string() {
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
    fn test_string_split() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let s: string = \"hello, world pipe-a|pipe-b|pipe-c\";
            let v: [string] = s.split(',');
            print(v[0]);
            print(v[1]);

            let v2: [string] = v[1].split('|');
            print(v2[0]);
            print(v2[1]);
            print(v2[2]);
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 5);
        assert_eq!(printer.strings[0], "hello");
        assert_eq!(printer.strings[1], " world pipe-a|pipe-b|pipe-c");
        assert_eq!(printer.strings[2], " world pipe-a");
        assert_eq!(printer.strings[3], "pipe-b");
        assert_eq!(printer.strings[4], "pipe-c");
    }

    #[test]
    fn test_string_starts_ends_with() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let s: string = \"hello, world\";
            print(s.starts_with(\"hello\"));
            print(s.ends_with(\"hello\"));
            print(s.starts_with(\"world\"));
            print(s.ends_with(\"world\"));
            print(s.contains(\"hello\"));
            print(s.contains(\"world\"));
            print(s.contains(\"contains\"));
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 7);
        assert_eq!(printer.strings[0], "true");
        assert_eq!(printer.strings[1], "false");
        assert_eq!(printer.strings[2], "false");
        assert_eq!(printer.strings[3], "true");
        assert_eq!(printer.strings[4], "true");
        assert_eq!(printer.strings[5], "true");
        assert_eq!(printer.strings[6], "false");
    }

    #[test]
    fn test_string_trim() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let s: string = \"  hello, world  \";
            print(s.trim());
            print(s.trim_start());
            print(s.trim_end());
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 3);
        assert_eq!(printer.strings[0], "hello, world");
        assert_eq!(printer.strings[1], "hello, world  ");
        assert_eq!(printer.strings[2], "  hello, world");
    }

    #[test]
    fn test_integer_builtin() {
        let mut printer = TestPrinter::new();
        let mut vm = VM::new(&mut printer);

        let src = "
            let a: int = -11;
            print(-10.abs());
            print(10.abs());
            print(a.abs());
        ";
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 3);
        assert_eq!(printer.strings[0], "10");
        assert_eq!(printer.strings[1], "10");
        assert_eq!(printer.strings[2], "11");
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
