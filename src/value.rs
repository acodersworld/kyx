use ordered_float::OrderedFloat;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ptr::NonNull;
use std::vec::Vec;

use crate::chunk::Chunk;
use crate::rust_function_ctx::RustFunctionCtx;

#[derive(Debug)]
pub struct StringValue {
    pub val: String,
    pub hash: usize,
}

pub struct FunctionValue {
    pub chunk: Chunk,
}

impl fmt::Debug for FunctionValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ptr = self.chunk.code.as_ptr();
        f.debug_struct("FunctionValue")
            .field("chunk", &ptr)
            .finish()
    }
}

pub struct RustFunctionValue {
    pub func: &'static dyn Fn(&mut dyn RustFunctionCtx),
}

impl RustFunctionValue {
    pub fn call(&self, ctx: &mut dyn RustFunctionCtx) {
        (self.func)(ctx);
    }
}

impl fmt::Debug for RustFunctionValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RustFunctionValue").finish()
    }
}

pub type VectorValue = Vec<Value>;
pub type HashMapValue = HashMap<Value, Value>;

#[derive(Debug)]
pub struct StructValue {
    pub members: Vec<Value>,
}

#[derive(Debug)]
pub struct UnionValue {
    pub determinant: usize,
    pub members: Vec<Value>,
}

#[derive(Debug)]
pub enum GcValue {
    //    Float(f32),
    //    Integer(i32),
    Str(Box<StringValue>),
    Vector(Box<VectorValue>),
    HashMap(Box<HashMapValue>),
    Function(Box<FunctionValue>),
    RustFunction(Box<RustFunctionValue>),
    Struct(Box<StructValue>),
    Union(Box<UnionValue>),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub enum Value {
    Float(OrderedFloat<f32>),
    Integer(i32),
    Str(NonNull<StringValue>),
    Bool(bool),
    Char(char),
    Vector(NonNull<VectorValue>),
    HashMap(NonNull<HashMapValue>),
    Function(NonNull<FunctionValue>),
    Struct(NonNull<StructValue>),
    RustFunction(NonNull<RustFunctionValue>),
    Union(NonNull<UnionValue>),
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Float(f) => f.hash(state),
            Value::Integer(i) => i.hash(state),
            Value::Str(s) => s.hash(state),
            Value::Bool(b) => b.hash(state),
            Value::Char(c) => c.hash(state),
            Value::Vector(v) => v.hash(state),
            Value::HashMap(h) => h.hash(state),
            Value::Function(f) => f.hash(state),
            Value::Struct(f) => f.hash(state),
            Value::RustFunction(f) => f.hash(state),
            Value::Union(f) => f.hash(state),
        }
    }
}
