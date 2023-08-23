use ordered_float::OrderedFloat;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::ptr::NonNull;
use std::vec::Vec;
use std::fmt;

use crate::chunk::Chunk;

#[derive(Debug)]
pub struct StringValue {
    pub val: String,
    pub hash: usize,
}

pub struct FunctionValue {
    pub chunk: Chunk 
}

impl fmt::Debug for FunctionValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ptr = self.chunk.code.as_ptr();
        f.debug_struct("FunctionValue")
         .field("chunk", &ptr)
         .finish()
    }
}

pub type VectorValue = Vec<Value>;
pub type HashMapValue = HashMap<Value, Value>;

#[derive(Debug)]
pub enum GcValue {
    //    Float(f32),
    //    Integer(i32),
    Str(Box<StringValue>),
    Vector(Box<VectorValue>),
    HashMap(Box<HashMapValue>),
    Function(Box<FunctionValue>),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub enum Value {
    Float(OrderedFloat<f32>),
    Integer(i32),
    Str(NonNull<StringValue>),
    Bool(bool),
    Vector(NonNull<VectorValue>),
    HashMap(NonNull<HashMapValue>),
    Function(NonNull<FunctionValue>),
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Float(f) => f.hash(state),
            Value::Integer(i) => i.hash(state),
            Value::Str(s) => s.hash(state),
            Value::Bool(b) => b.hash(state),
            Value::Vector(v) => v.hash(state),
            Value::HashMap(h) => h.hash(state),
            Value::Function(f) => f.hash(state),
        }
    }
}
