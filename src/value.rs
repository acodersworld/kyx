use std::ptr::NonNull;

#[derive(Debug)]
pub struct StringValue {
    pub val: String,
    pub hash: usize
}

#[derive(Debug)]
pub enum ManagedValue {
//    Float(f32),
//    Integer(i32),
    Str(Box<StringValue>)
}

#[derive(Debug)]
pub enum Value {
    Float(f32),
    Integer(i32),
    Str(NonNull<StringValue>)
}

pub trait FromValue {
    type ValueType;

    fn from_value(value: &Value) -> Option<Self::ValueType>;
}

impl FromValue for f32 {
    type ValueType = f32;
    fn from_value(value: &Value) -> Option<f32> {
        match value {
            Value::Float(f) => Some(*f),
            _ => None
        }
    }
}

impl FromValue for i32 {
    type ValueType = i32;
    fn from_value(value: &Value) -> Option<i32> {
        match value {
            Value::Integer(i) => Some(*i),
            _ => None
        }
    }
}

impl FromValue for StringValue {
    type ValueType = NonNull<StringValue>;
    fn from_value(value: &Value) -> Option<NonNull<StringValue>> {
        match value {
            Value::Str(s) => Some(*s),
            _ => None
        }
    }
}
