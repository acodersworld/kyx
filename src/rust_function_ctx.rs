#[derive(Debug, Clone)]
pub enum RustValue {
    Float(f64),
    Integer(i64),
    Str(String),
    Bool(bool),
    StringVector(Vec<String>),
}

pub trait RustFunctionCtx {
    fn get_parameter(&self, idx: usize) -> Option<RustValue>;

    fn get_parameter_float(&self, idx: usize) -> Option<f64>;
    fn get_parameter_integer(&self, idx: usize) -> Option<i64>;
    fn get_parameter_bool(&self, idx: usize) -> Option<bool>;
    fn get_parameter_string(&self, idx: usize) -> Option<String>;

    fn set_result(&mut self, value: RustValue);

    fn print(&mut self, s: &str);
}
