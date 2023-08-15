use std::vec::Vec;

pub struct Chunk {
    pub code: Vec<u8>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk { code: Vec::new() }
    }

    pub fn write_byte(&mut self, byte: u8) -> usize {
        let idx = self.code.len();
        self.code.push(byte);
        idx
    }
}
