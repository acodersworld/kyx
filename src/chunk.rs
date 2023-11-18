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

    pub fn write_short(&mut self, short: u16) -> usize {
        let idx = self.code.len();
        self.code.extend_from_slice(&short.to_be_bytes());
        idx
    }

    pub fn write_short_at(&mut self, idx: usize, short: u16) {
        let bytes = short.to_be_bytes();
        self.code[idx] = bytes[0];
        self.code[idx + 1] = bytes[1];
    }
}
