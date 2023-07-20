use std::vec::Vec;

pub struct Chunk {
    pub code: Vec<u8>
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new()
        }
    }

    pub fn write(self: &mut Self, byte: u8) {
        self.code.push(byte);
    }
}
