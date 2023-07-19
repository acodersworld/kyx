
pub struct Encoder {
    val: u32,
    shift: u32
}

impl Encoder {
    pub fn new(val: u32) -> Encoder {
        let mut shift = 0;
        if val > 0 {
            shift = 4 * 7;
            while (val & (0x7f << shift)) == 0 {
                shift -= 7;
            }
        }

        Encoder { 
            val,
            shift
        }
    }

    pub fn step_encode(self: &mut Self) -> (u8, bool) {
        let mut byte = ((self.val >> self.shift) & 0x7f) as u8;

        let mut complete = false;
        if self.shift != 0 {
            self.shift -= 7;
            byte |= 0x80;
        }
        else {
            complete = true;
        }

        (byte, complete)
    }
}

pub struct Decoder {
    val: u32
}

impl Decoder {
    pub fn new() -> Decoder {
        Decoder{ val: 0 }
    }

    pub fn step_decode(self: &mut Self, byte: u8) -> bool {
        let is_last = (byte & 0x80) == 0;

        self.val <<= 7;
        self.val |= (byte & 0x7f) as u32;

        is_last
    }

    pub fn val(self: &Self) -> u32 {
        self.val
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn encode(val: u32) -> std::vec::Vec<u8> {
        let mut bytes = std::vec::Vec::new();
        let mut encoder = Encoder::new(val);
        loop {
            let (byte, complete) = encoder.step_encode();
            bytes.push(byte);
            if complete {
                break;
            }
        }

        bytes
    }

    fn decode(bytes: std::vec::Vec<u8>) -> u32 {
        let mut found_last = false;
        let mut decoder = Decoder::new();
        for byte in bytes {
            found_last = decoder.step_decode(byte);
        }

        assert!(found_last);
        decoder.val()
    }

    #[test]
    fn test_0() {
        assert_eq!(0, decode(encode(0)));
    }

    #[test]
    fn test_1() {
        assert_eq!(1, decode(encode(1)));
    }

    #[test]
    fn test_127() {
        assert_eq!(127, decode(encode(127)));
    }

    #[test]
    fn test_128() {
        assert_eq!(128, decode(encode(128)));
    }

    #[test]
    fn test_300() {
        assert_eq!(300, decode(encode(300)));
    }

    #[test]
    fn test_ffff() {
        assert_eq!(0xffff, decode(encode(0xffff)));
    }

    #[test]
    fn test_ffffffff() {
        assert_eq!(0xffffffff, decode(encode(0xffffffff)));
    }
}
