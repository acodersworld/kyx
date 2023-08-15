pub struct Encoder {
    val: i32,
    shift: u32,
}

impl Encoder {
    pub fn new(val: i32) -> Encoder {
        let mut shift = 0;
        if val != 0 {
            shift = 4 * 7;
            while (val & (0x7f << shift)) == 0 {
                shift -= 7;
            }
        }

        Encoder { val, shift }
    }

    pub fn step_encode(&mut self) -> (u8, bool) {
        let mut byte = ((self.val >> self.shift) & 0x7f) as u8;

        let mut complete = false;
        if self.shift != 0 {
            self.shift -= 7;
            byte |= 0x80;
        } else {
            complete = true;
        }

        (byte, complete)
    }
}

pub struct Decoder {
    val: i32,
}

impl Decoder {
    pub fn new() -> Decoder {
        Decoder { val: 0 }
    }

    pub fn step_decode(&mut self, byte: u8) -> bool {
        let is_last = (byte & 0x80) == 0;

        self.val <<= 7;
        self.val |= (byte & 0x7f) as i32;

        is_last
    }

    pub fn val(&self) -> i32 {
        self.val
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn encode(val: i32) -> std::vec::Vec<u8> {
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

    fn decode(bytes: std::vec::Vec<u8>) -> i32 {
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
    fn test_n1() {
        assert_eq!(-1, decode(encode(-1)));
    }

    #[test]
    fn test_127() {
        assert_eq!(127, decode(encode(127)));
    }

    #[test]
    fn test_n127() {
        assert_eq!(-127, decode(encode(-127)));
    }

    #[test]
    fn test_128() {
        assert_eq!(128, decode(encode(128)));
    }

    #[test]
    fn test_n128() {
        assert_eq!(-128, decode(encode(-128)));
    }

    #[test]
    fn test_300() {
        assert_eq!(300, decode(encode(300)));
    }

    #[test]
    fn test_n300() {
        assert_eq!(-300, decode(encode(-300)));
    }

    #[test]
    fn test_ffff() {
        assert_eq!(0xffff, decode(encode(0xffff)));
    }

    #[test]
    fn test_nffff() {
        assert_eq!(-0xffff, decode(encode(-0xffff)));
    }

    #[test]
    fn test_7fffffff() {
        assert_eq!(0x7fffffff, decode(encode(0x7fffffff)));
    }

    #[test]
    fn test_n7fffffff() {
        assert_eq!(-0x7fffffff, decode(encode(-0x7fffffff)));
    }
}
