pub struct Encoder {
    val: i64,
    shift: u64,
}

impl Encoder {
    pub fn new(val: i64) -> Encoder {
        let mut shift = 0;
        if val != 0 {
            const NUM_BITS: u64 = (std::mem::size_of::<i64>() * 8) as u64;
            shift = ((NUM_BITS / 7) * 7) as u64;
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
    val: i64,
}

impl Decoder {
    pub fn new() -> Decoder {
        Decoder { val: 0 }
    }

    pub fn step_decode(&mut self, byte: u8) -> bool {
        let is_last = (byte & 0x80) == 0;

        self.val <<= 7;
        self.val |= (byte & 0x7f) as i64;

        is_last
    }

    pub fn val(&self) -> i64 {
        self.val
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn encode(val: i64) -> std::vec::Vec<u8> {
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

    fn decode(bytes: std::vec::Vec<u8>) -> i64 {
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

    #[test]
    fn test_max() {
        assert_eq!(i64::max_value(), decode(encode(i64::max_value())));
    }

    #[test]
    fn test_min() {
        assert_eq!(i64::min_value(), decode(encode(i64::min_value())));
    }
}
