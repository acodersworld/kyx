#[repr(C)]
union Float {
    f: f64,
    a: [u8; 8],
}

pub fn encode(value: f64) -> [u8; 8] {
    let flt = Float { f: value };
    unsafe { flt.a }
}

pub fn decode(bytes: &[u8; 8]) -> f64 {
    let flt = Float { a: *bytes };
    unsafe { flt.f }
}
