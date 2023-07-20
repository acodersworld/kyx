
#[repr(C)]
union Float {
    f: f32,
    a: [u8; 4]
}

pub fn encode(value: f32) -> [u8; 4] {
    let flt = Float { f: value };
    unsafe { flt.a }
}

pub fn decode(bytes: &[u8; 4]) -> f32 {
    let flt = Float { a: *bytes };
    unsafe { flt.f }
}

