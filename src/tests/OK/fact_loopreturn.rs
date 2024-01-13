fn fact(n : u32) -> u32 {
    let mut i = 2u32;
    let mut f = 1u32;
    loop {
        if i > n { return f };
        f = f * i;
        i = i + 1;
    };
    0xdeadC0de_u32
}

fn main() {
    let _ = fact(10);
}
