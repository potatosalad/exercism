pub fn private_key(p: u64) -> u64 {
    if p < 3 {
        panic!("p must be greater than or equal to 3");
    }
    let mut x = modulo(random(), p);
    while !(2..p).contains(&x) {
        x = modulo(random(), p);
    }
    x
}

pub fn public_key(p: u64, g: u64, a: u64) -> u64 {
    modpow(g, a, p)
}

pub fn secret(p: u64, b_pub: u64, a: u64) -> u64 {
    modpow(b_pub, a, p)
}

fn modpow(b: u64, e: u64, m: u64) -> u64 {
    if m == 1 {
        0
    } else {
        let mut r: u64 = 1;
        let mut b = b % m;
        let mut e = e;
        while e > 0 {
            if e % 2 == 1 {
                r = (r * b) % m;
            }
            e >>= 1;
            b = (b * b) % m;
        }
        r
    }
}

fn modulo(b: u64, m: u64) -> u64 {
    (b % m + m) % m
}

fn random() -> u64 {
    let mut x: u64 = 0;
    unsafe {
        while core::arch::x86_64::_rdrand64_step(&mut x) == 0 || x == 0 {
            core::arch::x86_64::_mm_pause();
        }
    }
    x
}
