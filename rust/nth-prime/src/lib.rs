pub fn nth(n: u32) -> u32 {
    let mut x = 1;
    let mut primes = std::iter::from_fn(move || {
        while !is_prime(x + 1) {
            x += 1;
        }
        x += 1;
        Some(x)
    });
    primes.nth(n as usize).unwrap()
}

fn is_prime(x: u32) -> bool {
    for i in 2..=((x as f32).sqrt().floor() as u32) {
        if x % i == 0 {
            return false;
        }
    }
    true
}
