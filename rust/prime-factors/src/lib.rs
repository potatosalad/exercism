pub fn factors(n: u64) -> Vec<u64> {
    if n < 1 {
        panic!("n must be a positive integer");
    }
    let mut factors: Vec<u64> = Vec::new();
    let mut x = n;
    loop {
        if x == 1 {
            break;
        }
        let m = find_smallest_factor(x);
        if x == m {
            factors.push(m);
            break;
        }
        while x % m == 0 {
            factors.push(m);
            x /= m;
        }
    }
    factors
}

fn find_smallest_factor(x: u64) -> u64 {
    if x % 2 == 0 {
        return 2;
    };
    for n in (1..).map(|m| 2 * m + 1).take_while(|m| m * m <= x) {
        if x % n == 0 {
            return n;
        };
    }
    x
}
