pub fn primes_up_to(upper_bound: u64) -> Vec<u64> {
    let limit = upper_bound as usize;
    let mut candidates = vec![true; limit + 1];
    candidates[0] = false;
    if upper_bound > 0 {
        candidates[1] = false;
    }
    for n in 2..=limit {
        if candidates[n] {
            let mut c = n * n;
            while c <= limit {
                candidates[c] = false;
                c += n;
            }
        }
    }
    candidates
        .iter()
        .enumerate()
        .filter_map(|(prime, &valid)| if valid { Some(prime as u64) } else { None })
        .collect()
}
