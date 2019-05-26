pub fn collatz(n: u64) -> Option<u64> {
    match std::iter::successors(Some(n), |n| match n {
        0 | 1 => None,
        _ if n % 2 == 0 => Some(n / 2),
        _ => Some(3 * n + 1),
    })
    .count() as u64
    {
        0 | 1 => None,
        c => Some(c - 1),
    }
}
