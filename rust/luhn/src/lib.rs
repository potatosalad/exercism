/// Check a Luhn checksum.
pub fn is_valid(code: &str) -> bool {
    let digits = code.chars().filter(|c| c.is_digit(10));
    if !code.chars().all(|c| c.is_digit(10) || c.is_whitespace()) || digits.clone().count() <= 1 {
        false
    } else {
        let sum: u32 = digits
            .rev()
            .map(|c| c.to_digit(10).unwrap())
            .enumerate()
            .map(|(i, x)| {
                if i % 2 == 0 {
                    x
                } else {
                    match x * 2 {
                        y if y > 9 => y - 9,
                        y => y,
                    }
                }
            })
            .sum();
        sum % 10 == 0
    }
}
