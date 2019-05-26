pub fn is_armstrong_number(num: u32) -> bool {
    let exp: u32 = if num > 0 {
        f32::log10(num as f32) as u32 + 1
    } else {
        1
    };
    num == num
        .to_string()
        .chars()
        .map(|c| (c as u32 - '0' as u32).pow(exp))
        .sum()
}
