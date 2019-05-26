pub fn series(digits: &str, len: usize) -> Vec<String> {
    digits
        .as_bytes()
        .windows(len)
        .map(|w| w.iter().map(|&b| b as char).collect())
        .collect()
}
