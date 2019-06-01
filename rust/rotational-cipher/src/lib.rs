pub fn rotate(input: &str, key: i8) -> String {
    let key = (key % 26) + if key < 0 { 26 } else { 0 };
    let key_iter = (0_i8..=26).map(|i| ((i + key) % 26) as u8);
    let lowercase_key: Vec<char> = key_iter.clone().map(|i| char::from(i + b'a')).collect();
    let uppercase_key: Vec<char> = key_iter.clone().map(|i| char::from(i + b'A')).collect();
    input
        .chars()
        .map(|c| {
            if c.is_ascii_alphabetic() && c.is_ascii_lowercase() {
                lowercase_key[c as usize - b'a' as usize]
            } else if c.is_ascii_alphabetic() && c.is_ascii_uppercase() {
                uppercase_key[c as usize - b'A' as usize]
            } else {
                c
            }
        })
        .collect()
}
