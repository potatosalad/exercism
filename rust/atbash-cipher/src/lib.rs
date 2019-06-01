extern crate itertools;
use itertools::Itertools;

pub fn encode(plain: &str) -> String {
    atbash_crypt(plain)
        .chunks(5)
        .into_iter()
        .map(|chunk| chunk.collect::<String>())
        .join(" ")
}

pub fn decode(cipher: &str) -> String {
    atbash_crypt(cipher).collect()
}

fn atbash_crypt<'a>(
    input: &'a str,
) -> std::iter::FilterMap<std::str::Chars<'a>, fn(char) -> Option<char>> {
    input.chars().filter_map(|c| match c {
        '0'...'9' => Some(c),
        'A'...'Z' => Some(atbash_rotate(c, b'A')),
        'a'...'z' => Some(atbash_rotate(c, b'a')),
        _ => None,
    })
}

fn atbash_rotate(c: char, base: u8) -> char {
    (b'a' + ((25 - (c as u8 - base)) % 26)) as char
}
