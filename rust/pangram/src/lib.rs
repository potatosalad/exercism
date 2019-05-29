#[macro_use]
extern crate lazy_static;

use std::collections::HashSet;

pub fn is_pangram(sentence: &str) -> bool {
    let chars: HashSet<char> = sentence
        .chars()
        .filter(|c| c.is_alphabetic())
        .flat_map(|c| c.to_lowercase())
        .collect();
    chars.eq(&ASCII_ALPHABET) || chars.eq(&GERMAN_ALPHABET) || chars.eq(&SPANISH_ALPHABET)
}

lazy_static! {
    static ref ASCII_ALPHABET: HashSet<char> = (b'a'..=b'z').map(char::from).collect();
    static ref GERMAN_ALPHABET: HashSet<char> = {
        let mut alphabet = ASCII_ALPHABET.clone();
        alphabet.extend(&['ä', 'ö', 'ü', 'ß']);
        alphabet
    };
    static ref SPANISH_ALPHABET: HashSet<char> = {
        let mut alphabet = ASCII_ALPHABET.clone();
        alphabet.extend(&['ñ']);
        alphabet
    };
}
