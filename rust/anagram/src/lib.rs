use std::collections::HashSet;

pub fn anagrams_for<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    let mut out: HashSet<&'a str> = HashSet::new();
    let word_vec = word_to_sorted_vec(word);
    for possible_anagram in possible_anagrams {
        let possible_anagram_vec = word_to_sorted_vec(possible_anagram);
        if word_vec == possible_anagram_vec && !is_same_word(word, possible_anagram) {
            out.insert(possible_anagram);
        }
    }
    out
}

fn is_same_word(a: &str, b: &str) -> bool {
    a.to_lowercase() == b.to_lowercase()
}

fn word_to_sorted_vec(word: &str) -> Vec<char> {
    let mut vec: Vec<char> = word.to_lowercase().chars().collect();
    vec.sort();
    vec
}
