use std::collections::HashMap;

pub fn word_count(words: &str) -> HashMap<String, u32> {
    let mut counts: HashMap<String, u32> = HashMap::new();
    for word in words
        .split(|c: char| c.is_whitespace() || c == ',' || c == ';')
        .map(|s| s.trim_matches(|c: char| !c.is_alphanumeric()))
        .filter(|s| !s.is_empty())
        .map(|s| s.to_lowercase().to_string())
    {
        counts.entry(word).and_modify(|v| *v += 1).or_insert(1);
    }
    counts
}
