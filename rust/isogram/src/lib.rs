use std::collections::HashSet;

pub fn check(candidate: &str) -> bool {
    let mut seen = HashSet::new();
    for c in candidate.chars().flat_map(|c| c.to_lowercase()) {
        if c.is_alphabetic() {
            if seen.contains(&c) {
                return false;
            } else {
                seen.insert(c);
            }
        }
    }
    true
}
