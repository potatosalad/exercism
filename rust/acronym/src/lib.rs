pub fn abbreviate(phrase: &str) -> String {
    phrase
        .split(|c: char| c.is_whitespace() || c == '-')
        .filter(|s| !s.is_empty())
        .flat_map(capitals)
        .collect()
}

fn capitals(s: &str) -> Vec<char> {
    let mut out = Vec::new();
    let mut flag: bool = false;
    for c in s.chars() {
        if out.is_empty() {
            for c in c.to_uppercase() {
                out.push(c);
            }
        } else if !flag && c.is_lowercase() {
            flag = true;
        } else if flag && c.is_uppercase() {
            out.push(c);
            flag = false;
        } else {
            continue;
        }
    }
    out
}
