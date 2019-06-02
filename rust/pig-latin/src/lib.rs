pub fn translate(input: &str) -> String {
    input
        .split(' ')
        .map(anslatetray)
        .collect::<Vec<String>>()
        .join(" ")
}

fn anslatetray(word: &str) -> String {
    let mut i = if word.starts_with('y') {
        word.find(|c| "aeiou".contains(c)).unwrap()
    } else {
        word.find(|c| "aeiouy".contains(c)).unwrap()
    };
    if i >= 1 && &word[i - 1..=i] == "qu" {
        i += 1
    }
    if &word[..2] == "yt" || "ay" == &word[i..] {
        i = 0
    }
    format!("{}{}ay", &word[i..], &word[..i])
}
