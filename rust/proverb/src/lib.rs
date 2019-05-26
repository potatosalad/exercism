pub fn build_proverb(list: &[&str]) -> String {
    if list.is_empty() {
        return String::new();
    }
    let mut text: Vec<String> = Vec::new();
    let mut iter = list.into_iter();
    let mut prev = iter.next().unwrap();
    let term = prev;
    while let Some(next) = iter.next() {
        text.push(format!("For want of a {} the {} was lost.", prev, next));
        prev = next;
    }
    text.push(format!("And all for the want of a {}.", term));
    text.join("\n")
}
