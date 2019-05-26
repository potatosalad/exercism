pub fn brackets_are_balanced(string: &str) -> bool {
    let mut stack = Vec::new();
    for c in string.chars() {
        match c {
            '[' | '{' | '(' => stack.push(c),
            ']' | '}' | ')' => match stack.pop() {
                Some(o) if o == inverse_of_bracket(c) => continue,
                _ => return false,
            },
            _ => continue,
        }
    }
    stack.is_empty()
}

fn inverse_of_bracket(c: char) -> char {
    match c {
        '[' => ']',
        ']' => '[',
        '{' => '}',
        '}' => '{',
        '(' => ')',
        ')' => '(',
        _ => unreachable!(),
    }
}
