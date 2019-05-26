pub fn reply(message: &str) -> &str {
    let mut alphabetics = message.chars().filter(|c| c.is_alphabetic()).peekable();
    let is_shouting = alphabetics.peek().is_some() && alphabetics.all(|c| c.is_uppercase());
    if message.trim().ends_with('?') {
        if is_shouting {
            "Calm down, I know what I'm doing!"
        } else {
            "Sure."
        }
    } else if is_shouting {
        "Whoa, chill out!"
    } else if message.trim().is_empty() {
        "Fine. Be that way!"
    } else {
        "Whatever."
    }
}
