pub fn verse(n: i32) -> String {
    if n < 0 {
        panic!("n must be >= 0");
    }
    let prev1 = match n {
        0 => "No more bottles".to_string(),
        1 => "1 bottle".to_string(),
        _ => format!("{} bottles", n),
    };
    let prev2 = if n == 0 {
        "no more bottles".to_string()
    } else {
        prev1.clone()
    };
    let line1 = format!("{} of beer on the wall, {} of beer.\n", prev1, prev2);
    let line2 = if n == 0 {
        "Go to the store and buy some more, 99 bottles of beer on the wall.\n".to_string()
    } else {
        let take = if n == 1 { "it" } else { "one" };
        let next = match n - 1 {
            0 => "no more bottles".to_string(),
            1 => "1 bottle".to_string(),
            _ => format!("{} bottles", n - 1),
        };
        format!(
            "Take {} down and pass it around, {} of beer on the wall.\n",
            take, next
        )
    };
    format!("{}{}", line1, line2)
}

pub fn sing(start: i32, end: i32) -> String {
    (end..=start)
        .rev()
        .map(verse)
        .collect::<Vec<String>>()
        .join("\n")
}
