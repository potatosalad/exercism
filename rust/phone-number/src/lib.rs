pub fn number(user_number: &str) -> Option<String> {
    let mut v: Vec<char> = user_number.chars().filter(|c| c.is_ascii_digit()).collect();
    if v.len() == 11 {
        if v[0] != '1' {
            return None;
        }
        v.remove(0);
    }
    if v.len() == 10 {
        if v[0] == '0' || v[0] == '1' || v[3] == '0' || v[3] == '1' {
            None
        } else {
            Some(v.into_iter().collect())
        }
    } else {
        None
    }
}
