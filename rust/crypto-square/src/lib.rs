pub fn encrypt(input: &str) -> String {
    let plaintext: Vec<char> = input
        .chars()
        .filter(|c| c.is_ascii_alphanumeric())
        .map(|c| c.to_ascii_lowercase())
        .collect();
    if plaintext.is_empty() {
        String::new()
    } else {
        let rows = (((plaintext.len() - 1) as f64).sqrt() as usize) + 1;
        let cols = (0..rows)
            .map(|r| {
                (0..)
                    .map(move |c| r + c * rows)
                    .take_while(|&i| i < plaintext.len())
                    .count()
            })
            .max()
            .unwrap()
            + 1;
        let mut out: String = (0..rows)
            .flat_map(|r| {
                let mut row: Vec<char> = (0..)
                    .map(move |c| r + c * rows)
                    .take_while(|&i| i < plaintext.len())
                    .map(|i| plaintext.get(i).unwrap())
                    .cloned()
                    .collect();
                while row.len() < cols {
                    row.push(' ');
                }
                row
            })
            .collect();
        out.remove(out.len() - 1);
        out
    }
}
