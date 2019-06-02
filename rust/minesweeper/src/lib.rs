pub fn annotate(minefield: &[&str]) -> Vec<String> {
    minefield
        .iter()
        .enumerate()
        .map(|(i, row)| {
            row.chars()
                .enumerate()
                .map(|(j, c)| minecount(minefield, i, j, c))
                .collect::<String>()
        })
        .collect()
}

fn minecount(minefield: &[&str], i: usize, j: usize, c: char) -> char {
    if c != ' ' {
        c
    } else {
        let mut count: u8 = 0;
        for x in 0..=2 {
            for y in 0..=2 {
                if i + 1 < x
                    || i + 1 - x >= minefield.len()
                    || j + 1 < y
                    || j + 1 - y >= minefield[i].len()
                    || (x == 1 && y == 1)
                {
                    continue;
                } else if &minefield[i + 1 - x][(j + 1 - y)..=(j + 1 - y)] == "*" {
                    count += 1;
                }
            }
        }
        if count == 0 {
            ' '
        } else {
            (count + b'0') as char
        }
    }
}
