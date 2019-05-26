pub fn count(lines: &[&str]) -> u32 {
    (0..lines.len())
        .flat_map(|i| std::iter::repeat(i).zip(0..lines[i].len()))
        .map(|(i, j)| count_rectangles(&lines, i, j))
        .sum()
}

fn count_rectangles(lines: &[&str], i: usize, j: usize) -> u32 {
    if &lines[i][j..=j] != "+" {
        return 0;
    }
    ((i + 1)..lines.len())
        .flat_map(|m| std::iter::repeat(m).zip((j + 1)..lines[m].len()))
        .filter(|(m, n)| is_rectancle(&lines, i, j, *m, *n))
        .count() as u32
}

fn is_rectancle(lines: &[&str], i: usize, j: usize, m: usize, n: usize) -> bool {
    if &lines[i][j..=j] != "+"
        || &lines[i][n..=n] != "+"
        || &lines[m][j..=j] != "+"
        || &lines[m][n..=n] != "+"
    {
        return false;
    }
    for line in lines.iter().take(m).skip(i) {
        if !(&line[j..=j] == "|" || &line[j..=j] == "+")
            || !(&line[n..=n] == "|" || &line[n..=n] == "+")
        {
            return false;
        }
    }
    for h in j..n {
        if !(&lines[i][h..=h] == "-" || &lines[i][h..=h] == "+")
            || !(&lines[m][h..=h] == "-" || &lines[m][h..=h] == "+")
        {
            return false;
        }
    }
    true
}
