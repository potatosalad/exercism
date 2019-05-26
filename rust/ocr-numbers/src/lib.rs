#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidRowCount(usize),
    InvalidColumnCount(usize),
}

pub fn convert(input: &str) -> Result<String, Error> {
    let lines: Vec<String> = input.split('\n').map(|s| s.to_string()).collect();
    let mut out = String::new();
    for i in 0.. {
        let row = i * 4;
        if row + 4 > lines.len() {
            return Err(Error::InvalidRowCount(lines.len()));
        }
        for j in 0.. {
            let col = j * 3;
            if lines[row].len() == col {
                break;
            }
            if let Some(bad_line) = lines[row..row + 4].iter().find(|line| line.len() < col + 3) {
                return Err(Error::InvalidColumnCount(bad_line.len()));
            }
            out.push_str(ocr(lines[row..row + 4]
                .iter()
                .map(|line| line[col..col + 3].to_string())
                .collect::<Vec<String>>()
                .join("\n")));
        }
        if row + 4 == lines.len() {
            break;
        }
        out.push_str(",");
    }
    Ok(out)
}

fn ocr(s: String) -> &'static str {
    match s.as_str() {
        " _ \n| |\n|_|\n   " => "0",
        "   \n  |\n  |\n   " => "1",
        " _ \n _|\n|_ \n   " => "2",
        " _ \n _|\n _|\n   " => "3",
        "   \n|_|\n  |\n   " => "4",
        " _ \n|_ \n _|\n   " => "5",
        " _ \n|_ \n|_|\n   " => "6",
        " _ \n  |\n  |\n   " => "7",
        " _ \n|_|\n|_|\n   " => "8",
        " _ \n|_|\n _|\n   " => "9",
        _ => "?",
    }
}
