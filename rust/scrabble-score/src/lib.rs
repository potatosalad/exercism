pub fn score(word: &str) -> u64 {
    simple_score_iter(word).sum()
}

fn simple_score_iter<'a>(word: &'a str) -> Box<Iterator<Item = u64> + 'a> {
    Box::new(word.chars().map(|c| match c.to_ascii_uppercase() {
        'A' | 'E' | 'I' | 'O' | 'U' | 'L' | 'N' | 'R' | 'S' | 'T' => 1,
        'D' | 'G' => 2,
        'B' | 'C' | 'M' | 'P' => 3,
        'F' | 'H' | 'V' | 'W' | 'Y' => 4,
        'K' => 5,
        'J' | 'X' => 8,
        'Q' | 'Z' => 10,
        _ => 0,
    }))
}

#[derive(Clone, Debug)]
pub enum ScrabbleSquare {
    SingleLetter,
    DoubleLetter,
    TripleLetter,
    DoubleWord,
    TripleWord,
}

pub fn score_complex(word: &str, squares: &[ScrabbleSquare]) -> Option<u64> {
    if word.len() == squares.len() {
        let base_score = simple_score_iter(word)
            .zip(squares.iter())
            .map(|(score, square)| match square {
                ScrabbleSquare::DoubleLetter => score * 2,
                ScrabbleSquare::TripleLetter => score * 3,
                _ => score,
            })
            .sum();
        Some(
            squares
                .iter()
                .fold(base_score, |score, square| match square {
                    ScrabbleSquare::DoubleWord => score * 2,
                    ScrabbleSquare::TripleWord => score * 3,
                    _ => score,
                }),
        )
    } else {
        None
    }
}
