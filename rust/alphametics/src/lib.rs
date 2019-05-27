use std::collections::HashMap;
use std::collections::HashSet;

pub fn solve(input: &str) -> Option<HashMap<char, u8>> {
    Alphametics::new(input).solve()
}

#[derive(Clone, Debug)]
struct Alphametics {
    matrix: Vec<Vec<char>>,
    terminals: HashSet<char>,
    solution: Option<HashMap<char, u8>>,
    solved: bool,
}

impl Alphametics {
    pub fn new(input: &str) -> Self {
        let mut matrix: Vec<Vec<char>> = Vec::new();
        let mut terminals: HashSet<char> = HashSet::new();
        for s in input
            .split(' ')
            .filter(|s| !s.is_empty() && s.chars().all(|c| c.is_alphabetic() && c.is_uppercase()))
        {
            terminals.insert(s.chars().nth(0).unwrap());
            for (i, c) in s.chars().rev().enumerate() {
                if matrix.len() <= i {
                    matrix.push(Vec::new());
                }
                matrix[i].push(c);
            }
        }
        Alphametics {
            matrix,
            terminals,
            solution: None,
            solved: false,
        }
    }

    pub fn solve(&mut self) -> Option<HashMap<char, u8>> {
        match &self.solution {
            Some(solution) => Some(solution.clone()),
            None if self.solved => None,
            None => {
                let mut map: HashMap<char, u8> = HashMap::new();
                if Self::solve_column(&mut map, &self.terminals, self.matrix.as_slice(), 0) {
                    self.solution = Some(map);
                } else {
                    self.solution = None;
                }
                self.solved = true;
                self.solve()
            }
        }
    }

    fn solve_column(
        map: &mut HashMap<char, u8>,
        terminals: &HashSet<char>,
        matrix: &[Vec<char>],
        carry: u8,
    ) -> bool {
        if matrix.is_empty() {
            return true;
        }
        let mut letters = matrix[0].iter().cloned().collect::<HashSet<char>>();
        let mut numbers = (0..10).collect::<HashSet<u8>>();
        for (letter, number) in map.iter() {
            letters.remove(letter);
            numbers.remove(number);
        }
        let letters = letters.into_iter().collect::<Vec<char>>();
        let numbers = numbers.into_iter().collect::<Vec<u8>>();
        Self::solve_row(
            map,
            &terminals,
            matrix,
            carry,
            letters.as_slice(),
            numbers.as_slice(),
        )
    }

    fn evaluate_column(map: &HashMap<char, u8>, column: &[char], carry: u8) -> Option<u8> {
        let mut values: Vec<u8> = Vec::with_capacity(column.len());
        for c in column.iter() {
            if let Some(v) = map.get(c) {
                values.push(*v);
            } else {
                return None;
            }
        }
        let expected = u64::from(*values.last().unwrap());
        let actual: u64 = values
            .iter()
            .take(column.len() - 1)
            .cloned()
            .map(u64::from)
            .chain(std::iter::once(u64::from(carry)))
            .sum();
        if expected == (actual % 10) {
            Some((actual / 10) as u8)
        } else {
            None
        }
    }

    fn solve_row(
        map: &mut HashMap<char, u8>,
        terminals: &HashSet<char>,
        matrix: &[Vec<char>],
        carry: u8,
        letters: &[char],
        numbers: &[u8],
    ) -> bool {
        if letters.is_empty() {
            if let Some(carry) = Self::evaluate_column(map, &matrix[0], carry) {
                Self::solve_column(map, &terminals, &matrix[1..], carry)
            } else {
                false
            }
        } else {
            let (letter, other_letters) = letters.split_first().unwrap();
            for number in numbers.iter().cloned() {
                if terminals.contains(letter) && number == 0 {
                    continue;
                }
                let other_numbers = numbers
                    .iter()
                    .filter(|&&n| n != number)
                    .cloned()
                    .collect::<Vec<u8>>();
                map.insert(*letter, number);
                if Self::solve_row(
                    map,
                    &terminals,
                    matrix,
                    carry,
                    other_letters,
                    other_numbers.as_slice(),
                ) {
                    return true;
                }
                map.remove(letter);
            }
            false
        }
    }
}
