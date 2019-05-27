use std::collections::HashMap;
use std::collections::HashSet;

pub fn solve(input: &str) -> Option<HashMap<char, u8>> {
    println!("");
    Alphametics::new(input).solve()
}

#[derive(Clone, Debug)]
struct Alphametics {
    chars: HashSet<char>,
    matrix: Vec<Vec<char>>,
    // lhs: Vec<String>,
    // rhs: String,
    // size: usize,
    // values: [u8; 10],
    // idx: Vec<usize>,
    // set: HashSet<Vec<(char, u8)>>,
}

impl Alphametics {
    pub fn new(input: &str) -> Self {
        let mut chars: [char; 10] = [0 as char; 10];
        let mut size: usize = 0;
        let values: [u8; 10] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
        for c in input
            .chars()
            .filter(|c| c.is_alphabetic() && c.is_uppercase())
        {
            if !chars.contains(&c) {
                size += 1;
                if size > 10 {
                    panic!("Alphametics only supports up to 10 distinct characters");
                }
                chars[size - 1] = c;
            }
        }
        match input.split(" == ").collect::<Vec<&str>>()[..] {
            [variables, total] => {
                let lhs: Vec<String> = variables
                    .split(" + ")
                    .map(|v| {
                        v.chars()
                            .filter(|c| c.is_alphabetic() && c.is_uppercase())
                            .collect::<String>()
                    })
                    .collect();
                let rhs: String = total
                    .chars()
                    .filter(|c| c.is_alphabetic() && c.is_uppercase())
                    .collect();
                Alphametics {
                    chars,
                    lhs,
                    rhs,
                    size,
                    values,
                    idx: std::iter::repeat(0).take(size).collect(),
                    set: HashSet::new(),
                }
            }
            _ => panic!("Bad input for Alphametics"),
        }
    }

    pub fn is_solution(&self, variables: &HashMap<char, u8>) -> bool {
        if let Some((lhs, rhs)) = self.solve_with(variables) {
            println!("{:?} == {:?}", lhs, rhs);
            lhs == rhs
        } else {
            false
        }
    }

    pub fn solve(&self) -> Option<HashMap<char, u8>> {
        // for variables in self.clone() {
        //     if self.is_solution(&variables) {
        //         return Some(variables);
        //     }
        // }
        // None
        self.clone().find(|variables| self.is_solution(variables))
    }

    pub fn solve_with(&self, variables: &HashMap<char, u8>) -> Option<(u64, u64)> {
        if variables.len() != self.size as usize
            || !self.chars.iter().all(|c| variables.contains_key(c))
        {
            None
        } else {
            let lhs: Vec<String> = self.lhs.iter().map(|v| {
                v.chars()
                    .map(|c| (variables.get(&c).unwrap() + '0' as u8) as char)
                    .collect::<String>()
            }).collect();
            let rhs: String = self
                .rhs
                .chars()
                .map(|c| (variables.get(&c).unwrap() + '0' as u8) as char)
                .collect();
            println!("lhs = {:?}", lhs);
            println!("rhs = {:?}", rhs);
            if lhs.iter().any(|v| v.starts_with("0")) || rhs.starts_with("0") {
                None
            } else {
                let lhs: u64 = lhs.iter().map(|v| v.parse::<u64>().unwrap()).sum();
                let rhs: u64 = rhs.parse().unwrap();
                Some((lhs, rhs))
            }
        }
    }
}

impl Iterator for Alphametics {
    type Item = HashMap<char, u8>;
    fn next(&mut self) -> Option<Self::Item> {
        let mut item: Self::Item = HashMap::with_capacity(self.size);
        let mut values: Vec<u8> = self.values.to_vec();
        for (i, c) in self.chars[0..self.size].iter().enumerate() {
            if self.idx[i] >= values.len() {
                self.idx[i] = 0;
                if self.idx.len() <= i + 1 {
                    return None;
                } else {
                    self.idx[i + 1] += 1;
                }
            }
            item.insert(c.clone(), values.remove(self.idx[i]));
        }
        self.idx[0] += 1;
        // let vec = item.clone().into_iter().collect::<Vec<(char, u8)>>();
        // if self.set.contains(&vec) {
        //     panic!("foo bar");
        // } else {
        //     self.set.insert(vec);
            Some(item)
        // }
        // if self.idx[i] >= values.len() {
        //         self.idx[i] = 0;
        //         if self.idx.len() <= i + 1 { return None }
        //         else { self.idx[i+1] += 1 }
        //     }

        // if self.base < self.size {
        //     if self.perm < self.size {
        //         let item: Self::Item = self.chars[0..self.size as usize]
        //             .iter()
        //             .enumerate()
        //             .map(|(i, &c)| (c, (i as u8 + self.base + self.perm) % self.size))
        //             .collect();
        //         self.perm += 1;
        //         Some(item)
        //     } else {
        //         self.base += 1;
        //         self.perm = 0;
        //         self.next()
        //     }
        // } else {
        //     None
        // }
    }
}

// #[derive(Clone, Debug)]
// struct Equation
