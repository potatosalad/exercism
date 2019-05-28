#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate maplit;

use std::collections::HashMap;
use std::collections::VecDeque;

pub fn encode(n: u64) -> String {
    SayEncoder::new(n).collect::<Vec<String>>().join(" ")
}

static MAGNITUDES: [&'static str; 7] = [
    "",
    "thousand",
    "million",
    "billion",
    "trillion",
    "quadrillion",
    "quintillion",
];

lazy_static! {
    static ref NUMBERS: HashMap<u64, &'static str> = hashmap! {
        0 => "zero",
        1 => "one",
        2 => "two",
        3 => "three",
        4 => "four",
        5 => "five",
        6 => "six",
        7 => "seven",
        8 => "eight",
        9 => "nine",
        10 => "ten",
        11 => "eleven",
        12 => "twelve",
        13 => "thirteen",
        14 => "fourteen",
        15 => "fifteen",
        16 => "sixteen",
        17 => "seventeen",
        18 => "eighteen",
        19 => "nineteen",
        20 => "twenty",
        30 => "thirty",
        40 => "forty",
        50 => "fifty",
        60 => "sixty",
        70 => "seventy",
        80 => "eighty",
        90 => "ninety",
    };
}

#[derive(Clone, Debug)]
pub struct SayEncoder {
    n: u64,
    group: Option<VecDeque<(u64, &'static str)>>,
    done: bool,
}

impl SayEncoder {
    pub fn new(n: u64) -> Self {
        SayEncoder {
            n,
            group: None,
            done: false,
        }
    }
}

impl Iterator for SayEncoder {
    type Item = String;
    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            None
        } else if let Some(word) = NUMBERS.get(&self.n) {
            self.done = true;
            Some(word.to_string())
        } else if let Some(mut group) = self.group.take() {
            if let Some((x, magnitude)) = group.pop_front() {
                self.group = Some(group);
                if magnitude.is_empty() {
                    Some(encode_less_than_one_thousand(x))
                } else {
                    Some(format!(
                        "{} {}",
                        encode_less_than_one_thousand(x),
                        magnitude
                    ))
                }
            } else {
                self.done = true;
                None
            }
        } else {
            let mut number = self.n;
            let mut group: Vec<u64> = Vec::new();
            while number >= 1000 {
                group.insert(0, number - number / 1000 * 1000);
                number /= 1000;
            }
            group.insert(0, number);
            let group: VecDeque<(u64, &'static str)> = group
                .iter()
                .zip(MAGNITUDES[..group.len()].iter().rev())
                .filter(|(&x, _)| x != 0)
                .map(|(&x, &magnitude)| (x, magnitude))
                .collect();
            self.group = Some(group);
            self.next()
        }
    }
}

fn encode_less_than_one_hundred(n: u64) -> String {
    assert!(n < 100);
    if let Some(word) = NUMBERS.get(&n) {
        word.to_string()
    } else {
        format!(
            "{}-{}",
            NUMBERS.get(&(n / 10 * 10)).unwrap(),
            NUMBERS.get(&(n - n / 10 * 10)).unwrap()
        )
    }
}

fn encode_less_than_one_thousand(n: u64) -> String {
    assert!(n < 1000);
    if let Some(word) = NUMBERS.get(&n) {
        word.to_string()
    } else if n % 100 == 0 {
        format!("{} hundred", NUMBERS.get(&(n / 100)).unwrap())
    } else if n < 100 {
        encode_less_than_one_hundred(n)
    } else {
        format!(
            "{} hundred {}",
            NUMBERS.get(&(n / 100)).unwrap(),
            encode_less_than_one_hundred(n - n / 100 * 100)
        )
    }
}
