#[macro_use]
extern crate lazy_static;

use std::fmt::{Display, Formatter, Result};

pub struct Roman(String);

impl Display for Roman {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.0)
    }
}

lazy_static! {
    static ref ROMAN_DIGITS: Vec<(u32, &'static str)> = {
        let mut v = vec![
            (1, "I"),
            (4, "IV"),
            (5, "V"),
            (9, "IX"),
            (10, "X"),
            (40, "XL"),
            (50, "L"),
            (90, "XC"),
            (100, "C"),
            (400, "CD"),
            (500, "D"),
            (900, "CM"),
            (1000, "M"),
        ];
        v.reverse();
        v
    };
}

impl From<u32> for Roman {
    fn from(num: u32) -> Self {
        let mut data = String::new();
        let mut n = num;
        for (value, digit) in ROMAN_DIGITS.iter() {
            if n >= *value {
                data += &digit.repeat((n / *value) as usize);
                n %= *value;
            }
        }
        Roman(data)
    }
}
