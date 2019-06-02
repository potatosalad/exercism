extern crate bigdecimal;
#[macro_use]
extern crate num_derive;
extern crate num_traits;

use bigdecimal::BigDecimal;
use std::str::FromStr;
#[derive(Debug, Eq, Num, NumOps, One, Ord, PartialEq, PartialOrd, Zero)]
pub struct Decimal(BigDecimal);

impl Decimal {
    pub fn try_from(input: &str) -> Option<Decimal> {
        match BigDecimal::from_str(input) {
            Ok(n) => Some(Decimal(n)),
            _ => None,
        }
    }
}
