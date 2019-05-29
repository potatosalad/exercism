use std::cmp::Ordering;
use std::collections::HashSet;

#[derive(Debug, PartialEq, Eq)]
pub enum Classification {
    Abundant,
    Perfect,
    Deficient,
}

pub fn classify(num: u64) -> Option<Classification> {
    if num == 0 {
        None
    } else {
        match num.cmp(&num.aliquot_sum()) {
            Ordering::Equal => Some(Classification::Perfect),
            Ordering::Less => Some(Classification::Abundant),
            Ordering::Greater => Some(Classification::Deficient),
        }
    }
}

trait AliquotSum: Sized {
    fn aliquot_sum(&self) -> Self;
}

macro_rules! integer_aliquot_sum {
    ($T:ty) => {
        impl AliquotSum for $T {
            fn aliquot_sum(&self) -> $T {
                assert!(*self != 0);
                if *self == 1 {
                    0
                } else {
                    let mut seen: HashSet<$T> = HashSet::new();
                    let mut aliquot_sum: $T = 0;
                    for n in (1..).into_iter().take_while(|n| n * n < *self) {
                        if n == 1 {
                            if seen.insert(n) {
                                aliquot_sum += n;
                            }
                        } else if *self % n == 0 {
                            if seen.insert(n) {
                                aliquot_sum += n;
                            }
                            if seen.insert(*self / n) {
                                aliquot_sum += *self / n;
                            }
                        }
                    }
                    aliquot_sum
                }
            }
        }
    };
}

integer_aliquot_sum!(u64);
