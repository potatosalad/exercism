extern crate num_cpus;

use std::collections::HashSet;
use std::sync::mpsc;
use std::thread;

pub type Palindrome = u64;

pub fn get_palindrome_products(min: u64, max: u64) -> Vec<Palindrome> {
    let cpus = num_cpus::get();
    if cpus == 1 {
        let mut products: HashSet<Palindrome> = HashSet::new();
        palindrome_products(min, max, 1, |product| {
            products.insert(product);
        });
        products.into_iter().collect()
    } else {
        let mut pool = Vec::new();
        let (tx, rx) = mpsc::channel::<Palindrome>();
        for n in 0..cpus {
            let tx = tx.clone();
            pool.push(Some(thread::spawn(move || {
                palindrome_products(min + n as u64, max, cpus, |product| {
                    tx.send(product).unwrap();
                });
            })));
        }
        for handle in &mut pool {
            if let Some(thread) = handle.take() {
                thread.join().unwrap();
            }
        }
        drop(tx);
        rx.into_iter()
            .collect::<HashSet<Palindrome>>()
            .into_iter()
            .collect()
    }
}

fn palindrome_products<F>(min: u64, max: u64, step: usize, mut cb: F)
where
    F: FnMut(Palindrome),
{
    for i in (min..=max).step_by(step) {
        for j in i..=max {
            let product = i * j;
            let digits = product.to_string();
            if digits.chars().eq(digits.chars().rev()) {
                cb(product);
            }
        }
    }
}

pub fn min(palindromes: &[Palindrome]) -> Option<Palindrome> {
    palindromes.iter().fold(None, |min, &p| match min {
        None => Some(p),
        Some(q) if p < q => Some(p),
        Some(q) => Some(q),
    })
}

pub fn max(palindromes: &[Palindrome]) -> Option<Palindrome> {
    palindromes.iter().fold(None, |max, &p| match max {
        None => Some(p),
        Some(q) if p > q => Some(p),
        Some(q) => Some(q),
    })
}
