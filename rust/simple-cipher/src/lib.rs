extern crate rand;

use rand::Rng;

pub fn encode(key: &str, s: &str) -> Option<String> {
    shift(key, s, &shift_encode)
}

pub fn decode(key: &str, s: &str) -> Option<String> {
    shift(key, s, &shift_decode)
}

pub fn encode_random(s: &str) -> (String, String) {
    let mut rng = rand::thread_rng();
    let key = (0..100).map(|_| rng.sample(Alphabetic)).collect::<String>();
    let out = encode(&key, s);
    (key, out.unwrap())
}

fn shift(key: &str, text: &str, shift_fn: &Fn(u8, u8) -> u8) -> Option<String> {
    if key.is_empty() || !key.chars().all(|c| c.is_ascii_lowercase()) {
        None
    } else {
        Some(
            text.bytes()
                .zip(key.bytes().cycle())
                .map(|(c, k)| shift_fn(c, k) as char)
                .collect(),
        )
    }
}

const BASE: u8 = ('a' as u32) as u8;

fn shift_encode(c: u8, k: u8) -> u8 {
    (c + k - 2 * BASE) % 26 + BASE
}

fn shift_decode(c: u8, k: u8) -> u8 {
    (c + 26 - k) % 26 + BASE
}

struct Alphabetic;

impl rand::distributions::Distribution<char> for Alphabetic {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> char {
        const RANGE: u32 = 26;
        const GEN_ALPHA_STR_CHARSET: &[u8] = b"abcdefghijklmnopqrstuvwxyz";
        loop {
            let var = rng.next_u32() >> (32 - 6);
            if var < RANGE {
                return GEN_ALPHA_STR_CHARSET[var as usize] as char;
            }
        }
    }
}
