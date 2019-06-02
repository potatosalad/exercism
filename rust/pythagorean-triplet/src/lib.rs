use std::collections::HashSet;

pub fn find(sum: u32) -> HashSet<[u32; 3]> {
    (1..(sum / 3))
        .filter_map(|b| {
            // a = (sum * (sum - 2 * b)) / (2 * (sum - b))
            let numerator = sum * (sum - 2 * b);
            let denominator = 2 * (sum - b);
            if numerator % denominator == 0 {
                let a = numerator / denominator;
                let c = sum - a - b;
                let mut v: [u32; 3] = [a, b, c];
                v.sort();
                Some(v)
            } else {
                None
            }
        })
        .collect()
}
