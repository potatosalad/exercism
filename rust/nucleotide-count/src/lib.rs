use std::collections::HashMap;

pub fn count(nucleotide: char, dna: &str) -> Result<usize, char> {
    if !is_nucleotide(nucleotide) {
        return Err(nucleotide);
    }
    let mut x = 0;
    for c in dna.chars() {
        if !is_nucleotide(c) {
            return Err(c);
        } else if c == nucleotide {
            x += 1;
        }
    }
    Ok(x)
}

pub fn nucleotide_counts(dna: &str) -> Result<HashMap<char, usize>, char> {
    let mut acc = HashMap::new();
    acc.insert('A', 0);
    acc.insert('C', 0);
    acc.insert('G', 0);
    acc.insert('T', 0);
    for c in dna.chars() {
        if !is_nucleotide(c) {
            return Err(c);
        } else {
            acc.entry(c).and_modify(|v| *v += 1).or_insert(1);
        }
    }
    Ok(acc)
}

fn is_nucleotide(c: char) -> bool {
    match c {
        'A' | 'C' | 'G' | 'T' => true,
        _ => false,
    }
}
