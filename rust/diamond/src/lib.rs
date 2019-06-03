pub fn get_diamond(c: char) -> Vec<String> {
    assert!(c.is_ascii_alphabetic() && c.is_ascii_uppercase());
    if c == 'A' {
        vec!["A".to_string()]
    } else {
        let root = c as u8 - b'A';
        let width = ((root as usize + 1) * 2) - 1;
        (0_u8..=root)
            .chain((0_u8..root).rev())
            .map(|side| get_diamond_line(side, width))
            .collect()
    }
}

fn get_diamond_line(side: u8, width: usize) -> String {
    if side == 0 {
        let padlen = (width - 1) / 2;
        let padstr = " ".repeat(padlen);
        let letter = (side + b'A') as char;
        format!("{}{}{}", padstr, letter, padstr)
    } else {
        let sidelen = ((side as usize + 1) * 2) - 1;
        let gaplen = sidelen - 2;
        let padlen = (width - gaplen - 2) / 2;
        let gapstr = " ".repeat(gaplen);
        let padstr = " ".repeat(padlen);
        let letter = (side + b'A') as char;
        format!("{}{}{}{}{}", padstr, letter, gapstr, letter, padstr)
    }
}
