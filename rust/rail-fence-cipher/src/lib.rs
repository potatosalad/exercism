pub struct RailFence(usize);

impl RailFence {
    pub fn new(rails: u32) -> RailFence {
        assert!(rails != 0);
        RailFence(rails as usize)
    }

    pub fn encode(&self, text: &str) -> String {
        if self.0 == 1 {
            text.to_string()
        } else {
            self.positions(text)
                .into_iter()
                .map(|position| text.chars().nth(position).unwrap())
                .collect()
        }
    }

    pub fn decode(&self, cipher: &str) -> String {
        if self.0 == 1 {
            cipher.to_string()
        } else {
            let positions = self.positions(cipher);
            let mut text = vec![' '; positions.len()];
            for (i, c) in cipher.chars().enumerate() {
                let position = positions[i];
                text[position] = c;
            }
            text.into_iter().collect()
        }
    }

    fn positions(&self, input: &str) -> Vec<usize> {
        assert!(self.0 > 1);
        let length = input.chars().count();
        let mut matrix = vec![vec![(' ', false); length]; self.0];
        for (col, c) in input.chars().enumerate() {
            let pad = 2 * self.0 - 2;
            let row = if col % pad < pad / 2 {
                col % pad
            } else {
                pad - col % pad
            };
            matrix[row][col] = (c, true);
        }
        let order: Vec<usize> = matrix
            .into_iter()
            .flat_map(|row| {
                row.into_iter()
                    .enumerate()
                    .filter_map(|(col, (_, x))| if x { Some(col) } else { None })
            })
            .collect();
        order
    }
}
