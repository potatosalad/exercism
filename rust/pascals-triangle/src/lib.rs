pub struct PascalsTriangle {
    row_count: u32,
}

impl PascalsTriangle {
    pub fn new(row_count: u32) -> Self {
        PascalsTriangle { row_count }
    }

    pub fn rows(&self) -> Vec<Vec<u32>> {
        match self.row_count {
            0 => vec![],
            1 => vec![vec![1]],
            _ => {
                let mut rows = vec![vec![1], vec![1, 1]];
                for i in 1..(self.row_count as usize - 1) {
                    let mut row = Vec::new();
                    for j in 0..=(i as usize + 1) {
                        match j {
                            0 => row.push(1),
                            _ if j == i + 1 => row.push(1),
                            _ => row.push(rows[i][j - 1] + rows[i][j]),
                        }
                    }
                    rows.push(row);
                }
                rows
            }
        }
    }
}
