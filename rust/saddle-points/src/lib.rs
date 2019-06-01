pub fn find_saddle_points(input: &[Vec<u64>]) -> Vec<(usize, usize)> {
    if input.is_empty() {
        return vec![];
    }
    let mut xmaxs: Vec<u64> = Vec::with_capacity(input.len());
    let mut ymins: Vec<u64> = Vec::new();
    for (row, xs) in input.iter().enumerate() {
        for (col, x) in xs.iter().enumerate() {
            if row + 1 > xmaxs.len() {
                xmaxs.push(*x);
            } else if *x > xmaxs[row] {
                xmaxs[row] = *x;
            }
            if col + 1 > ymins.len() {
                ymins.push(*x);
            } else if *x < ymins[col] {
                ymins[col] = *x;
            }
        }
    }
    if xmaxs.is_empty() || ymins.is_empty() {
        return vec![];
    }
    let mut points: Vec<(usize, usize)> = Vec::new();
    for (row, xs) in input.iter().enumerate() {
        let xmax = xmaxs[row];
        for (col, x) in xs.iter().enumerate() {
            let ymin = ymins[col];
            if *x == xmax && *x == ymin {
                points.push((row, col));
            }
        }
    }
    points
}
