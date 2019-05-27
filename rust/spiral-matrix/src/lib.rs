pub fn spiral_matrix(size: u32) -> Vec<Vec<u32>> {
    if size == 0 {
        return vec![];
    }
    if size == 1 {
        return vec![vec![1]];
    }
    let mut m = vec![vec![0; size as usize]; size as usize];
    let mut r: i8 = 0;
    let mut c: i8 = 0;
    let mut x: i8 = 0;
    let mut y: i8 = 1;
    let mut i = 0;
    let s = size as i8;
    while m[r as usize][c as usize] == 0 {
        i += 1;
        m[r as usize][c as usize] = i;
        if !(0 <= r + x && r + x < s)
            || !(0 <= c + y && c + y < s)
            || m[(r + x) as usize][(c + y) as usize] != 0
        {
            let t = y;
            y = -x;
            x = t;
        }
        r += x;
        c += y;
    }
    m
}
