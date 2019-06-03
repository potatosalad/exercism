pub fn lowest_price(books: &[u32]) -> u32 {
    let mut books: Vec<usize> = books.iter().map(|&book| book as usize).collect();
    books.sort();
    (combinations(&books[..])
        .into_iter()
        .map(|combination| match combination.len() {
            2 => 2.0 * 7.6,
            3 => 3.0 * 7.2,
            4 => 4.0 * 6.4,
            5 => 5.0 * 6.0,
            _ => 8.0,
        })
        .sum::<f64>()
        * 100.0)
        .round() as u32
}

fn combinations(books: &[usize]) -> Vec<Vec<usize>> {
    let mut combinations = Vec::new();
    if !books.is_empty() {
        combinations.push(Vec::new());
    }
    'next_book: for book in books {
        let sizes: Vec<usize> = combinations
            .clone()
            .into_iter()
            .map(|combination| combination.len())
            .collect();
        let threes = sizes.iter().filter(|&&size| size == 3).count();
        for (i, combination) in combinations.iter_mut().enumerate() {
            if !combination.contains(book) && (sizes[i] != 4 || threes == 0) {
                combination.push(*book);
                continue 'next_book;
            }
        }
        combinations.push(Vec::new());
        combinations.last_mut().unwrap().push(*book);
    }
    combinations
}
