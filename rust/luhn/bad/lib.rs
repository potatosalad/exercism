#[derive(Clone, Debug)]
struct EveryNth<I: Iterator> {
    iter: I,
    nth: usize,
}

impl<I: Iterator> EveryNth<I> {
    pub fn new(iter: I, nth: usize) -> Self {
        assert!(nth != 0);
        EveryNth { iter, nth }
    }
}

impl<I: Iterator> Iterator for EveryNth<I> {
    type Item = I::Item;
    fn next(&mut self) -> Option<Self::Item> {
        let mut skipped: usize = 0;
        while skipped < self.nth {
            if let Some(item) = self.iter.next() {
                skipped += 1;
            } else {
                return None;
            }
        }
        self.iter.next()
    }
}