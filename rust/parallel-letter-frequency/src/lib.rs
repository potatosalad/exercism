use std::collections::HashMap;
use std::sync::mpsc;
use std::thread;

pub fn frequency(input: &[&str], worker_count: usize) -> HashMap<char, usize> {
    if worker_count == 0 {
        panic!("worker_count must be greater than 0");
    }
    let non_empty_input = input
        .iter()
        .filter_map(|s| if s.is_empty() { None } else { Some(*s) })
        .collect::<Vec<&str>>();
    if non_empty_input.is_empty() {
        return HashMap::new();
    }
    let (tx, rx) = mpsc::channel();
    let ilen: usize = non_empty_input.iter().map(|s| s.len()).sum();
    let input = non_empty_input.into_iter().flat_map(|s| s.chars());
    let clen = (ilen / worker_count) + if ilen % worker_count == 0 { 0 } else { 1 };
    let mut workers = 0;
    for chunk in Chunks::new(input, clen) {
        let channel = tx.clone();
        thread::spawn(move || count_frequency(chunk, channel));
        workers += 1;
    }
    let mut hm: HashMap<char, usize> = HashMap::new();
    for _ in 0..workers {
        for (c, count) in rx.recv().unwrap() {
            hm.entry(c).and_modify(|v| *v += count).or_insert(count);
        }
    }
    hm
}

fn count_frequency(input: Vec<char>, channel: mpsc::Sender<HashMap<char, usize>>) {
    let mut hm: HashMap<char, usize> = HashMap::new();
    for c in input
        .into_iter()
        .filter(|c| c.is_alphabetic())
        .flat_map(|c| c.to_lowercase())
    {
        hm.entry(c).and_modify(|v| *v += 1).or_insert(1);
    }
    channel.send(hm).unwrap();
}

#[derive(Clone, Debug)]
struct Chunks<I: Iterator> {
    i: I,
    chunk_size: usize,
}

impl<I: Iterator> Chunks<I> {
    pub fn new(i: I, chunk_size: usize) -> Self {
        assert!(chunk_size != 0);
        Chunks { i, chunk_size }
    }
}

impl<I: Iterator> Iterator for Chunks<I> {
    type Item = Vec<I::Item>;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(item) = self.i.next() {
            let mut chunk: Vec<I::Item> = Vec::with_capacity(self.chunk_size);
            chunk.push(item);
            while chunk.len() < self.chunk_size {
                if let Some(item) = self.i.next() {
                    chunk.push(item);
                } else {
                    break;
                }
            }
            Some(chunk)
        } else {
            None
        }
    }
}
