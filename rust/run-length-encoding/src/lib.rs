pub fn encode(source: &str) -> String {
    RunLengthEncoder::new(source).collect()
}

pub fn decode(source: &str) -> String {
    RunLengthDecoder::new(source).collect()
}

#[derive(Clone, Debug)]
struct RunLengthEncoder<'a> {
    iter: std::str::Chars<'a>,
    prev: Option<char>,
}

impl<'a> RunLengthEncoder<'a> {
    pub fn new(source: &'a str) -> Self {
        RunLengthEncoder {
            iter: source.chars(),
            prev: None,
        }
    }
}

impl<'a> Iterator for RunLengthEncoder<'a> {
    type Item = String;
    fn next(&mut self) -> Option<String> {
        match (self.prev.take(), self.iter.next()) {
            (Some(a), Some(b)) => {
                if a == b {
                    let mut count = 2;
                    while let Some(c) = self.iter.next() {
                        if a == c {
                            count += 1;
                            continue;
                        } else {
                            self.prev = Some(c);
                            break;
                        }
                    }
                    Some(format!("{}{}", count, a))
                } else {
                    self.prev = Some(b);
                    Some(a.to_string())
                }
            }
            (None, Some(a)) => {
                self.prev = Some(a);
                self.next()
            }
            (Some(a), None) => Some(a.to_string()),
            (None, None) => None,
        }
    }
}

#[derive(Clone, Debug)]
struct RunLengthDecoder<'a> {
    iter: std::str::Chars<'a>,
    prev: Option<char>,
}

impl<'a> RunLengthDecoder<'a> {
    pub fn new(source: &'a str) -> Self {
        RunLengthDecoder {
            iter: source.chars(),
            prev: None,
        }
    }
}

impl<'a> Iterator for RunLengthDecoder<'a> {
    type Item = String;
    fn next(&mut self) -> Option<String> {
        match self.prev.take() {
            Some(a) => {
                if a.is_digit(10) {
                    let mut count = a.to_string();
                    while let Some(b) = self.iter.next() {
                        if b.is_digit(10) {
                            count.push(b);
                            continue;
                        } else {
                            return Some(b.to_string().repeat(count.parse::<usize>().unwrap()));
                        }
                    }
                    panic!("invalid run length decoding")
                } else {
                    Some(a.to_string())
                }
            }
            None => {
                if let Some(a) = self.iter.next() {
                    self.prev = Some(a);
                    self.next()
                } else {
                    None
                }
            }
        }
    }
}
