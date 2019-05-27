use std::collections::HashMap;

pub type Value = i32;
pub type ForthResult = Result<(), Error>;

#[derive(Debug, Default, PartialEq)]
pub struct Forth {
    stack: Vec<Value>,
    words: HashMap<String, Vec<String>>,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    DivisionByZero,
    StackUnderflow,
    UnknownWord,
    InvalidWord,
}

impl Forth {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn stack(&self) -> Vec<Value> {
        self.stack.clone()
    }

    pub fn eval(&mut self, input: &str) -> ForthResult {
        let mut tokens = Tokens::new(input.chars().flat_map(|c| c.to_lowercase()));
        while let Some(token) = tokens.next() {
            match token.as_str() {
                t if self.words.contains_key(t) => {
                    for dword in self.words.get(t).unwrap() {
                        tokens.push(dword.to_string());
                    }
                    Ok(())
                }
                ":" => match tokens.next() {
                    Some(word) => {
                        if word.parse::<Value>().is_ok() {
                            Err(Error::InvalidWord)
                        } else {
                            let mut definition = Vec::new();
                            let mut result = Err(Error::InvalidWord);
                            while let Some(token) = tokens.next() {
                                if token.as_str() == ";" {
                                    self.words.insert(word, definition);
                                    result = Ok(());
                                    break;
                                } else if self.words.contains_key(&token) {
                                    for dword in self.words.get(&token).unwrap() {
                                        definition.insert(0, dword.to_string());
                                    }
                                } else {
                                    definition.insert(0, token);
                                }
                            }
                            result
                        }
                    }
                    None => Err(Error::InvalidWord),
                },
                "+" => self.add(),
                "-" => self.subtract(),
                "/" => self.divide(),
                "*" => self.multiply(),
                "dup" => self.dup(),
                "drop" => self.drop(),
                "swap" => self.swap(),
                "over" => self.over(),
                t => self.value(t),
            }?;
        }
        Ok(())
    }

    fn add(&mut self) -> ForthResult {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            self.stack.push(b + a);
            Ok(())
        } else {
            Err(Error::StackUnderflow)
        }
    }

    fn subtract(&mut self) -> ForthResult {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            self.stack.push(b - a);
            Ok(())
        } else {
            Err(Error::StackUnderflow)
        }
    }

    fn divide(&mut self) -> ForthResult {
        match (self.stack.pop(), self.stack.pop()) {
            (Some(a), Some(b)) => {
                if a == 0 {
                    Err(Error::DivisionByZero)
                } else {
                    self.stack.push(b / a);
                    Ok(())
                }
            }
            _ => Err(Error::StackUnderflow),
        }
    }

    fn multiply(&mut self) -> ForthResult {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            self.stack.push(b * a);
            Ok(())
        } else {
            Err(Error::StackUnderflow)
        }
    }

    fn dup(&mut self) -> ForthResult {
        if let Some(v) = self.stack.pop() {
            self.stack.push(v);
            self.stack.push(v);
            Ok(())
        } else {
            Err(Error::StackUnderflow)
        }
    }

    fn drop(&mut self) -> ForthResult {
        if self.stack.pop().is_some() {
            Ok(())
        } else {
            Err(Error::StackUnderflow)
        }
    }

    fn swap(&mut self) -> ForthResult {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            self.stack.push(a);
            self.stack.push(b);
            Ok(())
        } else {
            Err(Error::StackUnderflow)
        }
    }

    fn over(&mut self) -> ForthResult {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            self.stack.push(b);
            self.stack.push(a);
            self.stack.push(b);
            Ok(())
        } else {
            Err(Error::StackUnderflow)
        }
    }

    fn value(&mut self, value: &str) -> ForthResult {
        if let Ok(v) = value.parse() {
            self.stack.push(v);
            Ok(())
        } else {
            Err(Error::UnknownWord)
        }
    }
}

#[derive(Clone, Debug)]
struct Tokens<I: Iterator> {
    iter: I,
    more: Option<Vec<String>>,
}

impl<I: Iterator<Item = char>> Iterator for Tokens<I> {
    type Item = String;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(more) = &mut self.more {
            if let Some(s) = more.pop() {
                return Some(s);
            } else {
                self.more = None;
            }
        }
        let mut s = String::new();
        loop {
            match self.iter.next() {
                Some(c) if c.is_whitespace() || c.is_control() => {
                    if s.is_empty() {
                        continue;
                    } else {
                        return Some(s);
                    }
                }
                Some(c) => s.push(c),
                None => {
                    if s.is_empty() {
                        return None;
                    } else {
                        return Some(s);
                    }
                }
            }
        }
    }
}

impl<I: Iterator> Tokens<I> {
    pub fn new(iter: I) -> Self {
        Tokens { iter, more: None }
    }

    pub fn push(&mut self, item: String) {
        match &mut self.more {
            None => self.more = Some(vec![item]),
            Some(more) => more.push(item),
        };
    }
}
