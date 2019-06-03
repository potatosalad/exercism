use std::collections::VecDeque;

#[derive(Clone, Debug)]
pub enum Operation {
    Literal(i32),
    Plus,
    Minus,
    Multiply,
    Divide,
    Power,
    Ignore,
}

impl Operation {
    pub fn parse(input: &str) -> Option<Self> {
        if input.is_empty() {
            None
        } else if input.starts_with('-') || input.chars().nth(0).unwrap().is_ascii_digit() {
            let mut n = input;
            if n.ends_with("nd") || n.ends_with("rd") || n.ends_with("st") {
                n = &n[0..(n.len() - 2)];
            } else if n.ends_with('?') {
                n = &n[0..(n.len() - 1)];
            }
            if let Ok(number) = n.parse::<i32>() {
                Some(Operation::Literal(number))
            } else {
                None
            }
        } else if input == "plus" {
            Some(Operation::Plus)
        } else if input == "minus" {
            Some(Operation::Minus)
        } else if input == "multiplied_by" {
            Some(Operation::Multiply)
        } else if input == "divided_by" {
            Some(Operation::Divide)
        } else if input == "raised_to_the" {
            Some(Operation::Power)
        } else if input == "power" || input == "power?" {
            Some(Operation::Ignore)
        } else {
            None
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct WordProblem {
    queue: VecDeque<Operation>,
}

impl WordProblem {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn parse(command: &str) -> Option<Self> {
        if command.len() < 8 {
            None
        } else {
            let mut word_problem = Self::new();
            let input = (&command[8..]).to_string();
            let input = input.replace("divided by", "divided_by");
            let input = input.replace("multiplied by", "multiplied_by");
            let input = input.replace("raised to the", "raised_to_the");
            for token in input.split_whitespace() {
                if let Some(operation) = Operation::parse(token) {
                    word_problem.push_back(operation);
                } else {
                    return None;
                }
            }
            if word_problem.is_empty() {
                None
            } else {
                Some(word_problem)
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }

    pub fn pop_back(&mut self) -> Option<Operation> {
        self.queue.pop_back()
    }

    pub fn pop_front(&mut self) -> Option<Operation> {
        self.queue.pop_front()
    }

    pub fn push_back(&mut self, operation: Operation) {
        self.queue.push_back(operation);
    }

    pub fn push_front(&mut self, operation: Operation) {
        self.queue.push_front(operation);
    }

    pub fn answer(&self) -> Option<i32> {
        let mut word_problem = self.clone();
        while let Some(operation) = word_problem.pop_front() {
            match operation {
                Operation::Literal(n1) => match word_problem.pop_front() {
                    Some(Operation::Plus) => {
                        if let Some(Operation::Literal(n2)) = word_problem.pop_front() {
                            word_problem.push_front(Operation::Literal(n1 + n2));
                            continue;
                        } else {
                            return None;
                        }
                    }
                    Some(Operation::Minus) => {
                        if let Some(Operation::Literal(n2)) = word_problem.pop_front() {
                            word_problem.push_front(Operation::Literal(n1 - n2));
                            continue;
                        } else {
                            return None;
                        }
                    }
                    Some(Operation::Multiply) => {
                        if let Some(Operation::Literal(n2)) = word_problem.pop_front() {
                            word_problem.push_front(Operation::Literal(n1 * n2));
                            continue;
                        } else {
                            return None;
                        }
                    }
                    Some(Operation::Divide) => {
                        if let Some(Operation::Literal(n2)) = word_problem.pop_front() {
                            word_problem.push_front(Operation::Literal(n1 / n2));
                            continue;
                        } else {
                            return None;
                        }
                    }
                    Some(Operation::Power) => {
                        if let Some(Operation::Literal(n2)) = word_problem.pop_front() {
                            word_problem.push_front(Operation::Literal(n1.pow(n2 as u32)));
                            continue;
                        } else {
                            return None;
                        }
                    }
                    Some(Operation::Literal(_)) => return None,
                    Some(Operation::Ignore) => return None,
                    None => return Some(n1),
                },
                Operation::Ignore => continue,
                _ => return None,
            }
        }
        None
    }
}

pub fn answer(command: &str) -> Option<i32> {
    WordProblem::parse(command).and_then(|word_problem| word_problem.answer())
}
