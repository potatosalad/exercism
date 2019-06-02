use std::fmt::Debug;

#[derive(Debug)]
pub struct CustomSet<T: Clone + Debug + PartialEq>(Vec<T>);

impl<T: Clone + Debug + PartialEq> CustomSet<T> {
    pub fn new(input: &[T]) -> Self {
        let mut set = Self::with_capacity(input.len());
        for element in input {
            set.add(element.clone());
        }
        set
    }

    pub fn empty() -> Self {
        CustomSet(Vec::new())
    }

    pub fn with_capacity(capacity: usize) -> Self {
        CustomSet(Vec::with_capacity(capacity))
    }

    pub fn contains(&self, element: &T) -> bool {
        self.0.contains(element)
    }

    pub fn add(&mut self, element: T) {
        if !self.contains(&element) {
            self.0.push(element);
        }
    }

    pub fn is_subset(&self, other: &Self) -> bool {
        self.0.iter().all(|element| other.contains(element))
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn is_disjoint(&self, other: &Self) -> bool {
        !self.0.iter().any(|element| other.contains(element))
    }

    pub fn intersection(&self, other: &Self) -> Self {
        let mut set = Self::empty();
        for element in self.0.iter() {
            if other.contains(element) {
                set.add(element.clone());
            }
        }
        set
    }

    pub fn difference(&self, other: &Self) -> Self {
        let mut set = Self::empty();
        for element in self.0.iter() {
            if !other.contains(element) {
                set.add(element.clone());
            }
        }
        set
    }

    pub fn union(&self, other: &Self) -> Self {
        let mut set = Self::empty();
        for element in self.0.iter().chain(other.0.iter()) {
            set.add(element.clone());
        }
        set
    }
}

impl<T: Clone + Debug + PartialEq> PartialEq for CustomSet<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &CustomSet<T>) -> bool {
        self.is_subset(other) && other.is_subset(self)
    }
}
