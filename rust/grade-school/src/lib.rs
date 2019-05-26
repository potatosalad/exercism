use std::collections::BTreeMap;
use std::collections::BinaryHeap;

#[derive(Default)]
pub struct School {
    grades: BTreeMap<u32, BinaryHeap<String>>,
}

impl School {
    pub fn new() -> School {
        Default::default()
    }

    pub fn add(&mut self, grade: u32, student: &str) {
        self.grades
            .entry(grade)
            .and_modify(|students| students.push(student.to_string()))
            .or_insert_with(|| BinaryHeap::from(vec![student.to_string()]));
    }

    pub fn grades(&self) -> Vec<u32> {
        self.grades.keys().cloned().collect()
    }

    pub fn grade(&self, grade: u32) -> Option<Vec<String>> {
        self.grades
            .get(&grade)
            .map(|students| students.clone().into_sorted_vec())
    }
}
