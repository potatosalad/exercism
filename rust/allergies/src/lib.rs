extern crate enum_iterator;

use enum_iterator::IntoEnumIterator;

pub struct Allergies(u32);

#[derive(Clone, Copy, Debug, IntoEnumIterator, PartialEq)]
pub enum Allergen {
    Eggs = 1,
    Peanuts = 1 << 1,
    Shellfish = 1 << 2,
    Strawberries = 1 << 3,
    Tomatoes = 1 << 4,
    Chocolate = 1 << 5,
    Pollen = 1 << 6,
    Cats = 1 << 7,
}

impl Allergies {
    pub fn new(score: u32) -> Self {
        Allergies(score)
    }

    pub fn is_allergic_to(&self, allergen: &Allergen) -> bool {
        let a = *allergen as u32;
        (self.0 & a) == a
    }

    pub fn allergies(&self) -> Vec<Allergen> {
        Allergen::into_enum_iter()
            .filter(|allergen| self.is_allergic_to(&allergen))
            .collect()
    }
}
