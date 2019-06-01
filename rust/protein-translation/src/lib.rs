use std::collections::BTreeMap;

pub struct CodonsInfo<'a>(BTreeMap<&'a str, &'a str>);

impl<'a> CodonsInfo<'a> {
    pub fn name_for(&self, codon: &str) -> Option<&'a str> {
        self.0.get(codon).cloned()
    }

    pub fn of_rna(&self, rna: &str) -> Option<Vec<&'a str>> {
        let mut buffer = rna;
        let mut names: Vec<&'a str> = Vec::new();
        while let Some(name) = self.name_for(&buffer[..3]) {
            if name == "stop codon" {
                break;
            }
            buffer = &buffer[3..];
            names.push(name);
            if buffer.len() < 3 {
                break;
            }
        }
        if names.is_empty() {
            None
        } else {
            Some(names)
        }
    }
}

pub fn parse<'a>(pairs: Vec<(&'a str, &'a str)>) -> CodonsInfo<'a> {
    let mut btree: BTreeMap<&'a str, &'a str> = BTreeMap::new();
    for (codon, name) in pairs {
        btree.insert(codon, name);
    }
    CodonsInfo(btree)
}
