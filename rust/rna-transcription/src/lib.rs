#[derive(Debug, PartialEq)]
pub struct DNA {
    nucleotides: Vec<DNANucleotide>,
}

#[derive(Debug, PartialEq)]
pub struct RNA {
    nucleotides: Vec<RNANucleotide>,
}

impl DNA {
    pub fn new(dna: &str) -> Result<DNA, usize> {
        let mut nucleotides = Vec::new();
        for (i, c) in dna.chars().enumerate() {
            match c {
                'A' => nucleotides.push(DNANucleotide::A),
                'C' => nucleotides.push(DNANucleotide::C),
                'G' => nucleotides.push(DNANucleotide::G),
                'T' => nucleotides.push(DNANucleotide::T),
                _ => return Err(i),
            }
        }
        Ok(DNA { nucleotides })
    }

    pub fn into_rna(self) -> RNA {
        RNA {
            nucleotides: self.nucleotides.iter().map(|n| n.to_rna()).collect(),
        }
    }
}

impl RNA {
    pub fn new(rna: &str) -> Result<RNA, usize> {
        let mut nucleotides = Vec::new();
        for (i, c) in rna.chars().enumerate() {
            match c {
                'A' => nucleotides.push(RNANucleotide::A),
                'C' => nucleotides.push(RNANucleotide::C),
                'G' => nucleotides.push(RNANucleotide::G),
                'U' => nucleotides.push(RNANucleotide::U),
                _ => return Err(i),
            }
        }
        Ok(RNA { nucleotides })
    }
}

#[derive(Debug, PartialEq)]
pub enum DNANucleotide {
    A,
    C,
    G,
    T,
}

impl DNANucleotide {
    pub fn to_rna(&self) -> RNANucleotide {
        match self {
            DNANucleotide::A => RNANucleotide::U,
            DNANucleotide::C => RNANucleotide::G,
            DNANucleotide::G => RNANucleotide::C,
            DNANucleotide::T => RNANucleotide::A,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum RNANucleotide {
    A,
    C,
    G,
    U,
}

impl RNANucleotide {
    pub fn to_dna(&self) -> DNANucleotide {
        match self {
            RNANucleotide::U => DNANucleotide::A,
            RNANucleotide::G => DNANucleotide::C,
            RNANucleotide::C => DNANucleotide::G,
            RNANucleotide::A => DNANucleotide::T,
        }
    }
}
