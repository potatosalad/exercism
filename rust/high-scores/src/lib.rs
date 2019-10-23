#[derive(Debug)]
pub struct HighScores<'a>(&'a [u32]);

impl<'a> HighScores<'a> {
    pub fn new(scores: &'a [u32]) -> Self {
        Self(scores)
    }

    pub fn scores(&'a self) -> &'a [u32] {
        self.0
    }

    pub fn latest(&self) -> Option<u32> {
        self.0.last().cloned()
    }

    pub fn personal_best(&self) -> Option<u32> {
        self.0.iter().max().cloned()
    }

    pub fn personal_top_three(&self) -> Vec<u32> {
        let mut scores = self.0.to_vec();
        scores.sort_unstable();
        scores.reverse();
        scores.into_iter().take(3).collect()
    }
}
