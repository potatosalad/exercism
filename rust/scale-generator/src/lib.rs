const SHARP: [&str; 14] = [
    "G", "D", "A", "E", "B", "F#", "e", "b", "f#", "c#", "g#", "d#", "C", "a",
];
const FLAT: [&str; 12] = [
    "F", "Bb", "Eb", "Ab", "Db", "Gb", "d", "g", "c", "f", "bb", "eb",
];
const SHARP_SCALE: [&str; 12] = [
    "A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#",
];
const FLAT_SCALE: [&str; 12] = [
    "A", "Bb", "B", "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab",
];
const STEPS: [char; 3] = ['m', 'M', 'A'];

#[derive(Debug)]
pub enum Error {
    InvalidTonic,
    InvalidInterval,
}

#[derive(Clone, Debug, Default)]
pub struct Scale {
    notes: Vec<String>,
}

impl Scale {
    pub fn new(tonic: &str, intervals: &str) -> Result<Scale, Error> {
        Ok(Scale {
            notes: Self::generate_notes(tonic, intervals)?,
        })
    }

    pub fn chromatic(tonic: &str) -> Result<Scale, Error> {
        Self::new(tonic, "mmmmmmmmmmmm")
    }

    pub fn enumerate(&self) -> Vec<String> {
        self.notes.clone()
    }

    fn get_interval(interval: char) -> Result<usize, Error> {
        STEPS
            .iter()
            .position(|&step| step == interval)
            .map(|i| i + 1)
            .ok_or(Error::InvalidInterval)
    }

    fn get_scale(tonic: &str) -> Result<&[&'static str], Error> {
        if SHARP.contains(&tonic) {
            Ok(&SHARP_SCALE)
        } else if FLAT.contains(&tonic) {
            Ok(&FLAT_SCALE)
        } else {
            Err(Error::InvalidTonic)
        }
    }

    fn generate_notes(tonic: &str, intervals: &str) -> Result<Vec<String>, Error> {
        let scale = Self::get_scale(tonic)?;
        let mut shift = scale
            .iter()
            .position(|&note| note.to_lowercase() == tonic.to_lowercase())
            .unwrap();
        Ok(intervals
            .chars()
            .map(|interval| {
                let note = scale[shift % 12].to_string();
                shift += Self::get_interval(interval)?;
                Ok(note)
            })
            .collect::<Result<_, _>>()?)
    }
}
