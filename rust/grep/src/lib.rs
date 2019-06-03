#[macro_use]
extern crate bitflags;
extern crate failure;

use failure::Error;
use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

bitflags! {
    pub struct Flags: u32 {
        const PRINT_LINE_NUMBER      = 0b0000_0001;
        const PRINT_FILE_NAME_ONLY   = 0b0000_0010;
        const MATCH_CASE_INSENSITIVE = 0b0000_0100;
        const MATCH_INVERT           = 0b0000_1000;
        const MATCH_ENTIRE_LINE      = 0b0001_0000;
    }
}

impl Flags {
    pub fn new(flags: &[&str]) -> Self {
        flags
            .iter()
            .map(|&flag| match flag {
                "-n" => Self::PRINT_LINE_NUMBER,
                "-l" => Self::PRINT_FILE_NAME_ONLY,
                "-i" => Self::MATCH_CASE_INSENSITIVE,
                "-v" => Self::MATCH_INVERT,
                "-x" => Self::MATCH_ENTIRE_LINE,
                _ => panic!("unrecognized flag: {:?}", flag),
            })
            .fold(Self::empty(), |f1, f2| f1 | f2)
    }
}

pub fn grep(pattern: &str, flags: &Flags, files: &[&str]) -> Result<Vec<String>, Error> {
    let needle = if flags.contains(Flags::MATCH_CASE_INSENSITIVE) {
        pattern.to_lowercase()
    } else {
        pattern.to_string()
    };
    let mut file_matches = HashSet::new();
    let mut matches: Vec<String> = Vec::new();
    for file in files {
        let file_handle = File::open(file)?;
        let buf_reader = BufReader::new(file_handle);
        let prefix = if files.len() == 1 {
            String::new()
        } else {
            format!("{}:", file)
        };
        for (number, line) in buf_reader.lines().enumerate() {
            let line = line?;
            if grep_line(&line, &needle, flags) != flags.contains(Flags::MATCH_INVERT) {
                if flags.contains(Flags::PRINT_FILE_NAME_ONLY) {
                    if !file_matches.contains(file) {
                        matches.push(file.to_string());
                    }
                } else if flags.contains(Flags::PRINT_LINE_NUMBER) {
                    matches.push(format!("{}{}:{}", prefix, number + 1, line));
                } else {
                    matches.push(format!("{}{}", prefix, line));
                }
                file_matches.insert(file);
            }
        }
    }
    Ok(matches)
}

fn grep_line(line: &str, needle: &str, flags: &Flags) -> bool {
    let haystack = if flags.contains(Flags::MATCH_CASE_INSENSITIVE) {
        line.to_lowercase()
    } else {
        line.to_string()
    };
    if flags.contains(Flags::MATCH_ENTIRE_LINE) {
        haystack == needle
    } else {
        haystack.contains(needle)
    }
}
