use std::cmp::Ordering;
use std::collections::HashMap;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Cursor;
use std::iter::FromIterator;

pub fn tally(match_results: &str) -> String {
    let reader = BufReader::new(Cursor::new(match_results));
    let mut table: HashMap<String, TeamResult> = HashMap::new();
    for line in reader.lines() {
        let line = line.unwrap();
        let (team1, team2, result) = match line.split(';').collect::<Vec<&str>>()[..] {
            [v0, v1, v2] => (v0, v1, v2),
            _ => unreachable!(),
        };
        table
            .entry(team1.to_string())
            .or_insert_with(|| TeamResult::new(team1));
        table
            .entry(team2.to_string())
            .or_insert_with(|| TeamResult::new(team2));
        match result {
            "win" => {
                table.get_mut(team1).unwrap().win += 1;
                table.get_mut(team2).unwrap().loss += 1;
            }
            "draw" => {
                table.get_mut(team1).unwrap().draw += 1;
                table.get_mut(team2).unwrap().draw += 1;
            }
            "loss" => {
                table.get_mut(team1).unwrap().loss += 1;
                table.get_mut(team2).unwrap().win += 1;
            }
            _ => (),
        }
    }
    table
        .into_iter()
        .map(|v| v.1)
        .collect::<TeamResults>()
        .to_string()
}

#[derive(Clone, Debug)]
struct TeamResult {
    name: String,
    win: u64,
    draw: u64,
    loss: u64,
}

impl TeamResult {
    pub fn new(name: &str) -> TeamResult {
        TeamResult {
            name: name.to_string(),
            win: 0,
            draw: 0,
            loss: 0,
        }
    }

    fn points(&self) -> u64 {
        self.win * 3 + self.draw
    }

    fn played_matches(&self) -> u64 {
        self.win + self.draw + self.loss
    }
}

#[derive(Clone, Debug)]
struct TeamResults(Vec<TeamResult>);

impl FromIterator<TeamResult> for TeamResults {
    fn from_iter<I: IntoIterator<Item = TeamResult>>(iter: I) -> Self {
        let mut vec: Vec<TeamResult> = iter.into_iter().collect();
        vec.sort_by(|team1, team2| match team2.points().cmp(&team1.points()) {
            Ordering::Equal => team1.name.cmp(&team2.name),
            ordering => ordering,
        });
        TeamResults(vec)
    }
}

impl ToString for TeamResults {
    fn to_string(&self) -> String {
        std::iter::once(format!(
            "{:<31}|{:>3} |{:>3} |{:>3} |{:>3} |{:>3}",
            "Team", "MP", "W", "D", "L", "P"
        ))
        .chain(self.0.iter().map(|team| {
            format!(
                "{:<31}|{:>3} |{:>3} |{:>3} |{:>3} |{:>3}",
                team.name,
                team.played_matches(),
                team.win,
                team.draw,
                team.loss,
                team.points()
            )
        }))
        .collect::<Vec<String>>()
        .join("\n")
    }
}
