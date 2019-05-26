#[derive(PartialEq, Debug)]
pub enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    pub fn turn_right(self) -> Self {
        use Direction::*;
        match self {
            North => East,
            East => South,
            South => West,
            West => North,
        }
    }

    pub fn turn_left(self) -> Self {
        use Direction::*;
        match self {
            North => West,
            East => North,
            South => East,
            West => South,
        }
    }
}

pub struct Robot {
    d: Direction,
    x: i32,
    y: i32,
}

impl Robot {
    pub fn new(x: i32, y: i32, d: Direction) -> Self {
        Robot { d, x, y }
    }

    pub fn turn_right(self) -> Self {
        Self::new(self.x, self.y, self.d.turn_right())
    }

    pub fn turn_left(self) -> Self {
        Self::new(self.x, self.y, self.d.turn_left())
    }

    pub fn advance(self) -> Self {
        use Direction::*;
        let (x, y) = match self.d {
            North => (self.x, self.y + 1),
            East => (self.x + 1, self.y),
            South => (self.x, self.y - 1),
            West => (self.x - 1, self.y),
        };
        Self::new(x, y, self.d)
    }

    pub fn instructions(self, instructions: &str) -> Self {
        instructions.chars().fold(self, |robot, c| match c {
            'A' => robot.advance(),
            'L' => robot.turn_left(),
            'R' => robot.turn_right(),
            _ => panic!("instructions may only contain 'A', 'L', and 'R' characters"),
        })
    }

    pub fn position(&self) -> (i32, i32) {
        (self.x, self.y)
    }

    pub fn direction(&self) -> &Direction {
        &self.d
    }
}
