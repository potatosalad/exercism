#[derive(Debug, PartialEq)]
pub struct Clock {
    minutes: i32,
}

const MINUTES_IN_DAY: i32 = 24 * 60;

fn modulo(b: i32, m: i32) -> i32 {
    (b % m + m) % m
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        Clock {
            minutes: modulo(minutes + hours * 60, MINUTES_IN_DAY),
        }
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        Clock {
            minutes: modulo(self.minutes + minutes, MINUTES_IN_DAY),
        }
    }
}

impl std::fmt::Display for Clock {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:02}:{:02}", self.minutes / 60, self.minutes % 60)
    }
}

impl From<Clock> for String {
    fn from(clock: Clock) -> String {
        clock.to_string()
    }
}

#[test]
fn test_string_from() {
    assert_eq!(String::from(Clock::new(12, 34)), "12:34")
}
