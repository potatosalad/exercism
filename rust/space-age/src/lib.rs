#[derive(Debug)]
pub struct Duration {
    seconds: f64,
}

impl From<u64> for Duration {
    fn from(s: u64) -> Self {
        Duration { seconds: s as f64 }
    }
}

pub trait Planet {
    fn year_in_earth_years() -> f64;
    fn years_during(d: &Duration) -> f64 {
        d.seconds / (31_557_600_f64 * Self::year_in_earth_years())
    }
}

macro_rules! planet_trait_impl {
    ($($name:ident => $y:expr,)+) => (planet_trait_impl!{$($name => $y),+});
    ($($name:ident => $y:expr),*) => ($(
        pub struct $name;
        impl Planet for $name {
            fn year_in_earth_years() -> f64 {
                $y
            }
        }
    )*)
}

planet_trait_impl! {
    Mercury => 0.240_846_7_f64,
    Venus => 0.615_197_26_f64,
    Earth => 1.0_f64,
    Mars => 1.880_815_8_f64,
    Jupiter => 11.862_615_f64,
    Saturn => 29.447_498_f64,
    Uranus => 84.016_846_f64,
    Neptune => 164.791_32_f64,
}
