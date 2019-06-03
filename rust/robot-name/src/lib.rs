#[derive(Default)]
pub struct Robot {
    name: String,
    bits: u64,
    left: usize,
}

impl Robot {
    pub fn new() -> Self {
        let mut robot: Self = Default::default();
        robot.reset_name();
        robot
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn reset_name(&mut self) {
        self.name = self.random_name();
    }

    fn random_name(&mut self) -> String {
        format!(
            "{}{}{}{}{}",
            self.random_letter(),
            self.random_letter(),
            self.random_digit(),
            self.random_digit(),
            self.random_digit()
        )
    }

    fn random_digit(&mut self) -> u8 {
        self.random_bits(4) % 9
    }

    fn random_letter(&mut self) -> char {
        ((self.random_bits(5) % 26) + b'A') as char
    }

    fn random_bits(&mut self, n: usize) -> u8 {
        assert!(n != 0 && n <= 8);
        let mut x: u8 = 0;
        let mut l: usize = n;
        while l != 0 {
            if self.left == 0 {
                self.bits = Self::random();
                self.left = 64;
            }
            x = (x << 1) | ((self.bits & 1) as u8);
            l -= 1;
            self.bits >>= 1;
            self.left -= 1;
        }
        x
    }

    fn random() -> u64 {
        let mut x: u64 = 0;
        unsafe {
            while core::arch::x86_64::_rdrand64_step(&mut x) == 0 || x == 0 {
                core::arch::x86_64::_mm_pause();
            }
        }
        x
    }
}
