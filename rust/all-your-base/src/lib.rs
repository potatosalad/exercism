#[derive(Debug, PartialEq)]
pub enum Error<T> {
    InvalidInputBase,
    InvalidOutputBase,
    InvalidDigit(T),
}

pub fn convert(digits: &[u32], from_base: u32, to_base: u32) -> Result<Vec<u32>, Error<u32>> {
    if from_base <= 1 {
        Err(Error::InvalidInputBase)
    } else if to_base <= 1 {
        Err(Error::InvalidOutputBase)
    } else {
        convert_from_base(digits, from_base).and_then(|number| Ok(convert_to_base(number, to_base)))
    }
}

fn convert_from_base(digits: &[u32], base: u32) -> Result<u32, Error<u32>> {
    let mut out: u32 = 0;
    for &digit in digits {
        if digit >= base {
            return Err(Error::InvalidDigit(digit));
        } else {
            out = out * base + digit;
        }
    }
    Ok(out)
}

fn convert_to_base(number: u32, base: u32) -> Vec<u32> {
    let mut n: u32 = number;
    let mut out: Vec<u32> = Vec::new();
    while n > 0 {
        out.insert(0, n % base);
        n = n / base;
    }
    out
}
