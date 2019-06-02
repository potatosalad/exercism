#[derive(Debug, PartialEq)]
pub enum Error {
    SpanTooLong,
    InvalidDigit(char),
}

pub fn lsp(string_digits: &str, span: usize) -> Result<u64, Error> {
    if span > string_digits.len() {
        Err(Error::SpanTooLong)
    } else if span == 0 {
        Ok(1)
    } else {
        let input: Vec<char> = string_digits.chars().collect();
        let mut max_product: u64 = 0;
        for window in input.windows(span) {
            let mut product: u64 = 1;
            for c in window {
                if c.is_ascii_digit() {
                    product *= (*c as u64) - u64::from(b'0');
                } else {
                    return Err(Error::InvalidDigit(*c));
                }
            }
            if product > max_product {
                max_product = product;
            }
        }
        Ok(max_product)
    }
}
