pub fn is_valid_isbn(isbn: &str) -> bool {
    match get_isbn(isbn) {
        Some(digits) => match digits.len() {
            10 => {
                modulo(
                    digits
                        .into_iter()
                        .rev()
                        .enumerate()
                        .map(|(i, x)| (i as u32 + 1) * x)
                        .sum::<u32>(),
                    11,
                ) == 0
            }
            13 => {
                modulo(
                    digits
                        .into_iter()
                        .zip([1_u32, 3].iter().cycle())
                        .map(|(i, x)| i * x)
                        .sum::<u32>(),
                    10,
                ) == 0
            }
            _ => false,
        },
        None => false,
    }
}

pub fn isbn_10_to_isbn_13(isbn: &str) -> Option<String> {
    match get_isbn(isbn) {
        Some(mut digits) => match digits.len() {
            10 => {
                digits.pop().unwrap();
                digits.insert(0, 8);
                digits.insert(0, 7);
                digits.insert(0, 9);
                let check_digit = modulo(
                    digits
                        .clone()
                        .into_iter()
                        .zip([1_u32, 3].iter().cycle())
                        .map(|(i, x)| i * x)
                        .sum::<u32>(),
                    10,
                );
                digits.push(check_digit);
                Some(
                    digits
                        .into_iter()
                        .map(|x| x.to_string())
                        .collect::<String>(),
                )
            }
            13 => Some(
                digits
                    .into_iter()
                    .map(|x| x.to_string())
                    .collect::<String>(),
            ),
            _ => None,
        },
        None => None,
    }
}

fn get_isbn(isbn: &str) -> Option<Vec<u32>> {
    if isbn.is_empty()
        || isbn.len() < 10
        || !isbn
            .chars()
            .all(|c| c.is_digit(10) || c == '-' || c == 'X' || c == 'x')
    {
        None
    } else {
        let digits = isbn
            .chars()
            .filter_map(|c| {
                if c.is_digit(10) {
                    Some(c.to_digit(10).unwrap())
                } else if c == 'x' || c == 'X' {
                    Some(10)
                } else {
                    None
                }
            })
            .collect::<Vec<u32>>();
        let mut ds = if digits.len() == 10 {
            digits[..digits.len() - 1].iter()
        } else {
            digits[..].iter()
        };
        if ds.any(|&x| x > 9) {
            None
        } else {
            Some(digits)
        }
    }
}

fn modulo<T>(b: T, m: T) -> T
where
    T: Copy + std::ops::Add<Output = T> + std::ops::Rem<Output = T>,
{
    (b + b % m) % m
}
