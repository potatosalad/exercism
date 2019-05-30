#[derive(Debug, PartialEq)]
pub enum Error {
    IncompleteNumber,
    Overflow,
}

pub fn to_bytes(values: &[u32]) -> Vec<u8> {
    values
        .iter()
        .flat_map(|&value| match value {
            0x00...0x7F => vec![value as u8],
            _ => {
                let mut part = value;
                let mut mask = 0x0;
                let mut bytes = Vec::new();
                while part != 0 || mask == 0x0 {
                    let byte = part & !0x80 | mask;
                    mask = 0x80;
                    part >>= 7;
                    bytes.insert(0, byte as u8);
                }
                bytes
            }
        })
        .collect()
}

pub fn from_bytes(bytes: &[u8]) -> Result<Vec<u32>, Error> {
    let mut values: Vec<u32> = vec![0];
    let last_byte = bytes.len() - 1;
    for (i, &byte) in bytes.iter().enumerate() {
        if let Some(num) = values.last_mut() {
            if *num >= (std::u32::MAX >> 4) {
                return Err(Error::Overflow);
            }
            *num = (*num << 7) | u32::from(byte & 0x7F);
        }
        if byte & 0x80 == 0x00 && i != last_byte {
            values.push(0);
        } else if byte & 0x80 != 0x00 && i == last_byte {
            return Err(Error::IncompleteNumber);
        }
    }
    Ok(values)
}
