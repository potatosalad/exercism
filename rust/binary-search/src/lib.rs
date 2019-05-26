use std::cmp::Ordering;

pub fn find<T, U>(array: U, key: T) -> Option<usize>
where
    T: Ord,
    U: AsRef<[T]>,
{
    let s = array.as_ref();
    if s.is_empty() {
        return None;
    }
    let mut size = s.len();
    let mut base = 0_usize;
    while size > 1 {
        let half = size / 2;
        let mid = base + half;
        let cmp = s.get(mid).map(|elem| elem.cmp(&key)).unwrap();
        base = if cmp == Ordering::Greater { base } else { mid };
        size -= half;
    }
    if let Some(Ordering::Equal) = s.get(base).map(|elem| elem.cmp(&key)) {
        Some(base)
    } else {
        None
    }
}
