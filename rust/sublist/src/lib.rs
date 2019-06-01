#[derive(Debug, PartialEq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

pub fn sublist<T: PartialEq>(first_list: &[T], second_list: &[T]) -> Comparison {
    if first_list.len() > second_list.len() {
        match sublist(second_list, first_list) {
            Comparison::Sublist => Comparison::Superlist,
            comparison => comparison,
        }
    } else {
        for i in 0..=(second_list.len() - first_list.len()) {
            if second_list[i..(i + first_list.len())] == first_list[..] {
                if first_list.len() == second_list.len() {
                    return Comparison::Equal;
                } else {
                    return Comparison::Sublist;
                }
            }
        }
        Comparison::Unequal
    }
}
