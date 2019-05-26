use std::collections::HashMap;

/// Given a list of poker hands, return a list of those hands which win.
///
/// Note the type signature: this function should return _the same_ reference to
/// the winning hand(s) as were passed in, not reconstructed strings which happen to be equal.
pub fn winning_hands<'a>(hands: &[&'a str]) -> Option<Vec<&'a str>> {
    let mut hands: Vec<_> = hands.iter().map(|&s| Hand::from(s)).collect();
    hands.sort_by(|h1, h2| h2.rank.cmp(&h1.rank));
    Some(
        hands
            .iter()
            .take_while(|h| h.rank == hands[0].rank)
            .map(|h| h.orig)
            .collect(),
    )
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Suit {
    Club,
    Diamond,
    Heart,
    Spade,
}

impl<'a> From<&'a str> for Suit {
    fn from(s: &str) -> Self {
        match &s[s.len() - 1..] {
            "C" => Suit::Club,
            "D" => Suit::Diamond,
            "H" => Suit::Heart,
            "S" => Suit::Spade,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone)]
pub struct Card {
    value: u8,
    suit: Suit,
}

impl<'a> From<&'a str> for Card {
    fn from(s: &str) -> Self {
        let value: u8 = match &s[..s.len() - 1] {
            "A" => 14,
            "K" => 13,
            "Q" => 12,
            "J" => 11,
            "10" => 10,
            "9" => 9,
            "8" => 8,
            "7" => 7,
            "6" => 6,
            "5" => 5,
            "4" => 4,
            "3" => 3,
            "2" => 2,
            _ => unreachable!(),
        };
        let suit: Suit = Suit::from(s);
        Card { value, suit }
    }
}

pub struct Hand<'a> {
    orig: &'a str,
    rank: Rank,
}

impl<'a> From<&'a str> for Hand<'a> {
    fn from(orig: &'a str) -> Self {
        let cards = orig.split_whitespace().map(Card::from).collect::<Vec<_>>();
        let rank = Rank::new(&cards);
        Hand { orig, rank }
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Rank {
    HighCard(Vec<u8>),
    OnePair(Vec<u8>),
    TwoPair(Vec<u8>),
    ThreeOfAKind(Vec<u8>),
    Straight(u8),
    Flush(Vec<u8>),
    FullHouse(Vec<u8>),
    FourOfAKind(Vec<u8>),
    StraightFlush(u8),
}

impl Rank {
    pub fn new(cards: &[Card]) -> Self {
        let mut value_map: HashMap<u8, usize> = HashMap::new();
        let mut suit_map: HashMap<Suit, usize> = HashMap::new();
        for card in cards {
            value_map
                .entry(card.value)
                .and_modify(|v| *v += 1)
                .or_insert(1);
            suit_map
                .entry(card.suit)
                .and_modify(|s| *s += 1)
                .or_insert(1);
        }
        let mut values: Vec<(u8, usize)> = value_map.into_iter().collect();
        values.sort_by(|&(k1, v1), &(k2, v2)| v2.cmp(&v1).then(k2.cmp(&k1)));
        let mut suits: Vec<(Suit, usize)> = suit_map.into_iter().collect();
        suits.sort_by(|&(_, v1), &(_, v2)| v2.cmp(&v1));
        crate::rank::build(&values, &suits)
    }
}

mod rank {
    use crate::Rank;
    use crate::Suit;

    pub fn build(values: &[(u8, usize)], suits: &[(Suit, usize)]) -> Rank {
        self::straight_flush(values, suits)
            .or_else(|| self::four_of_a_kind(values))
            .or_else(|| self::full_house(values))
            .or_else(|| self::flush(values, suits))
            .or_else(|| self::straight(values))
            .or_else(|| self::three_of_a_kind(values))
            .or_else(|| self::two_pair(values))
            .or_else(|| self::one_pair(values))
            .unwrap_or_else(|| self::high_card(values))
    }

    fn straight_flush(values: &[(u8, usize)], suits: &[(Suit, usize)]) -> Option<Rank> {
        flush(values, suits).and_then(|_| {
            straight(values).and_then(|rank| match rank {
                Rank::Straight(value) => Some(Rank::StraightFlush(value)),
                _ => unreachable!(),
            })
        })
    }

    fn four_of_a_kind(values: &[(u8, usize)]) -> Option<Rank> {
        if card_counts(values) == [4, 1] {
            Some(Rank::FourOfAKind(card_values(values)))
        } else {
            None
        }
    }

    fn full_house(values: &[(u8, usize)]) -> Option<Rank> {
        if card_counts(values) == [3, 2] {
            Some(Rank::FullHouse(card_values(values)))
        } else {
            None
        }
    }

    fn flush(values: &[(u8, usize)], suits: &[(Suit, usize)]) -> Option<Rank> {
        if suits.len() == 1 {
            Some(Rank::Flush(card_values(values)))
        } else {
            None
        }
    }

    fn straight(values: &[(u8, usize)]) -> Option<Rank> {
        let lowest = values.last().unwrap().0;
        if values
            .iter()
            .rev()
            .enumerate()
            .skip(1)
            .all(|(i, c)| lowest + i as u8 == c.0)
        {
            Some(Rank::Straight(values[0].0))
        } else if values.iter().map(|c| c.0).collect::<Vec<_>>() == [14, 5, 4, 3, 2] {
            Some(Rank::Straight(5))
        } else {
            None
        }
    }

    fn three_of_a_kind(values: &[(u8, usize)]) -> Option<Rank> {
        if card_counts(values) == [3, 1, 1] {
            Some(Rank::ThreeOfAKind(card_values(values)))
        } else {
            None
        }
    }

    fn two_pair(values: &[(u8, usize)]) -> Option<Rank> {
        if card_counts(values) == [2, 2, 1] {
            Some(Rank::TwoPair(card_values(values)))
        } else {
            None
        }
    }

    fn one_pair(values: &[(u8, usize)]) -> Option<Rank> {
        if card_counts(values) == [2, 1, 1, 1] {
            Some(Rank::OnePair(card_values(values)))
        } else {
            None
        }
    }

    fn high_card(values: &[(u8, usize)]) -> Rank {
        Rank::HighCard(card_values(values))
    }

    #[inline]
    fn card_counts(values: &[(u8, usize)]) -> Vec<usize> {
        values.iter().map(|v| v.1).collect()
    }

    #[inline]
    fn card_values(values: &[(u8, usize)]) -> Vec<u8> {
        values.iter().map(|v| v.0).collect()
    }
}
