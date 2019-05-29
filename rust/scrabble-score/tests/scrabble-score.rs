use scrabble_score::*;

#[test]
fn a_is_worth_one_point() {
    assert_eq!(score("a"), 1);
}

#[test]
#[ignore]
fn scoring_is_case_insensitive() {
    assert_eq!(score("A"), 1);
}

#[test]
#[ignore]
fn f_is_worth_four() {
    assert_eq!(score("f"), 4);
}

#[test]
#[ignore]
fn two_one_point_letters_make_a_two_point_word() {
    assert_eq!(score("at"), 2);
}

#[test]
#[ignore]
fn three_letter_word() {
    assert_eq!(score("zoo"), 12);
}

#[test]
#[ignore]
fn medium_word() {
    assert_eq!(score("street"), 6);
}

#[test]
#[ignore]
fn longer_words_with_valuable_letters() {
    assert_eq!(score("quirky"), 22);
}

#[test]
#[ignore]
fn long_mixed_case_word() {
    assert_eq!(score("OxyphenButazone"), 41);
}

#[test]
#[ignore]
fn non_english_scrabble_letters_do_not_score() {
    assert_eq!(score("pinata"), 8, "'n' should score 1");
    assert_eq!(score("piñata"), 7, "'ñ' should score 0");
}

#[test]
#[ignore]
fn empty_words_are_worth_zero() {
    assert_eq!(score(""), 0);
}

#[test]
#[ignore]
fn all_letters_work() {
    assert_eq!(score("abcdefghijklmnopqrstuvwxyz"), 87);
}

#[test]
#[ignore]
fn german_letters_do_not_score() {
    assert_eq!(score("STRASSE"), 7, "\"SS\" should score 2");
    assert_eq!(score("STRAßE"), 5, "'ß' should score 0");
}

#[test]
#[ignore]
fn complex_score_double_letters() {
    use ScrabbleSquare::*;
    let word = "cabbage";
    let squares = vec![SingleLetter; 7];
    assert_eq!(score_complex(word, &squares), Some(14));
    let mut squares = vec![SingleLetter; 7];
    squares[1] = DoubleLetter;
    assert_eq!(score_complex(word, &squares), Some(15));
    let mut squares = vec![SingleLetter; 7];
    squares[2] = DoubleLetter;
    squares[3] = DoubleLetter;
    assert_eq!(score_complex(word, &squares), Some(20));
}

#[test]
#[ignore]
fn complex_score_triple_letters() {
    use ScrabbleSquare::*;
    let word = "cabbage";
    let squares = vec![SingleLetter; 7];
    assert_eq!(score_complex(word, &squares), Some(14));
    let mut squares = vec![SingleLetter; 7];
    squares[1] = TripleLetter;
    assert_eq!(score_complex(word, &squares), Some(16));
    let mut squares = vec![SingleLetter; 7];
    squares[2] = TripleLetter;
    squares[3] = TripleLetter;
    assert_eq!(score_complex(word, &squares), Some(26));
}

#[test]
#[ignore]
fn complex_score_double_words() {
    use ScrabbleSquare::*;
    let word = "cabbage";
    let squares = vec![SingleLetter; 7];
    assert_eq!(score_complex(word, &squares), Some(14));
    let mut squares = vec![SingleLetter; 7];
    squares[1] = DoubleWord;
    assert_eq!(score_complex(word, &squares), Some(28));
    let mut squares = vec![SingleLetter; 7];
    squares[2] = DoubleWord;
    squares[3] = DoubleWord;
    assert_eq!(score_complex(word, &squares), Some(56));
}

#[test]
#[ignore]
fn complex_score_triple_words() {
    use ScrabbleSquare::*;
    let word = "cabbage";
    let squares = vec![SingleLetter; 7];
    assert_eq!(score_complex(word, &squares), Some(14));
    let mut squares = vec![SingleLetter; 7];
    squares[1] = TripleWord;
    assert_eq!(score_complex(word, &squares), Some(42));
    let mut squares = vec![SingleLetter; 7];
    squares[2] = TripleWord;
    squares[3] = TripleWord;
    assert_eq!(score_complex(word, &squares), Some(126));
}

#[test]
#[ignore]
fn complex_score_combined() {
    use ScrabbleSquare::*;
    let word = "cabbage";
    let squares = vec![
        SingleLetter,
        DoubleLetter,
        DoubleWord,
        SingleLetter,
        TripleLetter,
        TripleWord,
        SingleLetter,
    ];
    assert_eq!(score_complex(word, &squares), Some(102));
}
