use tc::token::{Token, Tokenizer};

fn main() {
    let str = "1 + 3".chars().collect::<Vec<_>>();

    let tokens = Tokenizer::new(str.as_slice()).collect::<Vec<_>>();

    assert_eq!(tokens, vec![Token::Num(1.0), Token::Add, Token::Num(3.0),]);
}
