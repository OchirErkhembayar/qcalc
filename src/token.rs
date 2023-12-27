#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Token {
    Num(f64),
    Div,
    Mult,
    Plus,
    Minus,
    RParen,
    LParen,
    Eoe,
}

pub struct Tokenizer<'a> {
    input: &'a [char],
    index: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a [char]) -> Self {
        Self { input, index: 0 }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.input.get(self.index) {
            self.index += 1;
            let token = match next {
                ' ' => {
                    return self.next();
                }
                '/' => Token::Div,
                '+' => Token::Plus,
                '-' => Token::Minus,
                '%' => Token::Div,
                '*' => Token::Mult,
                '(' => Token::LParen,
                ')' => Token::RParen,
                '0'..='9' => {
                    let mut num = next.to_string();
                    while self
                        .input
                        .get(self.index)
                        .is_some_and(|c| c.is_ascii_digit())
                    {
                        num.push(self.input[self.index]);
                        self.index += 1;
                    }
                    if self.input.get(self.index).is_some_and(|c| *c == '.') {
                        num.push(self.input[self.index]);
                        self.index += 1;
                        while self
                            .input
                            .get(self.index)
                            .is_some_and(|c| c.is_ascii_digit())
                        {
                            num.push(self.input[self.index]);
                            self.index += 1;
                        }
                    }
                    Token::Num(num.parse().unwrap())
                }
                _ => return None,
            };
            Some(token)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let str = "1.3+3.2".chars().collect::<Vec<_>>();
        let tokens = Tokenizer::new(str.as_slice()).collect::<Vec<_>>();
        assert_eq!(tokens, vec![Token::Num(1.3), Token::Plus, Token::Num(3.2),]);
    }

    #[test]
    fn divide() {
        let str = "13/32".chars().collect::<Vec<_>>();
        let tokens = Tokenizer::new(str.as_slice()).collect::<Vec<_>>();
        assert_eq!(
            tokens,
            vec![Token::Num(13.0), Token::Div, Token::Num(32.0),]
        );

        let str = "13.5/32.2".chars().collect::<Vec<_>>();
        let tokens = Tokenizer::new(str.as_slice()).collect::<Vec<_>>();
        assert_eq!(
            tokens,
            vec![Token::Num(13.5), Token::Div, Token::Num(32.2),]
        );
    }

    #[test]
    fn test_space() {
        let str = "13.5    + 12.5 *     30.5*10".chars().collect::<Vec<_>>();
        let tokens = Tokenizer::new(str.as_slice()).collect::<Vec<_>>();
        assert_eq!(
            tokens,
            vec![
                Token::Num(13.5),
                Token::Plus,
                Token::Num(12.5),
                Token::Mult,
                Token::Num(30.5),
                Token::Mult,
                Token::Num(10.0)
            ]
        );
    }
}
