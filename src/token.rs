use crate::inner_write;

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Num(f64),
    Fn,
    Comma,
    Ident(String),
    Let,
    Undef,
    Eq,
    Pipe,
    Mod,
    Div,
    Mult,
    Plus,
    Minus,
    RParen,
    LParen,
    Power,
    Eoe,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Num(num) => inner_write(num, f),
            Token::Fn => inner_write("fn", f),
            Token::Undef => inner_write("undef", f),
            Token::Comma => inner_write(',', f),
            Token::Ident(ident) => inner_write(ident, f),
            Token::Let => inner_write("let", f),
            Token::Eq => inner_write("=", f),
            Token::Pipe => inner_write('|', f),
            Token::Mod => inner_write('%', f),
            Token::Mult => inner_write('*', f),
            Token::Div => inner_write('/', f),
            Token::Plus => inner_write('+', f),
            Token::Minus => inner_write('-', f),
            Token::LParen => inner_write('(', f),
            Token::RParen => inner_write(')', f),
            Token::Power => inner_write('^', f),
            Token::Eoe => inner_write("", f),
        }
    }
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
    input: &'a [char],
    index: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a [char]) -> Self {
        Self { input, index: 0 }
    }

    pub fn into_tokens(self) -> Vec<Token> {
        let mut tokens = self.collect::<Vec<_>>();
        tokens.push(Token::Eoe);
        tokens
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.input.get(self.index)?;
        self.index += 1;
        Some(match next {
            ' ' => {
                return self.next();
            }
            ',' => Token::Comma,
            '=' => Token::Eq,
            '|' => Token::Pipe,
            '/' => Token::Div,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '%' => Token::Mod,
            '*' => Token::Mult,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '^' => Token::Power,
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
            'A'..='Z' | 'a'..='z' => {
                let mut func = next.to_string();
                while self
                    .input
                    .get(self.index)
                    .is_some_and(|c| c.is_ascii_alphabetic())
                {
                    func.push(self.input[self.index]);
                    self.index += 1;
                }
                match func.as_str() {
                    "fn" => Token::Fn,
                    "let" => Token::Let,
                    "undef" => Token::Undef,
                    _ => Token::Ident(func),
                }
            }
            _ => return None, // Unknown chars just end the parsing. Not sure if good or
                              // bad... In any case it's probably a bit confusing for the user. Definitely
                              // confused me lol.
        })
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
