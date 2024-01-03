use crate::inner_write;

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Num(f64),
    Var(char), // Change this to a string and combine functions and vars into one table and
    // dynamimcally use them
    // This must go bye-bye and become an Ident
    Fn,
    Ident(String),
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
            Token::Var(var) => inner_write(var, f),
            Token::Fn => inner_write("fn", f),
            Token::Ident(ident) => inner_write(ident, f),
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
                    if func.len() == 1 {
                        Token::Var(*next)
                    } else if func == "fn" {
                        Token::Fn
                    } else {
                        Token::Ident(func)
                    }
                }
                _ => return None, // Handle wrong tokens a different way
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

    #[test]
    fn test_var() {
        let str = "a + 3".chars().collect::<Vec<_>>();
        let tokens = Tokenizer::new(str.as_slice()).collect::<Vec<_>>();
        assert_eq!(tokens, vec![Token::Var('a'), Token::Plus, Token::Num(3.0)]);
    }
}
