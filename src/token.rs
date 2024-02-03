use core::iter::Peekable;
use std::str::Chars;

use crate::inner_write;

const LET: &str = "let";
const UNDEF: &str = "undef";
const TRUE: &str = "true";
const FALSE: &str = "false";
const IF: &str = "if";
const THEN: &str = "then";
const ELSE: &str = "else";

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Float(f64),
    Int(i64),
    Comma,
    Ident(String),
    Let,
    Undef,
    Assign,
    Pipe,
    Mod,
    Div,
    Mult,
    Plus,
    Minus,
    Pow,
    Not,
    RParen,
    LParen,
    BitAnd,
    BitXor,
    Shr,
    Shl,
    True,
    False,
    Eq,
    Ne,
    Gt,
    Gte,
    Lt,
    Lte,
    Or,
    And,
    If,
    Then,
    Else,
    String(String),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Float(float) => inner_write(float, f),
            Token::Int(int) => inner_write(int, f),
            Token::String(string) => inner_write(format!("\"{}\"", string), f),
            Token::Undef => inner_write(UNDEF, f),
            Token::Comma => inner_write(',', f),
            Token::Ident(ident) => inner_write(ident, f),
            Token::Let => inner_write(LET, f),
            Token::Assign => inner_write('=', f),
            Token::Mod => inner_write('%', f),
            Token::Mult => inner_write('*', f),
            Token::Div => inner_write('/', f),
            Token::Plus => inner_write('+', f),
            Token::Minus => inner_write('-', f),
            Token::Pow => inner_write("**", f),
            Token::Not => inner_write('!', f),
            Token::Pipe => inner_write('|', f),
            Token::BitAnd => inner_write('&', f),
            Token::BitXor => inner_write('^', f),
            Token::LParen => inner_write('(', f),
            Token::RParen => inner_write(')', f),
            Token::Shr => inner_write(">>", f),
            Token::Shl => inner_write("<<", f),
            Token::True => inner_write(TRUE, f),
            Token::False => inner_write(FALSE, f),
            Token::Eq => inner_write("==", f),
            Token::Ne => inner_write("!=", f),
            Token::Gt => inner_write(">", f),
            Token::Gte => inner_write(">=", f),
            Token::Lt => inner_write("<", f),
            Token::Lte => inner_write("<=", f),
            Token::Or => inner_write("||", f),
            Token::And => inner_write("&&", f),
            Token::If => inner_write(IF, f),
            Token::Then => inner_write(THEN, f),
            Token::Else => inner_write(ELSE, f),
        }
    }
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: Peekable<Chars<'a>>) -> Self {
        Self { input }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.input.next()?;
        Some(match next {
            ' ' => {
                while self.input.peek().is_some_and(|c| c.is_whitespace()) {
                    self.input.next();
                }
                return self.next();
            }
            '>' => match self.input.peek() {
                Some('>') => {
                    self.input.next();
                    Token::Shr
                }
                Some('=') => {
                    self.input.next();
                    Token::Gte
                }
                _ => Token::Gt,
            },
            '<' => match self.input.peek() {
                Some('<') => {
                    self.input.next();
                    Token::Shl
                }
                Some('=') => {
                    self.input.next();
                    Token::Lte
                }
                _ => Token::Lt,
            },
            '=' => match self.input.peek() {
                Some('=') => {
                    self.input.next();
                    Token::Eq
                }
                _ => Token::Assign,
            },
            '!' => match self.input.peek() {
                Some('=') => {
                    self.input.next();
                    Token::Ne
                }
                _ => Token::Not,
            },
            '&' => match self.input.peek() {
                Some('&') => {
                    self.input.next();
                    Token::And
                }
                _ => Token::BitAnd,
            },
            '^' => Token::BitXor,
            '|' => match self.input.peek() {
                Some('|') => {
                    self.input.next();
                    Token::Or
                }
                _ => Token::Pipe,
            },
            ',' => Token::Comma,
            '/' => Token::Div,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '%' => Token::Mod,
            '*' => {
                if self.input.peek().is_some_and(|c| *c == '*') {
                    self.input.next().unwrap();
                    Token::Pow
                } else {
                    Token::Mult
                }
            }
            '(' => Token::LParen,
            ')' => Token::RParen,
            '0'..='9' => {
                // Check if it's hex
                if next == '0'
                    && self
                        .input
                        .peek()
                        .is_some_and(|c| matches!(c, 'x' | 'X' | 'b' | 'B'))
                {
                    match self.input.next().unwrap() {
                        'x' | 'X' => {
                            let mut hex = String::new();
                            while self.input.peek().is_some_and(|c| {
                                c.is_numeric() || matches!(c, 'a'..='f' | 'A'..='F')
                            }) {
                                hex.push(self.input.next().unwrap());
                            }
                            Token::Int(i64::from_str_radix(&hex, 16).unwrap())
                        }
                        'b' | 'B' => {
                            let mut hex = String::new();
                            while self
                                .input
                                .peek()
                                .is_some_and(|c| c.is_numeric() || matches!(c, '0' | '1'))
                            {
                                hex.push(self.input.next().unwrap());
                            }
                            Token::Int(i64::from_str_radix(&hex, 2).unwrap())
                        }
                        _ => unreachable!(),
                    }
                } else {
                    let mut num = next.to_string();
                    while self.input.peek().is_some_and(|c| c.is_ascii_digit()) {
                        num.push(self.input.next().unwrap());
                    }
                    if self.input.peek().is_some_and(|c| *c == '.') {
                        num.push(self.input.next().unwrap());
                        while self.input.peek().is_some_and(|c| c.is_ascii_digit()) {
                            num.push(self.input.next().unwrap());
                        }
                        Token::Float(num.parse().unwrap())
                    } else {
                        Token::Int(num.parse().unwrap())
                    }
                }
            }
            '"' => {
                let mut string = String::new();
                while self.input.peek().is_some_and(|c| *c != '"') {
                    let next = self.input.next().unwrap();
                    if next == '\\' {
                        if let Some(char) = self.input.next() {
                            string.push('\\');
                            string.push(char);
                        } else {
                            return None;
                        }
                    } else {
                        string.push(next);
                    }
                }
                if let Some('"') = self.input.next() {
                    Token::String(string)
                } else {
                    return None;
                }
            }
            'A'..='Z' | 'a'..='z' => {
                let mut ident = next.to_string();
                while self
                    .input
                    .peek()
                    .is_some_and(|c| c.is_ascii_alphanumeric() || *c == '_')
                {
                    ident.push(self.input.next().unwrap());
                }
                match ident.as_str() {
                    LET => Token::Let,
                    UNDEF => Token::Undef,
                    TRUE => Token::True,
                    FALSE => Token::False,
                    IF => Token::If,
                    THEN => Token::Then,
                    ELSE => Token::Else,
                    _ => Token::Ident(ident),
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
        let str = "1.3+3.2".chars().peekable();
        let tokenizer = Tokenizer::new(str).collect::<Vec<_>>();
        assert_eq!(
            tokenizer,
            vec![Token::Float(1.3), Token::Plus, Token::Float(3.2),]
        );
    }

    #[test]
    fn divide() {
        let str = "13/32".chars().peekable();
        let tokenizer = Tokenizer::new(str).collect::<Vec<_>>();
        assert_eq!(tokenizer, vec![Token::Int(13), Token::Div, Token::Int(32),]);

        let str = "13.5/32.2".chars().peekable();
        let tokenizer = Tokenizer::new(str).collect::<Vec<_>>();
        assert_eq!(
            tokenizer,
            vec![Token::Float(13.5), Token::Div, Token::Float(32.2),]
        );
    }

    #[test]
    fn test_space() {
        let str = "13.5    + 12.5 *     30.5*10".chars().peekable();
        let tokens = Tokenizer::new(str).collect::<Vec<_>>();
        assert_eq!(
            tokens,
            vec![
                Token::Float(13.5),
                Token::Plus,
                Token::Float(12.5),
                Token::Mult,
                Token::Float(30.5),
                Token::Mult,
                Token::Int(10)
            ]
        );
    }

    #[test]
    fn test_tokenizer() {
        let str = "10 + 5";
        let mut tokenizer = Tokenizer::new(str.chars().peekable());
        assert_eq!(tokenizer.next(), Some(Token::Int(10)));
    }

    #[test]
    fn test_hex() {
        let str = "0x1ff";

        let mut tokenizer = Tokenizer::new(str.chars().peekable());
        assert_eq!(tokenizer.next(), Some(Token::Int(511)));
    }

    #[test]
    fn test_bin() {
        let str = "0b1100";

        let mut tokenizer = Tokenizer::new(str.chars().peekable());
        assert_eq!(tokenizer.next(), Some(Token::Int(12)));
    }

    #[test]
    fn alpha_undscore_idents() {
        let str = "foo_bar1337";

        let mut tokenizer = Tokenizer::new(str.chars().peekable());
        assert_eq!(
            tokenizer.next(),
            Some(Token::Ident("foo_bar1337".to_string()))
        );
    }

    #[test]
    fn test_if_else() {
        let str = "if 1 == 3 then 3 % 3 else \"lol\"".chars().peekable();
        let tokens = Tokenizer::new(str).collect::<Vec<_>>();
        assert_eq!(
            tokens,
            vec![
                Token::If,
                Token::Int(1),
                Token::Eq,
                Token::Int(3),
                Token::Then,
                Token::Int(3),
                Token::Mod,
                Token::Int(3),
                Token::Else,
                Token::String("lol".to_string()),
            ]
        );
    }
}
