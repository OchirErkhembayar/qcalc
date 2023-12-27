use crate::token::Token;

use std::ops::Deref;

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

#[derive(Debug, PartialEq)]
pub struct ParseErr {
    pub token: Token,
    pub msg: &'static str,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Num(f64),
    Negative(Box<Expr>),
}

impl ParseErr {
    fn new(token: Token, msg: &'static str) -> Self {
        Self { token, msg }
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    fn at_end(&self) -> bool {
        self.tokens.len() <= self.current + 1
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1]
    }

    fn advance(&mut self) -> Token {
        if !self.at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn peek(&self) -> Token {
        self.tokens[self.current]
    }

    fn check(&self, token: Token) -> bool {
        if self.at_end() {
            return false;
        }

        self.peek() == token
    }

    fn consume(&mut self, token: Token, msg: &'static str) -> Result<Token, ParseErr> {
        if self.check(token) {
            Ok(self.advance())
        } else {
            Err(ParseErr::new(token, msg))
        }
    }
}

impl Parser {
    pub fn parse(&mut self) -> Result<Expr, ParseErr> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Expr, ParseErr> {
        self.term()
    }

    fn term(&mut self) -> Result<Expr, ParseErr> {
        let mut expr = self.factor()?;

        while self.peek() == Token::Plus || self.peek() == Token::Minus {
            let operator = self.advance();
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseErr> {
        let mut expr = self.negative()?;

        while self.peek() == Token::Div || self.peek() == Token::Mult {
            let operator = self.advance();
            let right = self.negative()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn negative(&mut self) -> Result<Expr, ParseErr> {
        if self.peek() == Token::Minus {
            self.advance();
            let right = Box::new(self.negative()?);
            Ok(Expr::Negative(right))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expr, ParseErr> {
        if let Token::Num(num) = self.peek() {
            self.advance();
            return Ok(Expr::Num(num));
        }

        if let Token::LParen = self.peek() {
            self.advance();
            let expr = Box::new(self.expression()?);
            self.consume(Token::RParen, "Missing closing parentheses")?;
            return Ok(Expr::Grouping(expr));
        }

        Err(ParseErr::new(self.peek(), "Expected expression"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(tokens: Vec<Token>, expected: Expr) {
        let mut parser = Parser::new(tokens);
        let expr = parser.parse().unwrap();

        assert_eq!(expr, expected,);
    }

    #[test]
    fn simple_add() {
        let tokens = vec![Token::Num(10.0), Token::Plus, Token::Num(5.0)];
        let expr = Expr::Binary(
            Box::new(Expr::Num(10.0)),
            Token::Plus,
            Box::new(Expr::Num(5.0)),
        );

        check(tokens, expr);
    }

    #[test]
    fn simple_mult() {
        let tokens = vec![Token::Num(20.0), Token::Mult, Token::Num(5.0)];
        let expr = Expr::Binary(
            Box::new(Expr::Num(20.0)),
            Token::Mult,
            Box::new(Expr::Num(5.0)),
        );

        check(tokens, expr);
    }

    #[test]
    fn grouping() {
        let tokens = vec![
            Token::LParen,
            Token::Num(1.0),
            Token::Plus,
            Token::Num(2.0),
            Token::RParen,
            Token::Mult,
            Token::Num(5.0),
        ];
        let expr = Expr::Binary(
            Box::new(Expr::Grouping(Box::new(Expr::Binary(
                Box::new(Expr::Num(1.0)),
                Token::Plus,
                Box::new(Expr::Num(2.0)),
            )))),
            Token::Mult,
            Box::new(Expr::Num(5.0)),
        );

        check(tokens, expr);
    }

    #[test]
    fn negative() {
        let tokens = vec![
            Token::Minus,
            Token::LParen,
            Token::Num(5.0),
            Token::RParen,
            Token::Div,
            Token::Num(1.0),
        ];
        let expr = Expr::Binary(
            Box::new(Expr::Negative(Box::new(Expr::Grouping(Box::new(
                Expr::Num(5.0),
            ))))),
            Token::Div,
            Box::new(Expr::Num(1.0)),
        );

        check(tokens, expr);
    }

    #[test]
    fn test_missing_closing_paren() {
        let tokens = vec![Token::Minus, Token::LParen, Token::Num(5.0)];
        if let Err(err) = Parser::new(tokens).parse() {
            assert_eq!(
                err,
                ParseErr::new(Token::RParen, "Missing closing parentheses")
            );
        } else {
            panic!("Didn't return error");
        }
    }
}
