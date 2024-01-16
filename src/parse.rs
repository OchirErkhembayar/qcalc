use std::{error::Error, fmt::Display};

use crate::{inner_write, interpreter::Stmt, token::Token};
const COS: &str = "cos";
const COSH: &str = "cosh";
const SIN: &str = "sin";
const SINH: &str = "sinh";
const TAN: &str = "tan";
const TANH: &str = "tanh";
const LOG: &str = "log";
const LN: &str = "ln";
const DEGS: &str = "degs";
const RADS: &str = "rads";
const SQRT: &str = "sqrt";
const SQ: &str = "sq";
const CBRT: &str = "cbrt";
const CUBE: &str = "cube";
const ROUND: &str = "round";

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

// "Built in" functions, separate of user defined functions
#[derive(Debug, PartialEq, Clone)]
pub enum Func {
    Sin,
    Sinh,
    Cos,
    Cosh,
    Tan,
    Tanh,
    Ln,
    Log(f64),
    Degs,
    Rads,
    Sq,
    Sqrt,
    Cube,
    Cbrt,
    Round,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Num(f64),
    Negative(Box<Expr>),
    Abs(Box<Expr>),
    Exponent(Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    Func(Func, Box<Expr>),
    Var(String),
}

impl Expr {
    pub fn format(&self) -> String {
        match self {
            Self::Num(num) => num.to_string(),
            Self::Negative(expr) => format!("-{}", expr.format()),
            Self::Grouping(expr) => format!("({})", expr.format()),
            Self::Abs(expr) => format!("|{}|", expr.format()),
            Self::Var(var) => var.to_string(),
            Self::Binary(left, operator, right) => {
                format!("{}{}{}", left.format(), operator, right.format())
            }
            Self::Call(name, args) => {
                format!(
                    "{}({})",
                    name,
                    args.iter()
                        .map(|a| a.format())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Self::Exponent(base, exponent) => format!("{}^{}", base.format(), exponent.format()),
            Self::Func(func, argument) => format!("{}({})", func, argument.format()),
        }
    }
}

impl ParseErr {
    pub fn new(token: Token, msg: &'static str) -> Self {
        Self { token, msg }
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    fn at_end(&self) -> bool {
        *self.peek() == Token::Eoe
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn advance(&mut self) -> Token {
        if !self.at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn check(&self, token: &Token) -> bool {
        if self.at_end() {
            return false;
        }
        *self.peek() == *token
    }

    fn consume(&mut self, token: Token, msg: &'static str) -> Result<Token, ParseErr> {
        if self.check(&token) {
            Ok(self.advance())
        } else {
            Err(ParseErr::new(token, msg))
        }
    }
}

impl Parser {
    pub fn parse(&mut self) -> Result<Stmt, ParseErr> {
        let res = match self.peek() {
            Token::Fn => self.function()?,
            _ => Stmt::Expr(self.expression()?),
        };
        if self.at_end() {
            Ok(res)
        } else {
            // A complete expression was parsed but there were more tokens
            Err(ParseErr::new(self.peek().clone(), "Unexpected token"))
        }
    }

    fn function(&mut self) -> Result<Stmt, ParseErr> {
        self.advance();
        let name = match self.advance() {
            Token::Ident(name) => name,
            token => return Err(ParseErr::new(token, "Missing function name")),
        };
        self.consume(Token::LParen, "Missing opening parentheses")?;
        let mut parameters = Vec::new();
        if *self.peek() != Token::RParen {
            loop {
                let next = self.advance();
                if let Token::Ident(arg) = &next {
                    if parameters.contains(arg) {
                        return Err(ParseErr::new(
                            next.clone(),
                            "Function parameters must be unique",
                        ));
                    }
                    parameters.push(arg.clone()); // Not ideal...
                } else {
                    return Err(ParseErr::new(next, "Expected argument"));
                }
                if *self.peek() != Token::Comma {
                    break;
                }
                self.advance();
            }
        }
        self.consume(Token::RParen, "Missing closing parentheses")?;
        let expr = self.expression()?;
        Ok(Stmt::Fn(name, parameters, expr))
    }

    fn expression(&mut self) -> Result<Expr, ParseErr> {
        self.term()
    }

    fn term(&mut self) -> Result<Expr, ParseErr> {
        let mut expr = self.factor()?;
        while *self.peek() == Token::Plus || *self.peek() == Token::Minus {
            let operator = self.advance();
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseErr> {
        let mut expr = self.exponent()?;
        while *self.peek() == Token::Div
            || *self.peek() == Token::Mult
            || *self.peek() == Token::Mod
        {
            let operator = self.advance();
            let right = self.exponent()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn exponent(&mut self) -> Result<Expr, ParseErr> {
        let mut expr = self.negative()?;
        while *self.peek() == Token::Power {
            self.advance();
            let right = self.negative()?;
            expr = Expr::Exponent(Box::new(expr), Box::new(right));
        }
        Ok(expr)
    }

    fn negative(&mut self) -> Result<Expr, ParseErr> {
        if *self.peek() == Token::Minus {
            self.advance();
            let right = self.negative()?;
            Ok(Expr::Negative(Box::new(right)))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr, ParseErr> {
        let expr = self.primary()?;

        if let Expr::Var(name) = &expr {
            if self.check(&Token::LParen) {
                self.advance();
                let mut args = vec![];
                if !self.check(&Token::RParen) {
                    loop {
                        args.push(self.expression()?);
                        if !self.check(&Token::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
                self.consume(Token::RParen, "Missing closing parentheses")?;
                return Ok(Expr::Call(name.to_owned(), args));
            }
        }
        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr, ParseErr> {
        if let Token::Num(num) = *self.peek() {
            self.advance();
            return Ok(Expr::Num(num));
        }
        if let Token::LParen = *self.peek() {
            self.advance();
            let expr = Box::new(self.expression()?);
            self.consume(Token::RParen, "Missing closing parentheses")?;
            return Ok(Expr::Grouping(expr));
        }
        if let Token::Pipe = *self.peek() {
            self.advance();
            let expr = Box::new(self.expression()?);
            self.consume(Token::Pipe, "Missing closing pipe")?;
            return Ok(Expr::Abs(expr));
        }
        if let Token::Ident(func) = self.peek() {
            let func = func.to_owned();
            self.advance();
            let func = match func.as_str() {
                SIN => Func::Sin,
                SINH => Func::Sinh,
                COS => Func::Cos,
                COSH => Func::Cosh,
                TAN => Func::Tan,
                TANH => Func::Tanh,
                LN => Func::Ln,
                LOG => {
                    if let Token::Num(base) = self.advance() {
                        Func::Log(base)
                    } else {
                        return Err(ParseErr::new(
                            self.peek().clone(),
                            "Missing base for log function",
                        ));
                    }
                }
                DEGS => Func::Degs,
                RADS => Func::Rads,
                SQ => Func::Sq,
                SQRT => Func::Sqrt,
                CUBE => Func::Cube,
                CBRT => Func::Cbrt,
                ROUND => Func::Round,
                _ => return Ok(Expr::Var(func)),
            };
            self.consume(Token::LParen, "Missing opening parentheses")?;
            let arg = Box::new(self.expression()?);
            self.consume(Token::RParen, "Missing closing parentheses")?;
            return Ok(Expr::Func(func, arg));
        }
        Err(ParseErr::new(self.peek().clone(), "Expected expression"))
    }
}

impl Error for ParseErr {}

impl Display for ParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ERROR: {}", self.msg)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format())
    }
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Func::Sin => SIN,
                Func::Sinh => SINH,
                Func::Cos => COS,
                Func::Cosh => COSH,
                Func::Tan => TAN,
                Func::Tanh => TANH,
                Func::Ln => LN,
                Func::Log(base) => return inner_write(format!("log{}", base), f),
                Func::Degs => DEGS,
                Func::Rads => RADS,
                Func::Sq => SQ,
                Func::Sqrt => SQRT,
                Func::Cube => CUBE,
                Func::Cbrt => CBRT,
                Func::Round => ROUND,
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(tokens: Vec<Token>, expected: Expr) {
        let mut parser = Parser::new(tokens);
        let expr = parser.parse().unwrap();

        assert_eq!(expr, Stmt::Expr(expected));
    }

    #[test]
    fn simple_add() {
        let tokens = vec![Token::Num(10.0), Token::Plus, Token::Num(5.0), Token::Eoe];
        let expr = Expr::Binary(
            Box::new(Expr::Num(10.0)),
            Token::Plus,
            Box::new(Expr::Num(5.0)),
        );

        check(tokens, expr);
    }

    #[test]
    fn simple_mult() {
        let tokens = vec![Token::Num(20.0), Token::Mult, Token::Num(5.0), Token::Eoe];
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
            Token::Eoe,
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
            Token::Eoe,
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
        let tokens = vec![Token::Minus, Token::LParen, Token::Num(5.0), Token::Eoe];
        if let Err(err) = Parser::new(tokens).parse() {
            assert_eq!(
                err,
                ParseErr::new(Token::RParen, "Missing closing parentheses")
            );
        } else {
            panic!("Didn't return error");
        }
    }

    #[test]
    fn test_function_mult_params() {
        let tokens = vec![
            Token::Fn,
            Token::Ident("foo".to_string()),
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Eoe,
        ];
        let expected = Stmt::Fn(
            "foo".to_string(),
            vec!["x".to_string(), "y".to_string()],
            Expr::Binary(
                Box::new(Expr::Var("x".to_string())),
                Token::Plus,
                Box::new(Expr::Var("y".to_string())),
            ),
        );

        let stmt = Parser::new(tokens).parse().unwrap();
        assert_eq!(stmt, expected);
    }

    #[test]
    fn test_function_single_param() {
        let tokens = vec![
            Token::Fn,
            Token::Ident("foo".to_string()),
            Token::LParen,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::Ident("y".to_string()),
            Token::Plus,
            Token::Num(1.0),
            Token::Eoe,
        ];
        let expected = Stmt::Fn(
            "foo".to_string(),
            vec!["y".to_string()],
            Expr::Binary(
                Box::new(Expr::Var("y".to_string())),
                Token::Plus,
                Box::new(Expr::Num(1.0)),
            ),
        );

        let stmt = Parser::new(tokens).parse().unwrap();
        assert_eq!(stmt, expected);
    }

    #[test]
    fn test_uniq_params() {
        let tokens = vec![
            Token::Fn,
            Token::Ident("foo".to_string()),
            Token::LParen,
            Token::Ident("y".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::Ident("y".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Eoe,
        ];
        let expected = Err(ParseErr::new(
            Token::Ident("y".to_string()),
            "Function parameters must be unique",
        ));

        assert_eq!(Parser::new(tokens).parse(), expected);
    }
}
