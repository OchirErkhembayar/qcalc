use crate::Tokenizer;
use core::iter::Peekable;
use std::{error::Error, fmt::Display};

use crate::{inner_write, interpreter::Stmt, token::Token};
const COS: &str = "cos";
const COSH: &str = "cosh";
const ACOS: &str = "acos";
const ACOSH: &str = "acosh";
const SIN: &str = "sin";
const SINH: &str = "sinh";
const ASIN: &str = "asin";
const ASINH: &str = "asinh";
const TAN: &str = "tan";
const TANH: &str = "tanh";
const ATAN: &str = "atan";
const ATANH: &str = "atanh";
const LOG: &str = "log";
const LN: &str = "ln";
const DEGS: &str = "degs";
const RADS: &str = "rads";
const SQRT: &str = "sqrt";
const SQ: &str = "sq";
const CBRT: &str = "cbrt";
const CUBE: &str = "cube";
const ROUND: &str = "round";
const CEIL: &str = "ceil";
const FLOOR: &str = "floor";
const EXP: &str = "exp";
const EXP2: &str = "exp2";
const FRACT: &str = "fract";
const RECIP: &str = "recip";

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Peekable<Tokenizer<'a>>,
    current: Token,
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
    Asin,
    Asinh,
    Cos,
    Cosh,
    Acos,
    Acosh,
    Tan,
    Tanh,
    Atan,
    Atanh,
    Ln,
    Log(f64),
    Degs,
    Rads,
    Sq,
    Sqrt,
    Cube,
    Cbrt,
    Round,
    Ceil,
    Floor,
    Exp,
    Exp2,
    Fract,
    Recip,
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

impl<'a> Parser<'a> {
    pub fn new(tokens: Peekable<Tokenizer<'a>>, current: Token) -> Self {
        Self { tokens, current }
    }

    fn at_end(&mut self) -> bool {
        self.tokens.peek().is_none()
    }

    fn advance(&mut self) -> Token {
        if !self.at_end() {
            let mut next = self.tokens.next().unwrap();
            std::mem::swap(&mut self.current, &mut next);
            next
        } else {
            // Not a big deal because this is only for the last token
            self.current.clone()
        }
    }

    fn peek(&self) -> &Token {
        &self.current
    }

    fn check(&mut self, token: &Token) -> bool {
        self.current == *token
    }

    fn consume(&mut self, token: Token, msg: &'static str) -> Result<Token, ParseErr> {
        if self.check(&token) {
            Ok(self.advance())
        } else {
            Err(ParseErr::new(token, msg))
        }
    }
}

impl<'a> Parser<'a> {
    pub fn parse(&mut self) -> Result<Stmt, ParseErr> {
        let res = match self.peek() {
            Token::Fn => self.function()?,
            Token::Let => self.assign()?,
            Token::Undef => self.undef()?,
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
                if let Token::Ident(arg) = next {
                    if parameters.contains(&arg) {
                        return Err(ParseErr::new(
                            Token::Ident(arg),
                            "Function parameters must be unique",
                        ));
                    }
                    parameters.push(arg);
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

    fn undef(&mut self) -> Result<Stmt, ParseErr> {
        self.advance();
        self.consume(Token::LParen, "Missing opening parentheses")?;
        let mut parameters = Vec::new();
        if *self.peek() != Token::RParen {
            loop {
                let next = self.advance();
                if let Token::Ident(arg) = next {
                    if parameters.contains(&arg) {
                        return Err(ParseErr::new(
                            Token::Ident(arg),
                            "Function parameters must be unique",
                        ));
                    }
                    parameters.push(arg);
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
        Ok(Stmt::Undef(parameters))
    }

    fn assign(&mut self) -> Result<Stmt, ParseErr> {
        self.advance();
        let name = match self.advance() {
            Token::Ident(name) => name,
            token => return Err(ParseErr::new(token, "Missing function name")),
        };
        self.consume(Token::Eq, "Expected =")?;
        let expr = self.expression()?;
        Ok(Stmt::Assign(name, expr))
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
                ASIN => Func::Asin,
                ASINH => Func::Asinh,
                COS => Func::Cos,
                COSH => Func::Cosh,
                ACOS => Func::Acos,
                ACOSH => Func::Acosh,
                TAN => Func::Tan,
                TANH => Func::Tanh,
                ATAN => Func::Atan,
                ATANH => Func::Atanh,
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
                CEIL => Func::Ceil,
                FLOOR => Func::Floor,
                EXP => Func::Exp,
                EXP2 => Func::Exp2,
                FRACT => Func::Fract,
                RECIP => Func::Recip,
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
        write!(f, "{}, got: {}", self.msg, self.token)
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
                Func::Asin => ASIN,
                Func::Asinh => ASINH,
                Func::Cos => COS,
                Func::Cosh => COSH,
                Func::Acos => ACOS,
                Func::Acosh => ACOSH,
                Func::Tan => TAN,
                Func::Tanh => TANH,
                Func::Atan => ATAN,
                Func::Atanh => ATANH,
                Func::Ln => LN,
                Func::Log(base) => return inner_write(format!("log{}", base), f),
                Func::Degs => DEGS,
                Func::Rads => RADS,
                Func::Sq => SQ,
                Func::Sqrt => SQRT,
                Func::Cube => CUBE,
                Func::Cbrt => CBRT,
                Func::Round => ROUND,
                Func::Ceil => CEIL,
                Func::Floor => FLOOR,
                Func::Exp => EXP,
                Func::Exp2 => EXP2,
                Func::Fract => FRACT,
                Func::Recip => RECIP,
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(str: &str, expected: Expr) {
        let mut tokenizer = Tokenizer::new(str.chars().peekable()).peekable();
        let current = tokenizer.next().unwrap();
        let mut parser = Parser::new(tokenizer, current);
        let expr = parser.parse().unwrap();

        assert_eq!(expr, Stmt::Expr(expected));
    }

    #[test]
    fn simple_add() {
        let expr = Expr::Binary(
            Box::new(Expr::Num(10.0)),
            Token::Plus,
            Box::new(Expr::Num(5.0)),
        );

        check("10 + 5", expr);
    }

    #[test]
    fn simple_mult() {
        let expr = Expr::Binary(
            Box::new(Expr::Num(20.0)),
            Token::Mult,
            Box::new(Expr::Num(5.0)),
        );

        check("20 * 5", expr);
    }

    #[test]
    fn grouping() {
        let expr = Expr::Binary(
            Box::new(Expr::Grouping(Box::new(Expr::Binary(
                Box::new(Expr::Num(1.0)),
                Token::Plus,
                Box::new(Expr::Num(2.0)),
            )))),
            Token::Mult,
            Box::new(Expr::Num(5.0)),
        );

        check("(1 + 2) * 5", expr);
    }

    #[test]
    fn negative() {
        let expr = Expr::Binary(
            Box::new(Expr::Negative(Box::new(Expr::Grouping(Box::new(
                Expr::Num(5.0),
            ))))),
            Token::Div,
            Box::new(Expr::Num(1.0)),
        );

        check("-(5) / 1", expr);
    }

    #[test]
    fn test_missing_closing_paren() {
        let mut tokenizer = Tokenizer::new("-(5".chars().peekable()).peekable();
        let current = tokenizer.next().unwrap();
        if let Err(err) = Parser::new(tokenizer, current).parse() {
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
        let expected = Stmt::Fn(
            "foo".to_string(),
            vec!["x".to_string(), "y".to_string()],
            Expr::Binary(
                Box::new(Expr::Var("x".to_string())),
                Token::Plus,
                Box::new(Expr::Var("y".to_string())),
            ),
        );

        let mut tokenizer = Tokenizer::new("fn foo(x, y) x + y".chars().peekable()).peekable();
        let current = tokenizer.next().unwrap();
        let stmt = Parser::new(tokenizer, current).parse().unwrap();
        assert_eq!(stmt, expected);
    }

    #[test]
    fn test_function_single_param() {
        let expected = Stmt::Fn(
            "foo".to_string(),
            vec!["y".to_string()],
            Expr::Binary(
                Box::new(Expr::Var("y".to_string())),
                Token::Plus,
                Box::new(Expr::Num(1.0)),
            ),
        );

        let mut tokenizer = Tokenizer::new("fn foo(y)y+1".chars().peekable()).peekable();
        let current = tokenizer.next().unwrap();
        let stmt = Parser::new(tokenizer, current).parse().unwrap();
        assert_eq!(stmt, expected);
    }

    #[test]
    fn test_uniq_params() {
        let expected = Err(ParseErr::new(
            Token::Ident("y".to_string()),
            "Function parameters must be unique",
        ));

        let mut tokenizer = Tokenizer::new("fn foo(y, y, y, y)".chars().peekable()).peekable();
        let current = tokenizer.next().unwrap();
        assert_eq!(Parser::new(tokenizer, current).parse(), expected);
    }

    #[test]
    fn test_assignment() {
        let expected = Stmt::Assign(
            "foo".to_string(),
            Expr::Func(Func::Sq, Box::new(Expr::Num(2.0))),
        );

        let mut tokenizer = Tokenizer::new("let foo = sq(2.0)".chars().peekable()).peekable();
        let current = tokenizer.next().unwrap();
        assert_eq!(Parser::new(tokenizer, current).parse(), Ok(expected));
    }
}
