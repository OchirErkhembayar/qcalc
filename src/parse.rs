use crate::Tokenizer;
use core::iter::Peekable;
use std::{error::Error, fmt::Display};

use crate::{interpreter::Stmt, token::Token};
const COS: &str = "cos";
const COSH: &str = "cosh";
const ACOS: &str = "acos";
const ACOSH: &str = "acosh";
const ABS: &str = "abs";
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
const MAP: &str = "map";
const SUM: &str = "sum";
const FOLD: &str = "fold";
const FILTER: &str = "filter";
const ODD: &str = "odd";
const EVEN: &str = "even";

#[derive(Debug)]
pub struct Parser<'a> {
    tokenizer: Peekable<Tokenizer<'a>>,
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
    Abs,
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
    Log,
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
    Map,
    Sum,
    Fold,
    Filter,
    Odd,
    Even,
}

impl Func {
    pub fn arity(&self) -> usize {
        match self {
            Func::Abs => 1,
            Func::Sin => 1,
            Func::Sinh => 1,
            Func::Asin => 1,
            Func::Asinh => 1,
            Func::Cos => 1,
            Func::Cosh => 1,
            Func::Acos => 1,
            Func::Acosh => 1,
            Func::Tan => 1,
            Func::Tanh => 1,
            Func::Atan => 1,
            Func::Atanh => 1,
            Func::Ln => 1,
            Func::Log => 2,
            Func::Degs => 1,
            Func::Rads => 1,
            Func::Sq => 1,
            Func::Sqrt => 1,
            Func::Cube => 1,
            Func::Cbrt => 1,
            Func::Round => 1,
            Func::Ceil => 1,
            Func::Floor => 1,
            Func::Exp => 1,
            Func::Exp2 => 1,
            Func::Fract => 1,
            Func::Recip => 1,
            Func::Map => 2,
            Func::Sum => 1,
            Func::Fold => 3,
            Func::Filter => 2,
            Func::Odd => 1,
            Func::Even => 1,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Float(f64),
    Int(i64),
    Unary(Box<Expr>, Token),
    Call(Box<Expr>, Vec<Expr>),
    Func(Func, Vec<Expr>),
    Fun(Vec<String>, Box<Expr>),
    Var(String),
    Bool(bool),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    String(String),
    List(Vec<Expr>),
}

impl Expr {
    pub fn format(&self) -> String {
        match self {
            Self::Float(float) => float.to_string(),
            Self::If(cond, then, else_expr) => {
                format!("if {} then {} else {}", cond, then, else_expr)
            }
            Self::Int(int) => int.to_string(),
            Self::String(string) => format!("\"{}\"", string.clone()),
            Self::Unary(expr, operator) => format!("{}{}", operator, expr.format()),
            Self::Grouping(expr) => format!("({})", expr.format()),
            Self::Var(var) => var.to_string(),
            Self::Binary(left, operator, right) => {
                format!("{}{}{}", left.format(), operator, right.format())
            }
            Self::Bool(bool) => bool.to_string(),
            Self::Fun(params, body) => {
                format!("|{}| {}", params.join(", "), body,)
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
            Self::Func(func, arguments) => format!(
                "{}({})",
                func,
                arguments
                    .iter()
                    .map(|a| a.format())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::List(elems) => format!(
                "[{}]",
                elems
                    .iter()
                    .map(|e| e.format())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl ParseErr {
    pub fn new(token: Token, msg: &'static str) -> Self {
        Self { token, msg }
    }
}

impl<'a> Parser<'a> {
    pub fn new(tokenizer: Peekable<Tokenizer<'a>>, current: Token) -> Self {
        Self { tokenizer, current }
    }

    fn at_end(&mut self) -> bool {
        self.tokenizer.peek().is_none()
    }

    fn advance(&mut self) -> Token {
        if !self.at_end() {
            let mut next = self.tokenizer.next().unwrap();
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
        self.consume(Token::Assign, "Expected =")?;
        let expr = self.expression()?;
        Ok(Stmt::Assign(name, expr))
    }

    fn expression(&mut self) -> Result<Expr, ParseErr> {
        match self.peek() {
            Token::Pipe => self.callable(),
            _ => self.if_expr(),
        }
    }

    fn callable(&mut self) -> Result<Expr, ParseErr> {
        self.consume(Token::Pipe, "Missing opening pipe")?;
        let mut parameters = Vec::new();
        if *self.peek() != Token::Pipe {
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
        self.consume(Token::Pipe, "Missing closing pipe")?;
        let expr = Box::new(self.expression()?);
        Ok(Expr::Fun(parameters, expr))
    }

    fn if_expr(&mut self) -> Result<Expr, ParseErr> {
        if *self.peek() == Token::If {
            self.advance();
            let cond = Box::new(self.expression()?);
            self.consume(Token::Then, "Expected then after condition")?;
            let then = Box::new(self.expression()?);
            self.consume(Token::Else, "Expected else after then body")?;
            let else_expr = Box::new(self.expression()?);
            Ok(Expr::If(cond, then, else_expr))
        } else {
            self.or()
        }
    }

    fn or(&mut self) -> Result<Expr, ParseErr> {
        let mut expr = self.and()?;
        while *self.peek() == Token::Or {
            self.advance();
            let rhs = Box::new(self.and()?);
            expr = Expr::Binary(Box::new(expr), Token::Or, rhs);
        }
        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, ParseErr> {
        let mut expr = self.bit_binary()?;
        while *self.peek() == Token::And {
            self.advance();
            let rhs = Box::new(self.bit_binary()?);
            expr = Expr::Binary(Box::new(expr), Token::And, rhs);
        }
        Ok(expr)
    }

    fn bit_binary(&mut self) -> Result<Expr, ParseErr> {
        let mut expr = self.equality()?;
        while matches!(*self.peek(), Token::Pipe | Token::BitXor | Token::BitAnd) {
            let operator = self.advance();
            let right = self.equality()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right))
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseErr> {
        let mut expr = self.comparison()?;
        while matches!(*self.peek(), Token::Eq | Token::Ne) {
            let operator = self.advance();
            let right = Box::new(self.comparison()?);
            expr = Expr::Binary(Box::new(expr), operator, right);
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseErr> {
        let mut expr = self.bit_shift()?;
        while matches!(
            *self.peek(),
            Token::Gte | Token::Gt | Token::Lte | Token::Lt
        ) {
            let operator = self.advance();
            let right = Box::new(self.bit_shift()?);
            expr = Expr::Binary(Box::new(expr), operator, right);
        }
        Ok(expr)
    }

    fn bit_shift(&mut self) -> Result<Expr, ParseErr> {
        let mut expr = self.term()?;
        while matches!(*self.peek(), Token::Shr | Token::Shl) {
            let operator = self.advance();
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
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
        let mut expr = self.unary()?;
        while *self.peek() == Token::Pow {
            let operator = self.advance();
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseErr> {
        if matches!(*self.peek(), Token::Minus | Token::Not) {
            let operator = self.advance();
            let right = self.unary()?;
            Ok(Expr::Unary(Box::new(right), operator))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr, ParseErr> {
        let mut expr = self.primary()?;

        while *self.peek() == Token::LParen {
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
            expr = Expr::Call(Box::new(expr), args);
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr, ParseErr> {
        match self.peek() {
            Token::Float(float) => {
                let res = Ok(Expr::Float(*float));
                self.advance();
                res
            }
            Token::Int(int) => {
                let res = Ok(Expr::Int(*int));
                self.advance();
                res
            }
            Token::String(string) => {
                let res = Ok(Expr::String(string.clone()));
                self.advance();
                res
            }
            Token::True => {
                self.advance();
                Ok(Expr::Bool(true))
            }
            Token::False => {
                self.advance();
                Ok(Expr::Bool(false))
            }
            Token::LParen => {
                self.advance();
                let expr = Box::new(self.expression()?);
                self.consume(Token::RParen, "Missing closing parentheses")?;
                Ok(Expr::Grouping(expr))
            }
            Token::LBracket => {
                self.advance();
                let mut elems = vec![];
                if !self.check(&Token::RBracket) {
                    loop {
                        elems.push(self.expression()?);
                        if !self.check(&Token::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
                self.consume(Token::RBracket, "Missing closing bracket")?;
                Ok(Expr::List(elems))
            }
            Token::Ident(func) => {
                let func = func.to_owned();
                self.advance();
                let func = match func.as_str() {
                    ABS => Func::Abs,
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
                    LOG => Func::Log,
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
                    MAP => Func::Map,
                    SUM => Func::Sum,
                    FOLD => Func::Fold,
                    FILTER => Func::Filter,
                    EVEN => Func::Even,
                    ODD => Func::Odd,
                    _ => return Ok(Expr::Var(func)),
                };
                self.consume(Token::LParen, "Missing opening parentheses")?;
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
                Ok(Expr::Func(func, args))
            }
            _ => Err(ParseErr::new(self.peek().clone(), "Expected expression")),
        }
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
                Func::Abs => ABS,
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
                Func::Log => LOG,
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
                Func::Map => MAP,
                Func::Sum => SUM,
                Func::Fold => FOLD,
                Func::Filter => FILTER,
                Func::Odd => ODD,
                Func::Even => EVEN,
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
        let expr = Expr::Binary(Box::new(Expr::Int(10)), Token::Plus, Box::new(Expr::Int(5)));

        check("10 + 5", expr);
    }

    #[test]
    fn simple_mult() {
        let expr = Expr::Binary(
            Box::new(Expr::Float(20.5)),
            Token::Mult,
            Box::new(Expr::Float(5.1)),
        );

        check("20.5 * 5.1", expr);
    }

    #[test]
    fn grouping() {
        let expr = Expr::Binary(
            Box::new(Expr::Grouping(Box::new(Expr::Binary(
                Box::new(Expr::Int(1)),
                Token::Plus,
                Box::new(Expr::Int(2)),
            )))),
            Token::Mult,
            Box::new(Expr::Int(5)),
        );

        check("(1 + 2) * 5", expr);
    }

    #[test]
    fn negative() {
        let expr = Expr::Binary(
            Box::new(Expr::Unary(
                Box::new(Expr::Grouping(Box::new(Expr::Int(5)))),
                Token::Minus,
            )),
            Token::Div,
            Box::new(Expr::Int(1)),
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
        let expected = Stmt::Assign(
            "foo".to_string(),
            Expr::Fun(
                vec!["x".to_string(), "y".to_string()],
                Box::new(Expr::Binary(
                    Box::new(Expr::Var("x".to_string())),
                    Token::Plus,
                    Box::new(Expr::Var("y".to_string())),
                )),
            ),
        );

        let mut tokenizer = Tokenizer::new("let foo = |x, y| x + y".chars().peekable()).peekable();
        let current = tokenizer.next().unwrap();
        let stmt = Parser::new(tokenizer, current).parse().unwrap();
        assert_eq!(stmt, expected);
    }

    #[test]
    fn test_function_single_param() {
        let expected = Stmt::Assign(
            "foo".to_string(),
            Expr::Fun(
                vec!["y".to_string()],
                Box::new(Expr::Binary(
                    Box::new(Expr::Var("y".to_string())),
                    Token::Plus,
                    Box::new(Expr::Int(1)),
                )),
            ),
        );

        let mut tokenizer = Tokenizer::new("let foo = |y| y+1".chars().peekable()).peekable();
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

        let mut tokenizer = Tokenizer::new("let foo = |y, y, y, y|".chars().peekable()).peekable();
        let current = tokenizer.next().unwrap();
        assert_eq!(Parser::new(tokenizer, current).parse(), expected);
    }

    #[test]
    fn test_assignment() {
        let expected = Stmt::Assign(
            "foo".to_string(),
            Expr::Func(Func::Sq, vec![Expr::Float(2.0)]),
        );

        let mut tokenizer = Tokenizer::new("let foo = sq(2.0)".chars().peekable()).peekable();
        let current = tokenizer.next().unwrap();
        assert_eq!(Parser::new(tokenizer, current).parse(), Ok(expected));
    }

    #[test]
    fn integer_base_log() {
        let expected = Stmt::Expr(Expr::Func(Func::Log, vec![Expr::Int(10), Expr::Int(1000)]));

        let mut tokenizer = Tokenizer::new("log(10, 1000)".chars().peekable()).peekable();
        let current = tokenizer.next().unwrap();
        assert_eq!(Parser::new(tokenizer, current).parse(), Ok(expected));
    }

    #[test]
    fn test_bools() {
        let expected = Stmt::Expr(Expr::Binary(
            Box::new(Expr::Bool(true)),
            Token::Or,
            Box::new(Expr::Bool(false)),
        ));

        let mut tokenizer = Tokenizer::new("true || false".chars().peekable()).peekable();
        let current = tokenizer.next().unwrap();
        assert_eq!(Parser::new(tokenizer, current).parse(), Ok(expected));
    }

    #[test]
    fn test_if() {
        let expected = Stmt::Expr(Expr::If(
            Box::new(Expr::Bool(true)),
            Box::new(Expr::Int(1)),
            Box::new(Expr::String("this is the answer".to_string())),
        ));

        let mut tokenizer = Tokenizer::new(
            "if true then 1 else \"this is the answer\""
                .chars()
                .peekable(),
        )
        .peekable();
        let current = tokenizer.next().unwrap();
        assert_eq!(Parser::new(tokenizer, current).parse(), Ok(expected));
    }
}
