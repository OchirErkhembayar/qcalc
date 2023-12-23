use crate::token::Token;

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary(Box<Expr>, Operator, Box<Expr>),
    Grouping(Box<Expr>),
    Num(f64),
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mult,
    Div,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }

    // TODO: Error handling
    pub fn parse(&mut self) -> Result<Expr, ()> {
        Ok(Expr::Num(10.5))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_add() {
        let mut parser = Parser::new(vec![Token::Num(10.0), Token::Add, Token::Num(5.0)]);
        let expr = parser.parse().unwrap();

        assert_eq!(
            expr,
            Expr::Binary(
                Box::new(Expr::Num(10.0)),
                Operator::Add,
                Box::new(Expr::Num(5.0))
            )
        );
    }
}
