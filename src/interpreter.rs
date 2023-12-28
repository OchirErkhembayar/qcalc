use crate::{
    parse::{Expr, Func},
    token::Token,
};

pub fn interpret(expr: Expr) -> f64 {
    match expr {
        Expr::Num(num) => num,
        Expr::Binary(left, operator, right) => {
            let left = interpret(*left);
            let right = interpret(*right);
            match operator {
                Token::Plus => left + right,
                Token::Minus => left - right,
                Token::Mult => left * right,
                Token::Div => left / right,
                Token::Mod => left % right,
                _ => unreachable!(),
            }
        }
        Expr::Abs(expr) => interpret(*expr).abs(),
        Expr::Grouping(expr) => interpret(*expr),
        Expr::Negative(expr) => -interpret(*expr),
        Expr::Exponent(base, exponent) => interpret(*base).powf(interpret(*exponent)),
        Expr::Func(func, arg) => {
            let arg = interpret(*arg);
            match func {
                Func::Sin => arg.sin(),
                Func::Cos => arg.cos(),
                Func::Tan => arg.tan(),
                Func::Ln => arg.ln(),
                Func::Log(b) => arg.log(b),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_add() {
        let expr = Expr::Binary(
            Box::new(Expr::Num(5.0)),
            Token::Plus,
            Box::new(Expr::Num(1.0)),
        );

        assert_eq!(interpret(expr), 6.0);
    }

    #[test]
    fn test_negative() {
        let expr = Expr::Binary(
            Box::new(Expr::Grouping(Box::new(Expr::Binary(
                Box::new(Expr::Num(5.0)),
                Token::Minus,
                Box::new(Expr::Num(3.0)),
            )))),
            Token::Mult,
            Box::new(Expr::Negative(Box::new(Expr::Num(3.0)))),
        );

        assert_eq!(interpret(expr), -6.0);
    }
}
