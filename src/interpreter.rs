use std::{
    collections::HashMap,
    error::Error,
    f64::consts::{E, PI},
    fmt::Display,
    ops::Neg,
};

use crate::{
    inner_write,
    parse::{Expr, Func},
    token::Token,
};

#[derive(Debug)]
pub struct Interpreter {
    env: HashMap<String, Value>,
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Fn(String, Vec<String>, Expr),
}

#[derive(Debug, Clone)]
pub enum Value {
    Fn(Function),
    Num(f64),
}

#[derive(Debug, Clone)]
pub struct Function {
    closure: HashMap<String, Value>,
    parameters: Vec<String>,
    arity: usize,
    body: Expr,
}

#[derive(Debug, PartialEq)]
pub enum InterpretError {
    UnknownVariable(String),
    UnknownFunction(String),
    UnInvokedFunction(String),
    WrongArity(String, usize, usize),
}

impl Function {
    fn new(parameters: Vec<String>, body: Expr, closure: HashMap<String, Value>) -> Self {
        Self {
            arity: parameters.len(),
            parameters,
            body,
            closure,
        }
    }

    fn call(&self, args: Vec<Value>) -> Result<f64, InterpretError> {
        let mut interpreter = Interpreter::with_env(self.closure.clone());
        args.into_iter()
            .enumerate()
            .for_each(|(i, arg)| interpreter.define(self.parameters[i].clone(), arg));
        interpreter.interpret_expr(&self.body)
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Self::default_env(),
        }
    }

    fn default_env() -> HashMap<String, Value> {
        HashMap::from_iter([
            ("p".to_string(), Value::Num(PI)),
            ("e".to_string(), Value::Num(E)),
            (
                "deg".to_string(),
                Value::Fn(Function::new(
                    vec!["rads".to_string()],
                    Expr::Binary(
                        Box::new(Expr::Var("rads".to_string())),
                        Token::Mult,
                        Box::new(Expr::Binary(
                            Box::new(Expr::Num(180.0)),
                            Token::Div,
                            Box::new(Expr::Num(PI)),
                        )),
                    ),
                    HashMap::new(),
                )),
            ),
        ])
    }

    pub fn with_env(env: HashMap<String, Value>) -> Self {
        Self { env }
    }

    pub fn declare_function(&mut self, name: String, parameters: Vec<String>, body: Expr) {
        self.env.insert(
            name,
            Value::Fn(Function::new(parameters, body, self.env.clone())),
        );
    }

    pub fn interpret_expr(&self, expr: &Expr) -> Result<f64, InterpretError> {
        match expr {
            Expr::Num(num) => Ok(*num),
            Expr::Binary(left, operator, right) => {
                let left = self.interpret_expr(left)?;
                let right = self.interpret_expr(right)?;
                let val = match operator {
                    Token::Plus => left + right,
                    Token::Minus => left - right,
                    Token::Mult => left * right,
                    Token::Div => left / right,
                    Token::Mod => left % right,
                    _ => unreachable!(),
                };
                Ok(val)
            }
            Expr::Abs(expr) => Ok(self.interpret_expr(expr)?.abs()),
            Expr::Grouping(expr) => self.interpret_expr(expr),
            Expr::Negative(expr) => Ok(self.interpret_expr(expr)?.neg()),
            Expr::Exponent(base, exponent) => Ok(self
                .interpret_expr(base)?
                .powf(self.interpret_expr(exponent)?)),
            Expr::Call(name, args) => {
                if let Some(Value::Fn(func)) = self.env.get(name) {
                    if args.len() != func.arity {
                        Err(InterpretError::WrongArity(
                            name.to_owned(),
                            args.len(),
                            func.arity,
                        ))
                    } else {
                        let mut vals = vec![];
                        for arg in args.iter() {
                            vals.push(Value::Num(self.interpret_expr(arg)?));
                        }
                        func.call(vals)
                    }
                } else {
                    Err(InterpretError::UnknownFunction(name.to_owned()))
                }
            }
            Expr::Var(var) => {
                if let Some(val) = self.env.get(var) {
                    match val {
                        Value::Fn(_) => Err(InterpretError::UnInvokedFunction(var.clone())),
                        Value::Num(num) => Ok(*num),
                    }
                } else {
                    Err(InterpretError::UnknownVariable(var.clone()))
                }
            }
            Expr::Func(func, arg) => {
                let arg = self.interpret_expr(arg)?;
                let val = match func {
                    Func::Sin => arg.sin(),
                    Func::Sinh => arg.sinh(),
                    Func::Cos => arg.cos(),
                    Func::Cosh => arg.cosh(),
                    Func::Tan => arg.tan(),
                    Func::Tanh => arg.tanh(),
                    Func::Ln => arg.ln(),
                    Func::Log(b) => arg.log(*b),
                };
                Ok(val)
            }
        }
    }

    pub fn reset_vars(&mut self) {
        self.env = Self::default_env()
    }

    pub fn define(&mut self, var: String, val: Value) {
        self.env.insert(var, val);
    }

    pub fn env(&self) -> &HashMap<String, Value> {
        &self.env
    }
}

impl Error for InterpretError {}

impl Display for InterpretError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::UnknownVariable(c) => format!("Unknown variable {}", c),
                Self::UnknownFunction(c) => format!("Unknown function {}", c),
                Self::UnInvokedFunction(c) => format!("Uninvoked function {}", c),
                Self::WrongArity(name, actual, expected) => format!(
                    "Function {} takes {} arguments but {} were provided",
                    name, expected, actual
                ),
            },
        )
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(num) => inner_write(num, f),
            Self::Fn(func) => inner_write(func, f),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}) {}", self.parameters.join(", "), self.body)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(expr: Expr, expected: Result<f64, InterpretError>) {
        let interpreter = Interpreter::new();
        let res = interpreter.interpret_expr(&expr);
        assert_eq!(res, expected);
    }

    fn check_with_vars(
        expr: Expr,
        expected: Result<f64, InterpretError>,
        env: HashMap<String, Value>,
    ) {
        let interpreter = Interpreter::with_env(env);
        let res = interpreter.interpret_expr(&expr);
        assert_eq!(res, expected);
    }

    #[test]
    fn simple_add() {
        check(
            Expr::Binary(
                Box::new(Expr::Num(1.1)),
                Token::Plus,
                Box::new(Expr::Num(5.0)),
            ),
            Ok(6.1),
        );
    }

    #[test]
    fn simple_variable() {
        check_with_vars(
            Expr::Binary(
                Box::new(Expr::Num(12.0)),
                Token::Mult,
                Box::new(Expr::Var("foo".to_string())),
            ),
            Ok(144.0),
            HashMap::from_iter([("foo".to_string(), Value::Num(12.0))]),
        );
    }
}
