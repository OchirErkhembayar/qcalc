use std::{
    collections::HashMap,
    error::Error,
    f64::consts::{E, PI},
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Rem, Sub},
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
    Assign(String, Expr),
    Undef(Vec<String>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Fn(Function),
    Float(f64),
    Int(i64),
}

impl Value {
    pub fn to_input(&self, name: &str) -> String {
        match self {
            Self::Float(float) => format!("let {} = {}", name, float),
            Self::Int(int) => format!("let {} = {}", name, int),
            Self::Fn(func) => format!(
                "fn {}({}) {}",
                name,
                func.parameters.join(", "),
                func.body.format()
            ),
        }
    }

    fn abs(&mut self) -> Self {
        match self {
            Value::Int(int) => Value::Int(int.abs()),
            Value::Float(float) => Value::Float(float.abs()),
            _ => unreachable!(),
        }
    }

    fn pow(&self, exponent: Value) -> Value {
        match (self, exponent) {
            (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs.pow(rhs as u32)),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs.powf(rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Value::Float((*lhs as f64).powf(rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs.powi(rhs as i32)),
            _ => panic!("Cannot sub non numeric types"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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

    fn call(&self, args: Vec<Value>) -> Result<Value, InterpretError> {
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

    pub fn interpret(&mut self, stmt: Stmt) -> Result<Option<Value>, InterpretError> {
        match stmt {
            Stmt::Assign(name, expr) => {
                let val = self.interpret_expr(&expr)?;
                self.env.insert(name, val.clone()); // Some way to remove this clone?
                Ok(Some(val))
            }
            Stmt::Expr(expr) => {
                let ans = self.interpret_expr(&expr)?;
                self.env.insert("ans".to_string(), ans.clone()); // Some way to remove this clone?
                Ok(Some(ans))
            }
            Stmt::Fn(name, params, body) => {
                self.env.insert(
                    name,
                    Value::Fn(Function::new(params, body, self.env.clone())),
                );
                Ok(None)
            }
            Stmt::Undef(names) => {
                names.iter().for_each(|name| {
                    self.env.remove(name);
                });
                Ok(None)
            }
        }
    }

    fn default_env() -> HashMap<String, Value> {
        HashMap::from_iter([
            ("pi".to_string(), Value::Float(PI)),
            ("e".to_string(), Value::Float(E)),
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

    pub fn interpret_expr(&self, expr: &Expr) -> Result<Value, InterpretError> {
        match expr {
            Expr::Float(float) => Ok(Value::Float(*float)),
            Expr::Int(int) => Ok(Value::Int(*int)),
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
            Expr::Exponent(base, exponent) => {
                let base = self.interpret_expr(base)?;
                let exponent = self.interpret_expr(exponent)?;
                Ok(base.pow(exponent))
            }
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
                            vals.push(self.interpret_expr(arg)?);
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
                        Value::Float(_) | Value::Int(_) => Ok(val.to_owned()),
                    }
                } else {
                    Err(InterpretError::UnknownVariable(var.clone()))
                }
            }
            Expr::Func(func, arg) => {
                let arg = match self.interpret_expr(arg)? {
                    Value::Float(float) => float,
                    Value::Int(int) => int as f64,
                    _ => unreachable!(),
                };
                let val = match func {
                    Func::Sin => arg.sin(),
                    Func::Sinh => arg.sinh(),
                    Func::Asin => arg.asin(),
                    Func::Asinh => arg.asinh(),
                    Func::Cos => arg.cos(),
                    Func::Cosh => arg.cosh(),
                    Func::Acos => arg.acos(),
                    Func::Acosh => arg.acosh(),
                    Func::Tan => arg.tan(),
                    Func::Tanh => arg.tanh(),
                    Func::Atan => arg.atan(),
                    Func::Atanh => arg.tanh(),
                    Func::Ln => arg.ln(),
                    Func::Log(b) => arg.log(*b),
                    Func::Degs => arg.to_degrees(),
                    Func::Rads => arg.to_radians(),
                    Func::Sq => arg.powi(2),
                    Func::Sqrt => arg.sqrt(),
                    Func::Cube => arg.powi(3),
                    Func::Cbrt => arg.cbrt(),
                    Func::Round => arg.round(),
                    Func::Ceil => arg.ceil(),
                    Func::Floor => arg.floor(),
                    Func::Exp => arg.exp(),
                    Func::Exp2 => arg.exp2(),
                    Func::Fract => arg.fract(),
                    Func::Recip => arg.recip(),
                };
                Ok(Value::Float(val))
            }
        }
        .map(|n| {
            if let Value::Float(n) = n {
                if (n.round() - n).abs() < 1e-10 {
                    Value::Float(n.round())
                } else {
                    Value::Float(n)
                }
            } else {
                n
            }
        })
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
                Self::UnknownVariable(v) => format!("Unknown variable {}", v),
                Self::UnknownFunction(f) => format!("Unknown function {}", f),
                Self::UnInvokedFunction(f) => format!("Uninvoked function {}", f),
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
            Self::Float(float) => inner_write(float, f),
            Self::Int(int) => inner_write(int, f),
            Self::Fn(func) => inner_write(func, f),
        }
    }
}

impl Add for Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs + rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
            (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 + rhs),
            (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs + rhs as f64),
            _ => panic!("Cannot add non numeric types"),
        }
    }
}

impl Sub for Value {
    type Output = Value;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs - rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs - rhs),
            (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 - rhs),
            (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs - rhs as f64),
            _ => panic!("Cannot sub non numeric types"),
        }
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs * rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs * rhs),
            (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 * rhs),
            (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs * rhs as f64),
            _ => panic!("Cannot mul non numeric types"),
        }
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Value::Float(lhs as f64 / rhs as f64),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs / rhs),
            (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 / rhs),
            (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs / rhs as f64),
            _ => panic!("Cannot divide non numeric types"),
        }
    }
}

impl Neg for Value {
    type Output = Value;

    fn neg(self) -> Self::Output {
        match &self {
            Value::Int(int) => Value::Int(-*int),
            Value::Float(float) => Value::Float(-*float),
            _ => panic!("Cannot negate non numeric types"),
        }
    }
}

impl Rem for Value {
    type Output = Value;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Value::Float(lhs as f64 % rhs as f64),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs % rhs),
            (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 % rhs),
            (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs % rhs as f64),
            _ => panic!("Cannot calculate rem of non numeric types"),
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

    fn check(expr: Expr, expected: Result<Value, InterpretError>) {
        let interpreter = Interpreter::new();
        let res = interpreter.interpret_expr(&expr);
        assert_eq!(res, expected);
    }

    fn check_with_vars(
        expr: Expr,
        expected: Result<Value, InterpretError>,
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
                Box::new(Expr::Float(1.1)),
                Token::Plus,
                Box::new(Expr::Float(5.0)),
            ),
            Ok(Value::Float(6.1)),
        );
    }

    #[test]
    fn simple_variable() {
        check_with_vars(
            Expr::Binary(
                Box::new(Expr::Float(12.0)),
                Token::Mult,
                Box::new(Expr::Var("foo".to_string())),
            ),
            Ok(Value::Float(144.0)),
            HashMap::from_iter([("foo".to_string(), Value::Float(12.0))]),
        );
    }

    #[test]
    fn function_with_closure() {
        check_with_vars(
            Expr::Call("foo".to_string(), vec![Expr::Var("bar".to_string())]),
            Ok(Value::Float(27.0)),
            HashMap::from_iter([
                (
                    "foo".to_string(),
                    Value::Fn(Function::new(
                        vec!["x".to_string()],
                        Expr::Binary(
                            Box::new(Expr::Exponent(
                                Box::new(Expr::Var("x".to_string())),
                                Box::new(Expr::Float(2.0)),
                            )),
                            Token::Plus,
                            Box::new(Expr::Var("bar".to_string())),
                        ),
                        HashMap::from_iter([("bar".to_string(), Value::Float(2.0))]),
                    )),
                ),
                ("bar".to_string(), Value::Int(5)), // The function uses the closure value
            ]),
        );
    }
}
