use std::{
    collections::HashMap,
    error::Error,
    f64::consts::{E, PI},
    fmt::Display,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Not, Rem, Shl, Shr, Sub},
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

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expr(Expr),
    Assign(String, Expr),
    Undef(Vec<String>),
}
impl Stmt {
    pub(crate) fn format(&self) -> String {
        match self {
            Self::Undef(variables) => format!("undef({})", variables.join(", ")),
            Self::Expr(expr) => expr.format(),
            Self::Assign(name, expr) => format!("let {} = {}", name, expr.format()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Fn(Function),
    Float(f64),
    Int(i64),
    String(String),
    Bool(bool),
    Unit,
}

impl Value {
    pub fn to_input(&self, name: &str) -> String {
        match self {
            Self::Float(float) => format!("let {} = {}", name, float),
            Self::Int(int) => format!("let {} = {}", name, int),
            Self::String(string) => string.to_owned(),
            Self::Unit => "()".to_string(),
            Self::Bool(bool) => if *bool { "true" } else { "false" }.to_string(),
            Self::Fn(func) => format!(
                "let {} = |{}| {}",
                name,
                func.parameters.join(", "),
                func.body.format()
            ),
        }
    }

    fn abs(&self) -> Self {
        match self {
            Value::Int(int) => Value::Int(int.abs()),
            Value::Float(float) => Value::Float(float.abs()),
            _ => unreachable!(),
        }
    }

    fn pow(&self, rhs: Self) -> Self {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs.pow(rhs as u32)),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs.powf(rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Value::Float((*lhs as f64).powf(rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs.powi(rhs as i32)),
            _ => panic!("Cannot add non numeric types"),
        }
    }

    fn truthy(&self) -> bool {
        match self {
            Self::Int(int) => *int > 0,
            Self::Float(float) => *float > 0.0,
            Self::Bool(bool) => *bool,
            Self::String(string) => !string.is_empty(),
            Self::Unit => false,
            Self::Fn(_) => true,
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
    Uncallable(Value),
    UnInvokedFunction(String),
    WrongArity(Value, usize, usize),
    InvalidArgument(String),
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

    pub fn interpret(&mut self, stmt: Stmt) -> Result<Value, InterpretError> {
        match stmt {
            Stmt::Assign(name, expr) => {
                let val = self.interpret_expr(&expr)?;
                self.env.insert(name, val.clone()); // Some way to remove this clone?
                Ok(val)
            }
            Stmt::Expr(expr) => {
                let ans = self.interpret_expr(&expr)?;
                self.env.insert("ans".to_string(), ans.clone()); // Some way to remove this clone?
                Ok(ans)
            }
            Stmt::Undef(names) => {
                names.iter().for_each(|name| {
                    self.env.remove(name);
                });
                Ok(Value::Unit)
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

    pub fn interpret_expr(&self, expr: &Expr) -> Result<Value, InterpretError> {
        match expr {
            Expr::Float(float) => Ok(Value::Float(*float)),
            Expr::Int(int) => Ok(Value::Int(*int)),
            Expr::Bool(bool) => Ok(Value::Bool(*bool)),
            Expr::Binary(left, operator, right) => {
                let left = self.interpret_expr(left)?;
                let right = self.interpret_expr(right)?;
                let val = match operator {
                    Token::Plus => left + right,
                    Token::Minus => left - right,
                    Token::Mult => left * right,
                    Token::Div => left / right,
                    Token::Mod => left % right,
                    Token::BitAnd => (left & right)?,
                    Token::Pipe => (left | right)?,
                    Token::BitXor => (left ^ right)?,
                    Token::Shl => (left << right)?,
                    Token::Shr => (left >> right)?,
                    Token::Pow => left.pow(right),
                    Token::And => Value::Bool(left.truthy() && right.truthy()),
                    Token::Or => Value::Bool(left.truthy() || right.truthy()),
                    Token::Eq => Value::Bool(left == right),
                    Token::Ne => Value::Bool(left != right),
                    Token::Gte => Value::Bool(left >= right),
                    Token::Gt => Value::Bool(left > right),
                    Token::Lte => Value::Bool(left <= right),
                    Token::Lt => Value::Bool(left < right),
                    _ => unreachable!(),
                };
                Ok(val)
            }
            Expr::Fun(params, body) => Ok(Value::Fn(Function::new(
                params.to_owned(),
                *body.to_owned(),
                self.env.clone(),
            ))),
            Expr::Grouping(expr) => self.interpret_expr(expr),
            Expr::Unary(expr, operator) => match operator {
                Token::Not => self.interpret_expr(expr)?.not(),
                Token::Minus => Ok(self.interpret_expr(expr)?.neg()),
                _ => unreachable!(),
            },
            Expr::Call(name, args) => {
                let function = self.interpret_expr(name)?;
                if let Value::Fn(func) = function {
                    let arity = func.arity;
                    if args.len() != arity {
                        Err(InterpretError::WrongArity(
                            Value::Fn(func),
                            args.len(),
                            arity,
                        ))
                    } else {
                        let mut vals = vec![];
                        for arg in args.iter() {
                            vals.push(self.interpret_expr(arg)?);
                        }
                        func.call(vals)
                    }
                } else {
                    Err(InterpretError::Uncallable(function))
                }
            }
            Expr::Var(var) => {
                if let Some(val) = self.env.get(var) {
                    Ok(val.to_owned())
                } else {
                    Err(InterpretError::UnknownVariable(var.clone()))
                }
            }
            Expr::Func(func, args) => {
                let mut arguments = vec![];
                for arg in args.iter() {
                    let arg = match self.interpret_expr(arg)? {
                        Value::Float(float) => float,
                        Value::Int(int) => int as f64,
                        _ => unreachable!(),
                    };
                    arguments.push(arg);
                }
                let arity = func.arity();
                if arguments.len() != arity {
                    return Err(InterpretError::WrongArity(
                        Value::String(func.to_string()),
                        arguments.len(),
                        arity,
                    ));
                }
                let val = match func {
                    Func::Abs => return Ok(self.interpret_expr(&args[0])?.abs()),
                    Func::Sin => arguments[0].sin(),
                    Func::Sinh => arguments[0].sinh(),
                    Func::Asin => arguments[0].asin(),
                    Func::Asinh => arguments[0].asinh(),
                    Func::Cos => arguments[0].cos(),
                    Func::Cosh => arguments[0].cosh(),
                    Func::Acos => arguments[0].acos(),
                    Func::Acosh => arguments[0].acosh(),
                    Func::Tan => arguments[0].tan(),
                    Func::Tanh => arguments[0].tanh(),
                    Func::Atan => arguments[0].atan(),
                    Func::Atanh => arguments[0].tanh(),
                    Func::Ln => arguments[0].ln(),
                    Func::Log => arguments[1].log(arguments[0]),
                    Func::Degs => arguments[0].to_degrees(),
                    Func::Rads => arguments[0].to_radians(),
                    Func::Sq => arguments[0].powi(2),
                    Func::Sqrt => arguments[0].sqrt(),
                    Func::Cube => arguments[0].powi(3),
                    Func::Cbrt => arguments[0].cbrt(),
                    Func::Round => arguments[0].round(),
                    Func::Ceil => arguments[0].ceil(),
                    Func::Floor => arguments[0].floor(),
                    Func::Exp => arguments[0].exp(),
                    Func::Exp2 => arguments[0].exp2(),
                    Func::Fract => arguments[0].fract(),
                    Func::Recip => arguments[0].recip(),
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
                Self::Uncallable(f) => format!("Unknown function {}", f),
                Self::UnInvokedFunction(f) => format!("Uninvoked function {}", f),
                Self::InvalidArgument(m) => m.clone(),
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
            Self::String(string) => inner_write(string, f),
            Self::Unit => inner_write("()", f),
            Self::Bool(bool) => inner_write(bool, f),
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

impl BitAnd for Value {
    type Output = Result<Value, InterpretError>;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs & rhs)),
            _ => Err(InterpretError::InvalidArgument(
                "& cannot be applied to this data type".to_string(),
            )),
        }
    }
}

impl BitXor for Value {
    type Output = Result<Value, InterpretError>;

    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs ^ rhs)),
            _ => Err(InterpretError::InvalidArgument(
                "^ cannot be applied to this data type".to_string(),
            )),
        }
    }
}

impl BitOr for Value {
    type Output = Result<Value, InterpretError>;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs | rhs)),
            _ => Err(InterpretError::InvalidArgument(
                "| cannot be applied to this data type".to_string(),
            )),
        }
    }
}

impl Shl for Value {
    type Output = Result<Value, InterpretError>;

    fn shl(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs << rhs)),
            _ => Err(InterpretError::InvalidArgument(
                "<< cannot be applied to this data type".to_string(),
            )),
        }
    }
}

impl Shr for Value {
    type Output = Result<Value, InterpretError>;

    fn shr(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs >> rhs)),
            _ => Err(InterpretError::InvalidArgument(
                ">> cannot be applied to this data type".to_string(),
            )),
        }
    }
}

impl Not for Value {
    type Output = Result<Value, InterpretError>;

    fn not(self) -> Self::Output {
        match self {
            Value::Int(int) => Ok(Value::Int(!int)),
            _ => Err(InterpretError::InvalidArgument(
                "! cannot be applied to this data type".to_string(),
            )),
        }
    }
}

impl PartialOrd for Value {
    fn gt(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => lhs > rhs,
            (Value::Float(lhs), Value::Float(rhs)) => lhs > rhs,
            (Value::Int(lhs), Value::Float(rhs)) => *lhs as f64 > *rhs,
            (Value::Float(lhs), Value::Int(rhs)) => *lhs > *rhs as f64,
            _ => panic!("Cannot calculate rem of non numeric types"),
        }
    }

    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let res = match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => lhs.cmp(rhs),
            (Value::Float(lhs), Value::Float(rhs)) => return (*lhs).partial_cmp(rhs),
            (Value::Int(lhs), Value::Float(rhs)) => return (*lhs as f64).partial_cmp(rhs),
            (Value::Float(lhs), Value::Int(rhs)) => return (*lhs).partial_cmp(&(*rhs as f64)),
            (Value::String(lhs), Value::String(rhs)) => lhs.len().cmp(&rhs.len()),
            _ => return None,
        };
        Some(res)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "|{}| {}", self.parameters.join(", "), self.body)
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Stmt::Assign(name, expr) => format!("let {} = {}", name, expr),
                Stmt::Expr(expr) => return inner_write(expr, f),
                Stmt::Undef(vars) => format!("undef({})", vars.join(", ")),
            }
        )
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
            Expr::Call(
                Box::new(Expr::Var("foo".to_string())),
                vec![Expr::Var("bar".to_string())],
            ),
            Ok(Value::Int(12)),
            HashMap::from_iter([
                (
                    "foo".to_string(),
                    Value::Fn(Function::new(
                        vec!["x".to_string()],
                        Expr::Binary(
                            Box::new(Expr::Binary(
                                Box::new(Expr::Var("x".to_string())),
                                Token::Mult,
                                Box::new(Expr::Int(2)),
                            )),
                            Token::Plus,
                            Box::new(Expr::Var("bar".to_string())),
                        ),
                        HashMap::from_iter([("bar".to_string(), Value::Int(2))]),
                    )),
                ),
                ("bar".to_string(), Value::Int(5)), // The function uses the closure value
            ]),
        );
    }
}
