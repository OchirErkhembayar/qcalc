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

#[derive(Debug, Clone)]
pub enum Value {
    Fn(Function),
    Float(f64),
    Int(i64),
    String(String),
    Bool(bool),
    List(Vec<Value>),
    Tuple(Vec<Value>),
    Unit,
    Nil,
    NaN,
}

impl Value {
    pub fn to_input(&self, name: &str) -> String {
        match self {
            Self::Float(float) => format!("let {} = {}", name, float),
            Self::Int(int) => format!("let {} = {}", name, int),
            Self::String(string) => format!("let {} = \"{}\"", name, string),
            Self::Unit => "()".to_string(),
            Self::Bool(bool) => if *bool { "true" } else { "false" }.to_string(),
            Self::Fn(func) => format!(
                "let {} = |{}| {}",
                name,
                func.parameters.join(", "),
                func.body.format()
            ),
            Self::List(elems) => format!(
                "let {} = [{}]",
                name,
                elems
                    .iter()
                    // This is quite dangerous and error-prone. Considering changing how this works
                    .map(|e| match e {
                        Value::String(string) => format!("\"{}\"", string),
                        v => v.to_string(),
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Tuple(elems) => format!(
                "let {} = {{{}}}",
                name,
                elems
                    .iter()
                    .map(|e| match e {
                        Value::String(string) => format!("\"{}\"", string),
                        v => v.to_string(),
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Nil => "nil".to_string(),
            Self::NaN => "NaN".to_string(),
        }
    }

    fn abs(&self) -> Result<Self, InterpretError> {
        Ok(match self {
            Value::Int(int) => Value::Int(int.abs()),
            Value::Float(float) => Value::Float(float.abs()),
            _ => {
                return Err(InterpretError::InvalidArgument(
                    "abs expects a number type as an argument".to_string(),
                ))
            }
        })
    }

    fn pow(&self, rhs: Self) -> Result<Self, InterpretError> {
        let res = match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs.pow(rhs as u32)),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs.powf(rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Value::Float((*lhs as f64).powf(rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs.powi(rhs as i32)),
            _ => {
                return Err(InterpretError::RuntimeError(
                    "Cannot pow non numeric types".to_string(),
                ))
            }
        };
        Ok(res)
    }

    fn truthy(&self) -> bool {
        match self {
            Self::Int(int) => *int > 0,
            Self::Float(float) => *float > 0.0,
            Self::Bool(bool) => *bool,
            Self::String(string) => !string.is_empty(),
            Self::Unit => false,
            Self::Nil => false,
            Self::Fn(_) => true,
            Self::List(elems) => !elems.is_empty(),
            Self::Tuple(elems) => !elems.is_empty(),
            Self::NaN => false,
        }
    }

    fn to_float(&self) -> Result<f64, InterpretError> {
        if let Value::Float(float) = self.clone() {
            Ok(float)
        } else if let Value::Int(int) = self {
            Ok(*int as f64)
        } else if let Value::NaN = self {
            Ok(f64::NAN)
        } else {
            Err(InterpretError::InvalidArgument(format!(
                "Expected float, got: {}",
                self
            )))
        }
    }

    fn to_list(&self) -> Result<Vec<Self>, InterpretError> {
        if let Value::List(list) = self.clone() {
            Ok(list)
        } else {
            Err(InterpretError::InvalidArgument(format!(
                "Expected list, got: {}",
                self
            )))
        }
    }

    fn to_callable(&self) -> Result<Function, InterpretError> {
        if let Value::Fn(function) = self.clone() {
            Ok(function)
        } else {
            Err(InterpretError::InvalidArgument(format!(
                "Expected function, got: {}",
                self
            )))
        }
    }

    fn to_int(&self) -> Result<i64, InterpretError> {
        if let Value::Int(int) = self.clone() {
            Ok(int)
        } else {
            Err(InterpretError::InvalidArgument(format!(
                "Expected int, got: {}",
                self
            )))
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
    RuntimeError(String),
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
        let len = args.len();
        for (i, arg) in args.into_iter().enumerate() {
            let param = self
                .parameters
                .get(i)
                .ok_or(InterpretError::InvalidArgument(format!(
                    "Expected {} arguments",
                    len,
                )))?;
            if param == "_" {
                continue;
            }
            interpreter.define(param.clone(), arg)
        }
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
            Expr::List(elems) => {
                let mut elements = vec![];
                for elem in elems.iter() {
                    elements.push(self.interpret_expr(elem)?);
                }
                Ok(Value::List(elements))
            }
            Expr::Tuple(elems) => {
                let mut elements = vec![];
                for elem in elems.iter() {
                    elements.push(self.interpret_expr(elem)?);
                }
                Ok(Value::Tuple(elements))
            }
            Expr::Nan => Ok(Value::NaN),
            Expr::Nil => Ok(Value::Nil),
            Expr::Int(int) => Ok(Value::Int(*int)),
            Expr::String(string) => Ok(Value::String(string.clone())),
            Expr::Bool(bool) => Ok(Value::Bool(*bool)),
            Expr::If(cond, then, else_expr) => {
                if self.interpret_expr(cond)?.truthy() {
                    self.interpret_expr(then)
                } else {
                    self.interpret_expr(else_expr)
                }
            }
            Expr::Binary(left, operator, right) => {
                let left = self.interpret_expr(left)?;
                let right = self.interpret_expr(right)?;
                let val = match operator {
                    Token::Plus => (left + right)?,
                    Token::Minus => (left - right)?,
                    Token::Mult => (left * right)?,
                    Token::Div => (left / right)?,
                    Token::Mod => (left % right)?,
                    Token::BitAnd => (left & right)?,
                    Token::Pipe => (left | right)?,
                    Token::BitXor => (left ^ right)?,
                    Token::Shl => (left << right)?,
                    Token::Shr => (left >> right)?,
                    Token::Pow => left.pow(right)?,
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
                Token::Minus => Ok(self.interpret_expr(expr)?.neg()?),
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
                    arguments.push(self.interpret_expr(arg)?);
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
                    Func::Abs => return self.interpret_expr(&args[0])?.abs(),
                    Func::Sin => arguments[0].to_float()?.sin(),
                    Func::Sinh => arguments[0].to_float()?.sinh(),
                    Func::Asin => arguments[0].to_float()?.asin(),
                    Func::Asinh => arguments[0].to_float()?.asinh(),
                    Func::Cos => arguments[0].to_float()?.cos(),
                    Func::Cosh => arguments[0].to_float()?.cosh(),
                    Func::Acos => arguments[0].to_float()?.acos(),
                    Func::Acosh => arguments[0].to_float()?.acosh(),
                    Func::Tan => arguments[0].to_float()?.tan(),
                    Func::Tanh => arguments[0].to_float()?.tanh(),
                    Func::Atan => arguments[0].to_float()?.atan(),
                    Func::Atanh => arguments[0].to_float()?.tanh(),
                    Func::Ln => arguments[0].to_float()?.ln(),
                    Func::Log => arguments[1].to_float()?.log(arguments[0].to_float()?),
                    Func::Degs => arguments[0].to_float()?.to_degrees(),
                    Func::Rads => arguments[0].to_float()?.to_radians(),
                    Func::Sq => arguments[0].to_float()?.powi(2),
                    Func::Sqrt => arguments[0].to_float()?.sqrt(),
                    Func::Cube => arguments[0].to_float()?.powi(3),
                    Func::Cbrt => arguments[0].to_float()?.cbrt(),
                    Func::Round => arguments[0].to_float()?.round(),
                    Func::Ceil => arguments[0].to_float()?.ceil(),
                    Func::Floor => arguments[0].to_float()?.floor(),
                    Func::Exp => arguments[0].to_float()?.exp(),
                    Func::Exp2 => arguments[0].to_float()?.exp2(),
                    Func::Fract => arguments[0].to_float()?.fract(),
                    Func::Recip => arguments[0].to_float()?.recip(),
                    Func::Quadr => {
                        let a = arguments[0].to_float()?;
                        let b = arguments[1].to_float()?;
                        let c = arguments[2].to_float()?;
                        let n = (b.powi(2) - 4.0 * a * c).powf(0.5);
                        let pos = (-b + n) / (2.0 * a);
                        let neg = (-b - n) / (2.0 * a);
                        return Ok(Value::List(vec![Value::Float(pos), Value::Float(neg)]));
                    }
                    Func::Map => {
                        let list = arguments[0].to_list()?;
                        let callable = arguments[1].to_callable()?;
                        let mut result = vec![];
                        for elem in list.into_iter() {
                            result.push(callable.call(vec![elem])?);
                        }
                        return Ok(Value::List(result));
                    }
                    Func::Sum => {
                        let list = arguments[0].to_list()?;
                        let mut result = 0_f64;
                        for elem in list.into_iter() {
                            result += elem.to_float()?;
                        }
                        result
                    }
                    Func::Fold => {
                        let list = arguments[0].to_list()?;
                        let callable = arguments[1].to_callable()?;
                        let mut acc = arguments[2].clone();
                        for elem in list.into_iter() {
                            acc = callable.call(vec![acc, elem])?;
                        }
                        return Ok(acc);
                    }
                    Func::Filter => {
                        let list = arguments[0].to_list()?;
                        let callable = arguments[1].to_callable()?;
                        let mut result = vec![];
                        for elem in list.into_iter() {
                            if callable.call(vec![elem.clone()])?.truthy() {
                                result.push(elem);
                            }
                        }
                        return Ok(Value::List(result));
                    }
                    Func::Even => {
                        return Ok(Value::Bool(arguments[0].to_int()? % 2 == 0));
                    }
                    Func::Odd => {
                        return Ok(Value::Bool(arguments[0].to_int()? % 2 != 0));
                    }
                    Func::Fact => {
                        let arg = arguments[0].to_int()?;
                        if arg > 20 {
                            return Err(InterpretError::InvalidArgument(format!(
                                "Value too large {}",
                                arg
                            )));
                        } else {
                            return Ok(Value::Int(get_fact(arg)));
                        }
                    }
                    Func::Range => {
                        let start = arguments[0].to_int()?;
                        let end = arguments[1].to_int()?;
                        if start < 0 || end <= start {
                            return Err(InterpretError::InvalidArgument(
                                "Invalid range".to_string(),
                            ));
                        } else {
                            let vec = (start..=end).map(Value::Int).collect::<Vec<_>>();
                            return Ok(Value::List(vec));
                        }
                    }
                    Func::Elem => {
                        let elements = match &arguments[0] {
                            Value::List(elems) => elems.to_owned(),
                            Value::Tuple(elems) => elems.to_owned(),
                            _ => {
                                return Err(InterpretError::InvalidArgument(
                                    "Cannot call elem on this data type".to_string(),
                                ))
                            }
                        };
                        let index = arguments[1].to_int()?;
                        if index < 0 {
                            return Err(InterpretError::InvalidArgument(
                                "Indexes start at 0".to_string(),
                            ));
                        }

                        let element =
                            elements
                                .get(index as usize)
                                .ok_or(InterpretError::RuntimeError(format!(
                                    "Index {} out of range 0..{}",
                                    index,
                                    elements.len() - 1
                                )))?;

                        return Ok(element.to_owned());
                    }
                    Func::Min => {
                        let list = arguments[0].to_list()?;
                        return Ok(list.into_iter().min().unwrap_or(Value::Nil));
                    }
                    Func::Max => {
                        let list = arguments[0].to_list()?;
                        return Ok(list.into_iter().max().unwrap_or(Value::Nil));
                    }
                };
                Ok(Value::Float(val))
            }
        }
        .map(|n| {
            if let Value::Float(n) = n {
                if !n.is_nan() && !n.is_infinite() {
                    if n.is_subnormal() {
                        Value::Float(n.round())
                    } else {
                        Value::Float(n)
                    }
                } else {
                    Value::NaN
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

fn get_fact(arg: i64) -> i64 {
    match arg {
        1 => 1,
        2 => 2,
        3 => 6,
        4 => 24,
        5 => 120,
        6 => 720,
        7 => 5040,
        8 => 40320,
        9 => 362880,
        10 => 3628800,
        11 => 39916800,
        12 => 479001600,
        13 => 6227020800,
        14 => 87178291200,
        15 => 1307674368000,
        16 => 20922789888000,
        17 => 355687428096000,
        18 => 6402373705728000,
        19 => 121645100408832000,
        20 => 2432902008176640000,
        _ => unreachable!(),
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
                Self::RuntimeError(m) => m.clone(),
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
            Self::Nil => inner_write("nil", f),
            Self::NaN => inner_write("NaN", f),
            Self::Bool(bool) => inner_write(bool, f),
            Self::List(elems) => inner_write(
                format!(
                    "[{}]",
                    elems
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                f,
            ),
            Self::Tuple(elems) => inner_write(
                format!(
                    "{{{}}}",
                    elems
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                f,
            ),
        }
    }
}

impl Add for Value {
    type Output = Result<Value, InterpretError>;

    fn add(self, rhs: Self) -> Self::Output {
        let res = match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs + rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
            (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 + rhs),
            (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs + rhs as f64),
            (Value::List(mut lhs), Value::List(mut rhs)) => {
                lhs.append(&mut rhs);
                Value::List(lhs)
            }
            (Value::List(mut lhs), other) => {
                lhs.push(other);
                Value::List(lhs)
            }
            (Value::String(mut lhs), Value::String(rhs)) => {
                lhs.push_str(&rhs);
                Value::String(lhs)
            }
            (Value::NaN, _) | (_, Value::NaN) => Value::NaN,
            _ => {
                return Err(InterpretError::InvalidArgument(
                    "Cannot add these types together".to_string(),
                ))
            }
        };

        Ok(res)
    }
}

impl Sub for Value {
    type Output = Result<Value, InterpretError>;

    fn sub(self, rhs: Self) -> Self::Output {
        let res = match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs - rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs - rhs),
            (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 - rhs),
            (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs - rhs as f64),
            (Value::NaN, _) | (_, Value::NaN) => Value::NaN,
            _ => {
                return Err(InterpretError::InvalidArgument(
                    "Cannot sub non numeric types".to_string(),
                ))
            }
        };

        Ok(res)
    }
}

impl Mul for Value {
    type Output = Result<Value, InterpretError>;

    fn mul(self, rhs: Self) -> Self::Output {
        let res = match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs * rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs * rhs),
            (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 * rhs),
            (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs * rhs as f64),
            (Value::NaN, _) | (_, Value::NaN) => Value::NaN,
            _ => {
                return Err(InterpretError::InvalidArgument(
                    "Cannot mul non numeric types".to_string(),
                ))
            }
        };

        Ok(res)
    }
}

impl Div for Value {
    type Output = Result<Value, InterpretError>;

    fn div(self, rhs: Self) -> Self::Output {
        let res = match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Value::Float(lhs as f64 / rhs as f64),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs / rhs),
            (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 / rhs),
            (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs / rhs as f64),
            (Value::NaN, _) | (_, Value::NaN) => Value::NaN,
            _ => {
                return Err(InterpretError::InvalidArgument(
                    "Cannot divide non numeric types".to_string(),
                ))
            }
        };

        Ok(res)
    }
}

impl Neg for Value {
    type Output = Result<Value, InterpretError>;

    fn neg(self) -> Self::Output {
        match &self {
            Value::Int(int) => Ok(Value::Int(-*int)),
            Value::Float(float) => Ok(Value::Float(-*float)),
            _ => Err(InterpretError::RuntimeError(
                "Cannot negate non numeric types".to_string(),
            )),
        }
    }
}

impl Rem for Value {
    type Output = Result<Value, InterpretError>;

    fn rem(self, rhs: Self) -> Self::Output {
        let res = match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Value::Float(lhs as f64 % rhs as f64),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs % rhs),
            (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 % rhs),
            (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs % rhs as f64),
            _ => {
                return Err(InterpretError::InvalidArgument(
                    "Cannot calculate rem of non numeric types".to_string(),
                ))
            }
        };

        Ok(res)
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
            Value::Bool(bool) => Ok(Value::Bool(!bool)),
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
            _ => false,
        }
    }

    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => lhs == rhs,
            (Value::Float(lhs), Value::Float(rhs)) => lhs == rhs,
            (Value::Int(lhs), Value::Float(rhs)) => *lhs as f64 == *rhs,
            (Value::Float(lhs), Value::Int(rhs)) => *lhs == *rhs as f64,
            (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
            (Value::Fn(fl), Value::Fn(fr)) => fl == fr,
            (Value::Fn(_), _) => false,
            (Value::Float(_), _) => false,
            (Value::Int(_), _) => false,
            (Value::String(_), _) => false,
            (Value::Bool(bl), Value::Bool(br)) => bl == br,
            (Value::Bool(_), _) => false,
            (Value::List(ll), Value::List(lr)) => ll == lr,
            (Value::List(_), _) => false,
            (Value::Tuple(tl), Value::Tuple(tr)) => tl == tr,
            (Value::Tuple(_), _) => false,
            (Value::Unit, Value::Unit) => true,
            (Value::Unit, _) => false,
            (Value::Nil, Value::Nil) => true,
            (Value::Nil, _) => false,
            (Value::NaN, Value::NaN) => true,
            (Value::NaN, _) => false,
        }
    }
}

impl Eq for Value {}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap_or(std::cmp::Ordering::Equal)
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

    #[test]
    fn nan_for_nan() {
        check(Expr::Func(Func::Sqrt, vec![Expr::Int(-1)]), Ok(Value::NaN));
    }

    #[test]
    fn non_numeric_operations() {
        check(
            Expr::Binary(
                Box::new(Expr::String("foo".to_string())),
                Token::Pow,
                Box::new(Expr::String("bar".to_string())),
            ),
            Err(InterpretError::RuntimeError(
                "Cannot pow non numeric types".to_string(),
            )),
        );

        check(
            Expr::Unary(Box::new(Expr::String("foo".to_string())), Token::Minus),
            Err(InterpretError::RuntimeError(
                "Cannot negate non numeric types".to_string(),
            )),
        );
    }

    #[test]
    fn partial_cmp_values() {
        assert!(Value::Float(2.0) == Value::Int(2));
        let var = "foo".to_string();
        let fn_1 = Value::Fn(Function::new(
            vec![var.clone()],
            Expr::Var(var.clone()),
            HashMap::new(),
        ));
        let fn_2 = Value::Fn(Function::new(
            vec![var.clone()],
            Expr::Var(var.clone()),
            HashMap::new(),
        ));
        assert!(fn_1 == fn_2);

        assert!(Value::Float(0.0) == Value::Int(0));
        assert!(Value::Int(0) == Value::Float(0.0));
    }
}
