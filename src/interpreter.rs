use std::{
    collections::HashMap,
    f64::consts::{E, PI},
};

const CONSTS: [(char, f64); 2] = [('p', PI), ('e', E)];

#[derive(Debug)]
pub struct Interpreter {
    vars: HashMap<char, f64>,
}

#[derive(Debug)]
pub struct Fn {}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            vars: HashMap::from_iter(CONSTS),
        }
    }

    pub fn reset_vars(&mut self) {
        self.vars = HashMap::from_iter(CONSTS);
    }

    pub fn save_variable(&mut self, var: char, val: f64) {
        self.vars.insert(var, val);
    }

    pub fn vars_into(&self) -> HashMap<char, f64> {
        self.vars.clone()
    }

    pub fn vars(&self) -> &HashMap<char, f64> {
        &self.vars
    }

    pub fn vars_len(&self) -> usize {
        self.vars.len()
    }
}
