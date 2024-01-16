use std::error::Error;

use ratatui::{
    style::{Color, Style},
    widgets::{Block, Borders, Padding},
};
use tui_textarea::{Input, TextArea};

use crate::{
    interpreter::{Interpreter, Stmt, Value},
    parse::{Expr, ParseErr, Parser},
    token::{Token, Tokenizer},
};

pub enum InputType {
    Expr,
    SetVar,
}

pub enum Popup {
    Help,
    Function,
}

pub struct App<'ta> {
    pub input: TextArea<'ta>,
    pub input_type: InputType,
    pub output: Option<Result<f64, Box<dyn Error>>>,
    pub interpreter: Interpreter,
    pub expr_history: Vec<Expr>,
    pub expr_selector: usize,
    pub should_quit: bool,
    pub popup: Option<Popup>,
}

impl<'ta> App<'ta> {
    pub fn new() -> Self {
        Self {
            input: textarea(None, None, None),
            input_type: InputType::Expr,
            output: None,
            interpreter: Interpreter::new(),
            expr_history: Vec::new(),
            expr_selector: 0,
            should_quit: false,
            popup: None,
        }
    }

    pub fn reset_vars(&mut self) {
        self.interpreter.reset_vars();
    }

    pub fn reset_exprs(&mut self) {
        self.expr_history.clear();
    }

    pub fn input(&mut self, input: Input) {
        self.input.input(input);
    }

    pub fn eval(&mut self) {
        match self.input_type {
            InputType::Expr => {
                let input = &self.input.lines()[0];
                // TODO: Move the tokenizer into the parser so that we're not doing
                // this unnecessary allocation. Figure out how to handle end of expressions
                // without the use of semicolons (or implicitly add it in but then if someone
                // enters one it would terminate their expression which is weird)
                let mut tokens = Tokenizer::new(input.chars().collect::<Vec<_>>().as_slice())
                    .collect::<Vec<_>>();
                tokens.push(Token::Eoe);
                let res = Parser::new(tokens).parse();
                match res {
                    Ok(expr) => {
                        match expr {
                            Stmt::Expr(expr) => {
                                match self.interpreter.interpret_expr(&expr) {
                                    Ok(val) => {
                                        if !self.expr_history.contains(&expr) {
                                            self.expr_history.push(expr);
                                        }
                                        if self.expr_selector == self.expr_history.len() {
                                            self.expr_selector += 1;
                                        }
                                        // Only reset input if we successfully evaluate
                                        self.input = textarea(None, None, None);
                                        self.interpreter.define("ans".to_string(), Value::Num(val));
                                        self.output = Some(Ok(val));
                                    }
                                    Err(err) => self.output = Some(Err(Box::new(err))),
                                }
                            }
                            Stmt::Fn(name, parameters, body) => {
                                self.interpreter.declare_function(name, parameters, body);
                                self.input = textarea(None, None, None);
                            }
                        }
                    }
                    Err(err) => self.output = Some(Err(Box::new(err))),
                };
            }
            InputType::SetVar => {
                let name = self.input.lines()[0].trim().to_string();
                if name.is_empty() {
                    self.output = Some(Err(Box::new(ParseErr::new(
                        Token::Ident(name),
                        "Variable name cannot be empty",
                    ))));
                    self.input = textarea(None, None, None);
                    self.input_type = InputType::Expr;
                } else if !name.chars().all(|c| c.is_ascii_alphabetic()) {
                    self.output = Some(Err(Box::new(ParseErr::new(
                        Token::Ident(name),
                        "Variable names must be letters",
                    ))));
                    self.input = textarea(None, None, None);
                    self.input_type = InputType::Expr;
                } else {
                    // We know that this is a safe unwrap because the above two cases
                    // would reset and we can't enter this mode unless we have a
                    // valid output
                    let output = self.output.as_ref().unwrap().as_ref().unwrap();
                    self.interpreter.define(name, Value::Num(*output));
                    self.input = textarea(None, None, None);
                    self.input_type = InputType::Expr;
                }
            }
        }
    }

    // true == select up | false == select down
    pub fn input_select(&mut self, up: bool) {
        if self.expr_history.is_empty() {
            return;
        }
        if up {
            if self.expr_selector > 0 {
                self.expr_selector -= 1;
            }
        } else if self.expr_selector > self.expr_history.len() - 1 {
            self.expr_selector -= 1;
        } else if self.expr_selector < self.expr_history.len() - 1 {
            self.expr_selector += 1;
        }
        let expr = &self.expr_history[self.expr_selector];
        let string = expr.format();
        self.input = textarea(Some(string), None, None);
    }

    // Set the input mode to typing to saving the output as a variable
    pub fn save_result_input(&mut self) {
        self.input = textarea(
            None,
            Some("Select one letter"),
            Some("Enter a variable name"),
        );
        self.input_type = InputType::SetVar;
    }

    pub fn remove_expr(&mut self) {
        if self.expr_selector < self.expr_history.len() {
            self.expr_history.remove(self.expr_selector);
            if !self.expr_history.is_empty() && self.expr_history.len() <= self.expr_selector {
                self.expr_selector -= 1;
            }
        }
    }

    pub fn round_result(&mut self) {
        if let Some(Ok(output)) = &mut self.output {
            *output = output.round();
        }
    }
}

fn textarea<'a>(
    content: Option<String>,
    placeholder: Option<&'a str>,
    title: Option<&'a str>,
) -> TextArea<'a> {
    let mut textarea = if let Some(content) = content {
        TextArea::new(Vec::from([content]))
    } else {
        TextArea::default()
    };
    textarea.set_placeholder_text(placeholder.unwrap_or("Start typing..."));
    textarea.set_block(
        Block::default()
            .title(title.unwrap_or("Input"))
            .style(Style::default().fg(Color::White))
            .borders(Borders::ALL)
            .padding(Padding::horizontal(1)),
    );
    textarea.set_cursor_line_style(Style::default());
    textarea.move_cursor(tui_textarea::CursorMove::Down);
    textarea.move_cursor(tui_textarea::CursorMove::End);
    textarea
}

#[cfg(test)]
mod tests {
    use super::*;

    fn input_and_evaluate(app: &mut App, input: &str) {
        app.input = textarea(Some(input.to_string()), None, None);
        app.eval();
    }

    fn assert_output(app: &App, expected: f64) {
        // Yuck.
        if let Some(ref output) = app.output {
            assert_eq!(*output.as_ref().unwrap(), expected);
        } else {
            panic!("Not equal");
        }
    }

    #[test]
    fn create_and_call_function() {
        let mut app = App::new();
        input_and_evaluate(&mut app, "fn foo(x, y) x + y");
        input_and_evaluate(&mut app, "foo (1, 2)");
        assert!(app.output.is_some_and(|r| r.is_ok_and(|n| n == 3.0)));
    }

    #[test]
    fn save_variable() {
        let mut app = App::new();
        input_and_evaluate(&mut app, "12");
        app.save_result_input();
        input_and_evaluate(&mut app, "foo");
        input_and_evaluate(&mut app, "foo * 12");
        assert_output(&app, 144.0);
    }

    #[test]
    fn test_empty_input() {
        let mut app = App::new();
        input_and_evaluate(&mut app, "");
        assert!(app.output.is_some_and(|o| o.is_err()));
    }

    #[test]
    fn test_built_in_fns() {
        let mut app = App::new();
        let input_and_ans = [("sq(2)", 4.0), ("sqrt(16)", 4.0), ("cube(2)", 8.0), ("cbrt(8)", 2.0)];

        input_and_ans.iter().for_each(|(input, exp)| {
            input_and_evaluate(&mut app, input);
            assert_output(&app, *exp);
        });
    }
}
