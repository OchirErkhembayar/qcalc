use std::collections::HashMap;

use ratatui::{
    style::{Color, Style},
    widgets::{Block, Borders, Padding},
};
use tui_textarea::{Input, Key, TextArea};

use crate::{
    interpreter::interpret,
    parse::{Expr, ParseErr, Parser},
    token::{Token, Tokenizer},
};

pub enum InputType {
    Expr,
    SetVar,
}

pub struct App<'ta> {
    pub input: TextArea<'ta>,
    pub input_type: InputType,
    pub output: Option<Result<f64, ParseErr>>,
    pub stored_vals: HashMap<char, f64>,
    pub expr_history: Vec<Expr>,
    pub expr_selector: usize,
    pub should_quit: bool,
}

impl<'ta> App<'ta> {
    pub fn new() -> Self {
        Self {
            input: textarea(None, None),
            input_type: InputType::Expr,
            output: None,
            stored_vals: HashMap::new(),
            expr_history: Vec::new(),
            expr_selector: 0,
            should_quit: false,
        }
    }

    pub fn input(&mut self, input: Input) {
        match self.input_type {
            InputType::Expr => self.input.input(input),
            InputType::SetVar => {
                if let Key::Char(c) = input.key {
                    // TODO: Wtf is this
                    let output = self
                        .output
                        .as_ref()
                        .unwrap()
                        .to_owned()
                        .as_ref()
                        .to_owned()
                        .unwrap();
                    self.stored_vals.insert(c, *output);
                    self.input = textarea(None, None);
                    self.input_type = InputType::Expr;
                }
                true
            }
        };
    }

    pub fn eval(&mut self) {
        let input = &self.input.lines()[0];
        let mut tokens =
            Tokenizer::new(input.chars().collect::<Vec<_>>().as_slice()).collect::<Vec<_>>();
        tokens.push(Token::Eoe);
        let res = Parser::new(tokens, self.stored_vals.clone()).parse();
        let output = match res {
            Ok(expr) => {
                let val = interpret(expr.clone()); // TODO: Don't clone this
                if !self.expr_history.contains(&expr) {
                    self.expr_history.push(expr);
                }
                if self.expr_selector == self.stored_vals.len() {
                    self.expr_selector += 1;
                }
                Ok(val)
            }
            Err(err) => Err(err),
        };
        self.output = Some(output);
        self.input = textarea(None, None);
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
        } else {
            if self.expr_selector > self.expr_history.len() - 1 {
                self.expr_selector -= 1;
            } else if self.expr_selector < self.expr_history.len() - 1 {
                self.expr_selector += 1;
            }
        }
        let expr = &self.expr_history[self.expr_selector];
        let string = expr.format();
        self.input = textarea(Some(string), None);
    }

    pub fn save_result(&mut self) {
        self.input = textarea(None, Some("Select a variable name"));
        self.input_type = InputType::SetVar;
    }
}

fn textarea<'a>(content: Option<String>, placeholder: Option<&'a str>) -> TextArea<'a> {
    let mut textarea = if let Some(content) = content {
        TextArea::new(Vec::from([content]))
    } else {
        TextArea::default()
    };
    textarea.set_placeholder_text(placeholder.unwrap_or("Start typing..."));
    textarea.set_block(
        Block::default()
            .title("Input")
            .style(Style::default().fg(Color::White))
            .borders(Borders::ALL)
            .padding(Padding::horizontal(1)),
    );
    textarea.set_cursor_line_style(Style::default());
    textarea.move_cursor(tui_textarea::CursorMove::Down);
    textarea.move_cursor(tui_textarea::CursorMove::End);
    textarea
}
