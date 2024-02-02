use std::{
    fs::{File, OpenOptions},
    io::{Read, Write},
    path::PathBuf,
};

use ratatui::{
    style::{Color, Style},
    widgets::{Block, Borders, Padding},
};
use tui_textarea::{Input, TextArea};

use crate::{
    interpreter::{Interpreter, Stmt},
    parse::{Expr, Parser},
    token::Tokenizer,
};

pub enum Popup {
    Help,
    Function,
}

pub struct App<'ta> {
    pub input: TextArea<'ta>,
    pub output: Option<String>,
    pub err: Option<String>,
    pub interpreter: Interpreter,
    pub expr_history: Vec<Expr>,
    pub expr_selector: usize,
    pub should_quit: bool,
    pub popup: Option<Popup>,
    rc_file: PathBuf,
}

impl<'ta> App<'ta> {
    pub fn new(rc_file: PathBuf) -> Self {
        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&rc_file)
            .expect("Failed to create RC file");
        let mut app = Self {
            input: textarea(None, None, None),
            output: None,
            err: None,
            interpreter: Interpreter::new(),
            expr_history: Vec::new(),
            expr_selector: 0,
            should_quit: false,
            popup: None,
            rc_file,
        };
        app.run_commands(file);
        app
    }

    fn run_commands(&mut self, mut file: File) {
        // Okay to fail here because there's nothing the user
        // can do expect go and fix or delete their rc file
        let mut buf = String::new();
        file.read_to_string(&mut buf)
            .expect("Failed to read from RC file");
        buf.lines().for_each(|line| {
            let mut tokenizer = Tokenizer::new(line.chars().peekable()).peekable();
            if let Some(token) = tokenizer.next() {
                let res = Parser::new(tokenizer, token)
                    .parse()
                    .expect("Invalid syntax in RC file");
                match res {
                    Stmt::Fn(name, params, body) => {
                        self.interpreter.declare_function(name, params, body)
                    }
                    Stmt::Assign(name, expr) => {
                        self.interpreter.define(
                            name,
                            self.interpreter.interpret_expr(&expr).unwrap_or_else(|_| {
                                panic!("RC file: {} not found", &self.rc_file.display())
                            }),
                        );
                    }
                    _ => {}
                }
            }
        });
    }

    pub fn update_rc(&mut self) {
        let commands = self
            .interpreter
            .env()
            .iter()
            .fold(Vec::new(), |mut acc, (string, val)| {
                acc.push(val.to_input(string));
                acc
            })
            .join("\n");
        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(true)
            .open(&self.rc_file)
            .map_err(|e| format!("ERROR: Failed to open rc file, {}", e));
        match file {
            Ok(mut file) => {
                match file
                    .write_all(commands.as_bytes())
                    .map_err(|e| format!("ERROR: Failed to write to rc file, {}", e))
                {
                    Ok(_) => self.set_output("Success".to_string()),
                    Err(err) => self.set_err(err.to_string()),
                }
            }
            Err(err) => self.set_err(err.to_string()),
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
        let input = &self.input.lines()[0];
        let mut tokenizer = Tokenizer::new(input.chars().peekable()).peekable();
        if tokenizer.peek().is_none() {
            return;
        }
        let current = tokenizer.next().unwrap();
        match Parser::new(tokenizer, current).parse() {
            Ok(stmt) => {
                if let Stmt::Expr(expr) = &stmt {
                    if !self.expr_history.contains(expr) {
                        self.expr_history.push(expr.clone());
                    }
                    if self.expr_selector == self.expr_history.len() {
                        self.expr_selector += 1;
                    }
                }
                match self.interpreter.interpret(stmt) {
                    Ok(res) => {
                        if let Some(res) = res {
                            self.set_output(res.to_string());
                        }
                        self.input = textarea(None, None, None);
                    }
                    Err(err) => self.set_err(err.to_string()),
                }
            }
            Err(err) => self.set_err(err.to_string()),
        };
    }

    fn set_output(&mut self, msg: String) {
        self.output = Some(msg);
        self.err = None;
    }

    fn set_err(&mut self, msg: String) {
        self.err = Some(msg);
        self.output = None;
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

    pub fn remove_expr(&mut self) {
        if self.expr_selector < self.expr_history.len() {
            self.expr_history.remove(self.expr_selector);
            if !self.expr_history.is_empty() && self.expr_history.len() <= self.expr_selector {
                self.expr_selector -= 1;
            }
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

    const TEST_FILE: &str = "./test";

    fn new_app<'a>() -> App<'a> {
        App::new(PathBuf::from(TEST_FILE))
    }

    fn new_app_empty_rc<'a>() -> App<'a> {
        File::create(TEST_FILE).expect("Failed to create test file");
        App::new(PathBuf::from(TEST_FILE))
    }

    fn input_and_evaluate(app: &mut App, input: &str) {
        app.input = textarea(Some(input.to_string()), None, None);
        app.eval();
    }

    fn assert_output(app: &App, expected: f64) {
        // Yuck.
        if let Some(output) = &app.output {
            assert_eq!(output.parse::<f64>().unwrap(), expected);
        } else {
            panic!("Error: {:?}", app.err);
        }
    }

    #[test]
    fn create_and_call_function() {
        let mut app = new_app();
        input_and_evaluate(&mut app, "fn foo(x, y) x + y");
        input_and_evaluate(&mut app, "foo (1, 2)");
        assert!(app.output.is_some_and(|r| r == "3"));
    }

    #[test]
    fn test_built_in_fns() {
        let mut app = new_app();
        let input_and_ans = [
            ("sq(2)", 4.0),
            ("sqrt(16)", 4.0),
            ("cube(2)", 8.0),
            ("cbrt(8)", 2.0),
            ("round(2.49999)", 2.0),
            ("cos(rads(180))", -1.0),
            ("floor(2.99)", 2.0),
            ("recip(2)", 0.5),
        ];

        input_and_ans.iter().for_each(|(input, exp)| {
            input_and_evaluate(&mut app, input);
            assert_output(&app, *exp);
        });
    }

    #[test]
    fn test_assignment() {
        let mut app = new_app();

        input_and_evaluate(&mut app, "let foo = sqrt(144)");
        input_and_evaluate(&mut app, "foo");
        assert_output(&app, 12.0);
    }

    #[test]
    fn test_rc_file() {
        let mut app = new_app_empty_rc();
        input_and_evaluate(&mut app, "let x = 5");
        input_and_evaluate(&mut app, "fn foo(a, b) a + b * 5");
        app.update_rc();
        drop(app);
        let mut app = new_app();
        input_and_evaluate(&mut app, "x");
        assert_output(&app, 5.0);
        input_and_evaluate(&mut app, "foo(2, 3)");
        assert_output(&app, 17.0);
    }

    #[test]
    fn test_bin_hex() {
        let mut app = new_app();

        input_and_evaluate(&mut app, "0x1f + 0b110 / 2");
        assert_output(&app, 34.0);
    }
}
