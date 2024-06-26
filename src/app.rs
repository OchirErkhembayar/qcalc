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
    interpreter::{Interpreter, Stmt, Value},
    parse::Parser,
    token::Tokenizer,
};

pub enum Popup {
    Help,
    Function,
    Language,
}

pub struct App<'ta> {
    pub input: TextArea<'ta>,
    pub output: Option<Value>,
    pub err: Option<String>,
    pub interpreter: Interpreter,
    pub expr_history: Vec<Stmt>,
    pub expr_selector: usize,
    pub should_quit: bool,
    pub popup: Option<Popup>,
    should_save: bool,
    rc_file: PathBuf,
}

impl<'ta> App<'ta> {
    pub fn new(rc_file: PathBuf, should_save: bool) -> Self {
        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
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
            should_save,
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
                self.interpreter
                    .interpret(res)
                    .expect("Runtime error in RC file. Either edit it or delete it and restart");
            }
        });
    }

    fn update_rc(&mut self) {
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
                    Ok(_) => {}
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
                if !self.expr_history.contains(&stmt) {
                    self.expr_history.push(stmt.clone());
                }
                if self.expr_selector == self.expr_history.len() {
                    self.expr_selector += 1;
                }
                match self.interpreter.interpret(stmt) {
                    Ok(res) => {
                        self.set_output(res);
                        self.input = textarea(None, None, None);
                        // Hack for testing. Allow user to customise later
                        if self.should_save {
                            self.update_rc();
                        }
                    }
                    Err(err) => self.set_err(err.to_string()),
                }
            }
            Err(err) => self.set_err(err.to_string()),
        };
    }

    fn set_output(&mut self, msg: Value) {
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
        App::new(PathBuf::from(TEST_FILE), false)
    }

    fn new_app_empty_rc<'a>() -> App<'a> {
        File::create(TEST_FILE).expect("Failed to create test file");
        App::new(PathBuf::from(TEST_FILE), true)
    }

    fn input_and_evaluate(app: &mut App, input: &str) {
        app.input = textarea(Some(input.to_string()), None, None);
        app.eval();
    }

    fn assert_output(app: &App, expected: Value) {
        // Yuck.
        if let Some(output) = &app.output {
            assert_eq!(output, &expected);
        } else {
            panic!("Error: {:?}", app.err);
        }
    }

    #[test]
    fn create_and_call_function() {
        let mut app = new_app();
        input_and_evaluate(&mut app, "let foo = |x, y| x + y");
        input_and_evaluate(&mut app, "foo (1, 2)");
        assert!(app.output.is_some_and(|r| r == Value::Int(3)));
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

        input_and_ans.into_iter().for_each(|(input, exp)| {
            input_and_evaluate(&mut app, input);
            assert_output(&app, Value::Float(exp));
        });
    }

    #[test]
    fn test_assignment() {
        let mut app = new_app();

        input_and_evaluate(&mut app, "let foo = sqrt(144)");
        input_and_evaluate(&mut app, "foo");
        assert_output(&app, Value::Float(12.0));

        input_and_evaluate(&mut app, "let foo = -2 ** 3");
        input_and_evaluate(&mut app, "foo");
        assert_output(&app, Value::Int(-8));
    }

    #[test]
    fn test_rc_file() {
        let mut app = new_app_empty_rc();
        input_and_evaluate(&mut app, "let x = 5");
        input_and_evaluate(&mut app, "let foo = |a, b| a + b * 5");
        drop(app);
        let mut app = new_app();
        input_and_evaluate(&mut app, "x");
        assert_output(&app, Value::Int(5));
        input_and_evaluate(&mut app, "foo(2, 3)");
        assert_output(&app, Value::Int(17));
    }

    #[test]
    fn test_bin_hex() {
        let mut app = new_app();

        input_and_evaluate(&mut app, "0x1f + 0b110 / 2");
        assert_output(&app, Value::Float(34.0));
    }

    #[test]
    fn test_bitwise_operators() {
        let mut app = new_app();

        input_and_evaluate(&mut app, "0b1010 & 0b0111");
        assert_output(&app, Value::Int(0b0010));

        input_and_evaluate(&mut app, "0b1010 | 0b0111");
        assert_output(&app, Value::Int(0b1111));

        input_and_evaluate(&mut app, "0b1010 ^ 0b0111");
        assert_output(&app, Value::Int(0b1101));

        input_and_evaluate(&mut app, "!45");
        assert_output(&app, Value::Int(-46));

        input_and_evaluate(&mut app, "0b11 | 0x1 << 2");
        assert_output(&app, Value::Int(7));
    }

    #[test]
    fn higher_order() {
        let mut app = new_app();

        input_and_evaluate(&mut app, "let bar = |x| |y| x ** y");
        input_and_evaluate(&mut app, "bar(3)(2)");
        assert_output(&app, Value::Int(9));
    }

    #[test]
    fn test_if_else() {
        let mut app = new_app();

        input_and_evaluate(&mut app, "if 0 then 3 % 5 else \"this is the answer\"");
        assert_output(&app, Value::String("this is the answer".to_string()));
    }

    #[test]
    fn test_list() {
        let mut app = new_app();

        input_and_evaluate(&mut app, "[1, \"foo\", 1 == 1, []]");
        assert_output(
            &app,
            Value::List(vec![
                Value::Int(1),
                Value::String("foo".to_string()),
                Value::Bool(true),
                Value::List(vec![]),
            ]),
        )
    }

    #[test]
    fn test_iterator() {
        let mut app = new_app();

        input_and_evaluate(
            &mut app,
            "fold([0, 1, 2, 3], |acc, x| if x > 1 then acc + x else acc, 10)",
        );
        assert_output(&app, Value::Int(15));

        input_and_evaluate(&mut app, "map([1, 2, 3], |x| even(x))");
        assert_output(
            &app,
            Value::List(vec![
                Value::Bool(false),
                Value::Bool(true),
                Value::Bool(false),
            ]),
        );

        input_and_evaluate(
            &mut app,
            "filter([[1, 2], [2, 3], [3, 4]], |arr| sum(arr) > 3)",
        );
        assert_output(
            &app,
            Value::List(vec![
                Value::List(vec![Value::Int(2), Value::Int(3)]),
                Value::List(vec![Value::Int(3), Value::Int(4)]),
            ]),
        );
    }

    #[test]
    fn test_factorial() {
        let mut app = new_app();

        input_and_evaluate(&mut app, "factorial(15)");
        assert_output(&app, Value::Int(1307674368000));
    }

    #[test]
    fn test_range() {
        let mut app = new_app();

        input_and_evaluate(&mut app, "range(0, 3)");
        assert_output(
            &app,
            Value::List(vec![Value::Int(0), Value::Int(1), Value::Int(2)]),
        );
    }
}
