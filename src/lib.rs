use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use interpreter::{Interpreter, Value};
use parse::Parser;
use ratatui::{prelude::CrosstermBackend, Terminal};
use std::{error::Error, io};
use token::Tokenizer;

use app::{App, Popup};
use event::{Event, EventHandler};
use tui::Tui;

const RC_PATH: &str = ".qcalcrc";

mod app;
mod event;
mod interpreter;
mod parse;
mod token;
mod tui;
mod ui;

pub fn tui() -> Result<(), Box<dyn Error>> {
    let mut tui = Tui::new(
        Terminal::new(CrosstermBackend::new(io::stdout()))?,
        EventHandler::new(250),
    );
    tui.enter().expect("Failed to initialise TUI");
    let mut rc_file = dirs_next::home_dir().expect("Could not find home directory");
    rc_file.push(RC_PATH);
    let mut app = App::new(rc_file);

    while !app.should_quit {
        tui.draw(&mut app)?;

        match tui.events.next()? {
            Event::Tick => {}
            Event::Key(key_event) => update(&mut app, key_event),
            Event::Mouse(_) => {}
            Event::Resize(_, _) => {}
        };
    }

    tui.exit()?;
    Ok(())
}

pub fn eval(input: &str) -> Result<Value, Box<dyn Error>> {
    let mut tokenizer = Tokenizer::new(input.chars().peekable()).peekable();
    let current = tokenizer.next().ok_or("Expected expression")?;
    let stmt = Parser::new(tokenizer, current).parse()?;
    Ok(Interpreter::new().interpret(stmt)?)
}

fn update(app: &mut App, key_event: KeyEvent) {
    if app.popup.is_some() {
        app.popup = None;
    } else {
        match key_event.code {
            KeyCode::Esc => {
                app.should_quit = true;
            }
            KeyCode::Up | KeyCode::Char('k')
                if key_event.modifiers.contains(KeyModifiers::CONTROL) =>
            {
                app.input_select(true);
            }
            KeyCode::Down | KeyCode::Char('j')
                if key_event.modifiers.contains(KeyModifiers::CONTROL) =>
            {
                app.input_select(false);
            }
            KeyCode::Char('s') if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
                app.update_rc();
            }
            KeyCode::Char('e') if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
                app.reset_exprs();
            }
            KeyCode::Char('v') if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
                app.reset_vars();
            }
            KeyCode::Char('h') if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
                app.popup = Some(Popup::Help);
            }
            KeyCode::Char('f') if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
                app.popup = Some(Popup::Function);
            }
            KeyCode::Char('x') if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
                app.remove_expr();
            }
            KeyCode::Enter => app.eval(),
            _ => app.input(key_event.into()),
        }
    }
}

// TODO: Turn this into a decl macro
#[inline(always)]
fn inner_write<T: std::fmt::Display>(val: T, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", val,)
}
