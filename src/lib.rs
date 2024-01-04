use color_eyre::Result;
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use ratatui::{prelude::CrosstermBackend, Terminal};
use std::io;

use app::{App, Popup};
use event::{Event, EventHandler};
use tui::Tui;

mod app;
mod event;
mod interpreter;
mod parse;
mod token;
mod tui;
mod ui;

pub fn run() -> Result<()> {
    let mut tui = Tui::new(
        Terminal::new(CrosstermBackend::new(io::stdout()))?,
        EventHandler::new(250),
    );
    tui.enter().expect("Failed to initialise TUI");
    let mut app = App::new();

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
                if app.output.as_ref().is_some_and(|o| o.is_ok()) {
                    app.save_result_input();
                }
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
            KeyCode::Char('x') if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
                app.remove_expr();
            }
            KeyCode::Char('r') if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
                app.round_result();
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
