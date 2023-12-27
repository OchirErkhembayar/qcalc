use color_eyre::Result;
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use ratatui::{prelude::CrosstermBackend, Terminal};
use std::io;

use app::App;
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
    match key_event.code {
        KeyCode::Char('q') => {
            app.should_quit = true;
        }
        KeyCode::Up if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
            app.input_select(true);
        }
        KeyCode::Down if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
            app.input_select(false);
        }
        KeyCode::Char('s') if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
            if app.output.as_ref().is_some_and(|o| o.is_ok()) {
                app.save_result();
            }
        }
        KeyCode::Enter => app.eval(),
        _ => app.input(key_event.into()),
    }
}
