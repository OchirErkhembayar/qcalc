use ratatui::{
    prelude::{Constraint, Direction, Frame, Layout, Line},
    style::{Color, Style},
    text::{Span, Text},
    widgets::{Block, Borders, Clear, List, ListItem, Padding, Paragraph, Wrap},
};

use crate::app::{App, Popup};

pub fn render(app: &mut App, f: &mut Frame) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Min(1),
            Constraint::Length(3),
            Constraint::Length(3),
            Constraint::Length(3),
        ])
        .split(f.size());

    // Middle
    {
        let layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(chunks[0]);

        // Value map
        {
            let value_chunks = Layout::default()
                .direction(Direction::Vertical)
                .constraints([Constraint::Length(3), Constraint::Min(1)])
                .split(layout[0]);
            let title_block = Block::default()
                .borders(Borders::ALL)
                .padding(Padding::horizontal(2))
                .style(Style::default());
            let title = Paragraph::new(Text::styled(
                "Saved values/functions",
                Style::default().fg(Color::LightYellow),
            ))
            .block(title_block);
            f.render_widget(title, value_chunks[0]);

            let list_items = app.interpreter.env().as_ref().borrow().vars().iter().fold(
                vec![],
                |mut acc, (i, val)| {
                    let list_item = ListItem::new(Line::from(Span::styled(
                        format!(" {}: {}", i, val),
                        Style::default().fg(Color::LightYellow),
                    )));
                    acc.push(list_item);
                    acc
                },
            );
            let output_list = List::new(list_items).block(Block::default().borders(Borders::LEFT));
            f.render_widget(output_list, value_chunks[1]);
        }

        // Expressions
        {
            let expr_chunks = Layout::default()
                .direction(Direction::Vertical)
                .constraints([Constraint::Length(3), Constraint::Min(1)])
                .split(layout[1]);

            let title_block = Block::default()
                .borders(Borders::ALL)
                .padding(Padding::horizontal(2))
                .style(Style::default());
            let title = Paragraph::new(Text::styled(
                "Expressions | (Ctrl ↑ / ↓) select",
                Style::default().fg(Color::LightYellow),
            ))
            .block(title_block);
            f.render_widget(title, expr_chunks[0]);

            let list_items =
                app.expr_history
                    .iter()
                    .enumerate()
                    .fold(vec![], |mut acc, (i, expr)| {
                        let mut style = Style::default().fg(Color::LightYellow);
                        if app.expr_selector == i {
                            style = style.bg(Color::Blue);
                        }
                        let list_item = ListItem::new(Line::from(Span::styled(
                            format!("{}. {}", i + 1, expr),
                            style,
                        )));
                        acc.push(list_item);
                        acc
                    });
            let expr_list = List::new(list_items).block(Block::default().borders(Borders::LEFT));
            f.render_widget(expr_list, expr_chunks[1]);
        }
    }

    // Result + Input
    {
        let result = {
            let (content, color, border_color) = match &app.output {
                Some(output) => match output {
                    Ok(num) => (format!("Result: {}", num), Color::Green, Color::Green),
                    Err(err) => (format!("{}", err), Color::Red, Color::Red),
                },
                None => (
                    "Press enter to evaluate expression".to_string(),
                    Color::LightYellow,
                    Color::White,
                ),
            };
            let output_block = Block::default()
                .borders(Borders::ALL)
                .padding(Padding::horizontal(2))
                .style(Style::default().fg(border_color));
            Paragraph::new(Text::styled(content, Style::default().fg(color))).block(output_block)
        };

        f.render_widget(result, chunks[1]);
        f.render_widget(app.input.widget(), chunks[2]);
    }

    if let Some(popup) = &app.popup {
        match popup {
            Popup::Help => {
                let popup_layout = Layout::default()
                    .direction(Direction::Vertical)
                    .constraints([
                        Constraint::Percentage(15),
                        Constraint::Percentage(70),
                        Constraint::Percentage(15),
                    ])
                    .split(f.size());

                let area = Layout::default()
                    .direction(Direction::Vertical)
                    .constraints([
                        Constraint::Percentage(10),
                        Constraint::Percentage(80),
                        Constraint::Percentage(10),
                    ])
                    .split(popup_layout[1])[1];

                let message = "
Available Functions
-------------------
_arg_ should be replaced by an expression eg. ln(2)
_rads_ indicates that the argument should be in radians eg. cos(p)

cos(_rads_)       cosh(_rads_)
sin(_rads_)       sinh(_rads_)
tan(_rads_)       tanh(_rads_)
log_base_(_arg_)  ln(_arg_)

The result of a successful eval is stored in q

Shortcuts
---------
(Ctrl r) Round result to the nearest integer
(Ctrl s) Store result
(Ctrl e/v) Reset exprs/vars
(Ctrl x) Delete selected expression
";
                let message_block = Block::default()
                    .title("Help")
                    .borders(Borders::ALL)
                    .padding(Padding::horizontal(3))
                    .style(Style::default().fg(Color::White));
                let message_text = Paragraph::new(message)
                    .wrap(Wrap::default())
                    .block(message_block);
                f.render_widget(Clear, area);
                f.render_widget(message_text, area);
            }
        }
    }

    // Footer
    {
        let help_block = Block::default()
            .borders(Borders::ALL)
            .padding(Padding::horizontal(1))
            .style(Style::default());
        let message = if app.popup.is_some() {
            "(Esc) Back"
        } else {
            "(Esc) Quit | (Ctrl h) Help"
        };
        let help = Paragraph::new(Text::styled(
            message,
            Style::default().fg(Color::LightYellow),
        ))
        .block(help_block);
        f.render_widget(help, chunks[3]);
    }
}
