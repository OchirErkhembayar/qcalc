use ratatui::{
    prelude::{Constraint, Direction, Frame, Layout, Line},
    style::{Color, Style},
    text::{Span, Text},
    widgets::{Block, Borders, List, ListItem, Padding, Paragraph},
};

use crate::app::App;

pub fn render(app: &mut App, f: &mut Frame) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3),
            Constraint::Min(1),
            Constraint::Length(3),
            Constraint::Length(3),
            Constraint::Length(3),
        ])
        .split(f.size());

    // Title
    {
        let title_block = Block::default()
            .borders(Borders::ALL)
            .padding(Padding::horizontal(2))
            .style(Style::default());
        let title = Paragraph::new(Text::styled(
            "Do some calculations stuff",
            Style::default().fg(Color::LightYellow),
        ))
        .block(title_block);
        f.render_widget(title, chunks[0]);
    }

    // Middle
    {
        let layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(chunks[1]);

        // Value map
        {
            let value_chunks = Layout::default()
                .direction(Direction::Vertical)
                .constraints([Constraint::Length(3), Constraint::Min(1)])
                .split(layout[0]);
            {
                let title_block = Block::default()
                    .borders(Borders::ALL)
                    .padding(Padding::horizontal(2))
                    .style(Style::default());
                let title = Paragraph::new(Text::styled(
                    "Saved values | Use them in your input",
                    Style::default().fg(Color::LightYellow),
                ))
                .block(title_block);
                f.render_widget(title, value_chunks[0]);
            }
            let list_items = app.stored_vals.iter().fold(vec![], |mut acc, (i, val)| {
                let list_item = ListItem::new(Line::from(Span::styled(
                    format!("{}: {}", i, val),
                    Style::default().fg(Color::LightYellow),
                )));
                acc.push(list_item);
                acc
            });
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
                    format!("Press enter to evaluate expression"),
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

        f.render_widget(result, chunks[2]);
        f.render_widget(app.input.widget(), chunks[3]);
    }

    // Footer
    {
        let help_block = Block::default()
            .borders(Borders::ALL)
            .padding(Padding::horizontal(1))
            .style(Style::default());
        let help = Paragraph::new(Text::styled(
            "(Ctrl s) Store result in variable",
            Style::default().fg(Color::LightYellow),
        ))
        .block(help_block);
        f.render_widget(help, chunks[4]);
    }
}
