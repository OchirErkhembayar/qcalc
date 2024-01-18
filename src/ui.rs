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

            let list_items = app
                .interpreter
                .env()
                .iter()
                .fold(vec![], |mut acc, (i, val)| {
                    let list_item = ListItem::new(Line::from(Span::styled(
                        format!(" {}: {}", i, val),
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
            let (content, color, border_color) = if let Some(msg) = &app.err {
                (format!("ERROR: {}", msg), Color::Red, Color::Red)
            } else if let Some(msg) = &app.output {
                (format!("Result: {}", msg), Color::Green, Color::Green)
            } else {
                (
                    "Press enter to evaluate expression".to_string(), // Could use lazy_static here
                    Color::LightYellow,
                    Color::White,
                )
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
        let mut render_popup = |message: &str| {
            let popup_layout = Layout::default()
                .direction(Direction::Vertical)
                .constraints([
                    Constraint::Percentage(10),
                    Constraint::Percentage(80),
                    Constraint::Percentage(10),
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
        };
        match popup {
            Popup::Help => {
                let message = "
Available Functions
-------------------
_arg_ should be replaced by an expression eg. ln(2)
_rads_ indicates that the argument should be in radians eg. cos(p)

cos(_rads_)       cosh(_rads_)  acos(_rads_)  acosh(_rads_)  sq(_arg_)    
sin(_rads_)       sinh(_rads_)  asin(_rads_)  asinh(_rads_)  sqrt(_arg_)
tan(_rads_)       tanh(_rads_)  atan(_rads_)  atanh(_rads_)  cube(_arg_)
log_base_(_arg_)  ln(_arg_)     cbrt(_arg_)   ceil(_arg_)    exp(_arg_)
degs(_rads_)      rads(_degs_)  round(_arg_)  floor(_arg_)   exp2(_arg_)
fract(_arg_)      recip(_arg_)

Examples: \"log10(100)\", \"cos(pi)\"

Absolute value: |-123| == 123

Creating variables: \"let x = 5 * 20\"

Shortcuts
---------
(Ctrl s) Save current values and functions
(Ctrl e/v) Reset exprs/vars
(Ctrl x) Delete selected expression
";
                render_popup(message);
            }
            Popup::Function => {
                let message = "
Defining Functions / Variables
----------------
Defining: fn [NAME]([ARG]..) [BODY]
- NAME: Name of the function
- ARGS: Parameter (comma separated identifier)
- BODY: The expression

Example: fn myfun(a, b) a + b^2

Calling: [NAME]([ARG]...)
- NAME: Name of the function
- ARG: Argument (comma separated expression)

Examples:
- myfun(10)
- myfun(cos(p))

Undefining variables and functions: undef([ARG]..)

If existing functions / variables are used in a custom function
then a snapshot of them is taken such that even if they are changed
or redefined, the custom function will use the old values
";
                render_popup(message);
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
            "(Esc) Quit | (Ctrl h) Help | (Ctrl f) Custom fn/var help"
        };
        let help = Paragraph::new(Text::styled(
            message,
            Style::default().fg(Color::LightYellow),
        ))
        .block(help_block);
        f.render_widget(help, chunks[3]);
    }
}
