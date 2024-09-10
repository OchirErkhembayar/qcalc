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
        .split(f.area());

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
        f.render_widget(&app.input, chunks[2])
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
                .split(f.area());

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
        let message = match popup {
            Popup::Help => {
                "
Built in Functions
-------------------
_arg_ should be replaced by an expression eg. ln(2)
_rads_ indicates that the argument should be in radians eg. cos(p)

cos(_rads_)        cosh(_rads_)  acos(_rads_)  acosh(_rads_)    sq(_arg_)    
sin(_rads_)        sinh(_rads_)  asin(_rads_)  asinh(_rads_)    sqrt(_arg_)
tan(_rads_)        tanh(_rads_)  atan(_rads_)  atanh(_rads_)    cube(_arg_)
log(_base_, _arg_) ln(_arg_)     cbrt(_arg_)   ceil(_arg_)      exp(_arg_)
degs(_rads_)       rads(_degs_)  round(_arg_)  floor(_arg_)     exp2(_arg_)
fract(_arg_)       recip(_arg_)  abs(_arg_)    factorial(_arg_) range(_arg_, _arg_)
elem(_arg_)        min([_arg_])  max([_arg_])  quadr(_arg_, _arg_, _arg_)

Examples: \"log10(100)\", \"cos(pi)\"

Shortcuts
---------
(Ctrl v) Reset vars
(Ctrl x) Delete selected expression
"
            }
            Popup::Function => {
                "
Defining Functions / Variables
------------------------------
Syntax: let [NAME] = |[ARG]..| [BODY]
- NAME: Name of the function
- ARGS: Parameter (comma separated identifier)
- BODY: The expression

Example: let myfun = |a, b| a + b ** 2
         let foo = cos(pi)

Calling: [NAME]([ARG]...)
- NAME: Name of the function
- ARG: Argument (comma separated expression)

Examples:
- myfun(10)
- myfun(cos(p))
- foo // Where foo is a variable

Undefining variables and functions: undef([ARG]..)

If existing functions / variables are used in a custom function
then a snapshot of them is taken such that even if they are changed
or redefined, the custom function will use the old values
"
            }
            Popup::Language => {
                "
Language Details
----------------
Types
    - integers 10
    - floats 12.0
    - booleans true
    - strings \"Hello, World!\"
    - lists [1, true, \"Hi\", 2.0]
    - tuples {false, 1, 2}
    - NaN, nil
Control flow
    if [BOOL_EXPR] then [THEN_EXPR] else [ELSE_EXPR]
    eg. if true then \"foo\" else \"bar\"
Lists
    map(_list_, _callback_)
        eg. map([1, 2, 3], |x| x ** 2) == [1, 4, 9]
    filter(_list_, _callback_)
        eg. filter([1, 2, 3], |x| odd(x))
    sum(_list_)
        eg. sum([1, 2, 3, 4, 5]) == 15
    fold(_list_, _callback_, _initial_value_)
        eg. fold([1, 2, 3], |acc, curr| acc + curr, 1) == 6
    min(_list_)
        eg. min([1, 2, 3]) == 1
    max(_list_)
        eg. max([1, 2, 3]) == 3
"
            }
        };
        render_popup(message);
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
            "(Esc) Quit | (Ctrl h) Help | (Ctrl l) Syntax | (Ctrl f) Custom fn/var help"
        };
        let help = Paragraph::new(Text::styled(
            message,
            Style::default().fg(Color::LightYellow),
        ))
        .block(help_block);
        f.render_widget(help, chunks[3]);
    }
}
