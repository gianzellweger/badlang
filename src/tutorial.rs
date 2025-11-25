use std::{
    collections::HashSet,
    io::{self, Stdout},
    time::Duration,
};

use anyhow::{Context, Result};
use badlang_parser as pa;
use crokey::{key, Combiner};
use ms::EditorState;
use ratatui::{
    backend::CrosstermBackend,
    crossterm::{
        event::{self, Event, KeyCode, KeyModifiers},
        execute,
        terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    },
    layout::{Constraint, Layout},
    style::Stylize,
    terminal::{Frame, Terminal},
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph},
};

use crate::microservices as ms;

#[derive(Debug, Clone)]
struct Level {
    preset_text:     &'static str,
    expected_output: &'static str,
    objective:       &'static str,
}

impl Level {
    const fn new(text: &'static str, output: &'static str, objective: &'static str) -> Self {
        Self {
            preset_text: text,
            expected_output: output,
            objective,
        }
    }
}

const LEVELS: [Level; 2] = [
    Level::new(
        r#"comment
    Welcome to the BadLang tutorial! Here you can learn the language and unlock features
    you can then use in the 'playground mode'. Lets start with a simple Hello World program!
    In BadLang, strings are annotated with single quotes ('). This decision was made to save
    space in comparison to double quotes ("). Since Badlang is stack-based, you first need to
    push the string on the stack and then print it using the print keyword. Pushing something
    on the stack is as easy as just writing the literal. Now have fun!
no_comment

@"#,
        "Hello World!",
        "- Print 'Hello World!' to the console",
    ),
    Level::new(
        r#"comment
    Now that you're familiar with the language in it's most basic form, open the shop to buy
    more of the various language features. Once you bought the right tools for the job, you
    can complete this level
no_comment

@"#,
        "42",
        "- Add two numbers to get '42'
  and print it to the console",
    ),
];

const HELP_TEXT: &str = r"Keyboard shortcuts:
- Exit tutorial: ESC
- Run program: Ctrl+r
- Reload state: Ctrl+z
";

pub fn tutorial(state: &mut EditorState) -> Result<()> {
    let mut terminal = setup_terminal().context("setup failed")?;
    terminal.show_cursor().context("Showing cursor failed")?;
    run(&mut terminal, state).context("app loop failed")?;
    restore_terminal(&mut terminal).context("restore terminal failed")?;
    Ok(())
}

fn setup_terminal() -> Result<Terminal<CrosstermBackend<Stdout>>> {
    let mut stdout = io::stdout();
    enable_raw_mode().context("failed to enable raw mode")?;
    execute!(stdout, EnterAlternateScreen).context("unable to enter alternate screen")?;
    Terminal::new(CrosstermBackend::new(stdout)).context("creating terminal failed")
}

fn restore_terminal(terminal: &mut Terminal<CrosstermBackend<Stdout>>) -> Result<()> {
    disable_raw_mode().context("failed to disable raw mode")?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen).context("unable to switch to main screen")?;
    terminal.show_cursor().context("unable to show cursor")
}

fn restore_state(state: &mut EditorState) {
    state.text = LEVELS[(state.level) as usize].preset_text.split('\n').map(ToString::to_string).collect();
    for (y_index, line) in state.text.iter_mut().enumerate() {
        if let Some(x_index) = line.find('@') {
            state.cursor = (x_index as u16, y_index as u16);
            line.remove(x_index);
        }
    }
}

fn run(terminal: &mut Terminal<CrosstermBackend<Stdout>>, state: &mut EditorState) -> Result<()> {
    if state.text.is_empty() {
        restore_state(state);
    }
    loop {
        terminal.draw(|f| render_app(f, state))?;
        match poll_input() {
            Err(err) => {
                let _ = restore_terminal(terminal);
                panic!("{err}")
            }
            Ok(input) => match input {
                Input::None => {}
                Input::Quit => break,
                Input::Enter => {
                    let line = state.text.get_mut(state.cursor.1 as usize).unwrap();
                    let new_line = if usize::from(state.cursor.0) == line.len() {
                        String::new()
                    } else {
                        let res = line[usize::from(state.cursor.0)..].to_string();
                        *line = line[0..usize::from(state.cursor.0)].to_string();
                        res
                    };
                    state.cursor.0 = 0;
                    state.cursor.1 += 1;
                    if state.text.len() == usize::from(state.cursor.1) {
                        state.text.push(new_line);
                    } else {
                        state.text.insert(state.cursor.1 as usize, new_line);
                    }
                }
                Input::Letter(letter) => {
                    if let Some(line) = state.text.get_mut(state.cursor.1 as usize) {
                        state.cursor.0 = state.cursor.0.min(line.len() as u16);
                        if line.len() == usize::from(state.cursor.0) {
                            line.push(letter);
                        } else {
                            line.insert(state.cursor.0 as usize, letter);
                        }
                    }
                    state.cursor.0 += 1;
                }
                Input::Delete => {
                    if state.cursor.0 > 0 {
                        state.cursor.0 -= 1;
                        state.text.get_mut(state.cursor.1 as usize).unwrap().remove(state.cursor.0 as usize);
                    } else if state.cursor.1 > 0 {
                        state.text.remove(state.cursor.1 as usize);
                        state.cursor.1 -= 1;
                        state.cursor.0 = state.text.get(state.cursor.1 as usize).unwrap().len() as u16;
                    }
                }
                Input::Right => {
                    if usize::from(state.cursor.0) < state.text.get(state.cursor.1 as usize).unwrap().len() {
                        state.cursor.0 += 1;
                    }
                }
                Input::Left => {
                    state.cursor.0 = state.cursor.0.min(state.text.get(state.cursor.1 as usize).unwrap().len() as u16);
                    if state.cursor.0 > 0 {
                        state.cursor.0 -= 1;
                    }
                }
                Input::Up => {
                    if state.cursor.1 > 0 {
                        state.cursor.1 -= 1;
                    }
                }
                Input::Down => {
                    if usize::from(state.cursor.1) + 1 < state.text.len() {
                        state.cursor.1 += 1;
                    }
                }
                Input::Run => {
                    let tokens = match pa::parse_string(state.text.join("\n")) {
                        Ok(toks) => toks,
                        Err(error) => {
                            state.console = Err(error.to_string());
                            continue;
                        }
                    };
                    if tokens.into::<HashSet<_>>().union(state.unlocked_features.into::<HashSet<_>>()).len() > state.unlocked_features.len() {
                        state.console = Err("You used features which you have not yet unlocked!".to_string());
                    }
                    let mut output = vec![];
                    match pa::execute_tokens(&tokens, false, &mut output, None) {
                        Ok(stack) => {
                            if !stack.is_empty() {
                                state.console = Err("Stack is not empty! Please empty your stack before the end of the program!".to_string());
                                continue;
                            }
                            let output_string = String::from_utf8(output).unwrap();
                            if output_string == LEVELS[state.level as usize].expected_output {
                                state.level += 1;
                                restore_state(state);
                            }
                            state.console = Ok(output_string);
                        }
                        Err(error) => {
                            state.console = Err(format!("{error}"));
                        }
                    }
                }
                Input::Reload => restore_state(state),
            },
        }
    }
    Ok(())
}

fn render_app(frame: &mut Frame, state: &EditorState) {
    frame.set_cursor(state.text.get(state.cursor.1 as usize).unwrap().len().min((state.cursor.0) as usize) as u16 + 1, state.cursor.1 + 1);
    let editor_block = Block::default().borders(Borders::ALL).title(format!("Editor | Level {}", state.level + 1));
    let console_block = Block::default().borders(Borders::ALL).title("Console");
    let help_block = Block::default().borders(Borders::ALL).title("Help");
    let editor = Paragraph::new(state.text.join("\n")).block(editor_block);
    let console = match &state.console {
        Ok(console) => Paragraph::new(console.as_str()).block(console_block),
        Err(err) => Paragraph::new(Line::from(vec![Span::raw("ERROR: ").red().bold(), Span::raw(err.as_str())])).block(console_block),
    };
    let help = Paragraph::new(HELP_TEXT.to_string() + "\nObjective:\n" + LEVELS[state.level as usize].objective).block(help_block);
    let horizontal_layout = Layout::horizontal([Constraint::Percentage(75), Constraint::Percentage(25)]).split(frame.size());
    let vertical_layout = Layout::vertical([Constraint::Percentage(70), Constraint::Percentage(30)]).split(horizontal_layout[0]);
    frame.render_widget(editor, vertical_layout[0]);
    frame.render_widget(console, vertical_layout[1]);
    frame.render_widget(help, horizontal_layout[1]);
}

#[derive(Debug, PartialEq, Eq)]
enum Input {
    None,
    Quit,
    Delete,
    Enter,
    Up,
    Down,
    Left,
    Right,
    Run,
    Reload,
    Letter(char),
}

fn poll_input() -> Result<Input> {
    let mut combiner = Combiner::default();
    let _ = combiner.enable_combining().unwrap();

    if event::poll(Duration::from_millis(100)).context("event poll failed")? {
        if let Event::Key(key) = event::read().context("event read failed")? {
            let combo = combiner.transform(key).context("combining failed")?;
            Ok(match combo {
                key!(ctrl - r) => Input::Run,
                key!(ctrl - z) => Input::Reload,
                key!(enter) => Input::Enter,
                key!(esc) => Input::Quit,
                key!(backspace) => Input::Delete,
                key!(up) => Input::Up,
                key!(left) => Input::Left,
                key!(down) => Input::Down,
                key!(right) => Input::Right,
                e => {
                    if let Some(c) = e.as_letter()
                        && c.len_utf8() == 1
                    {
                        Input::Letter(c)
                    } else if matches!(e.modifiers, KeyModifiers::SHIFT)
                        && let KeyCode::Char(c) = e.codes.first()
                        && c.len_utf8() == 1
                    {
                        Input::Letter(*c)
                    } else {
                        Input::None
                    }
                }
            })
        } else {
            Ok(Input::None)
        }
    } else {
        Ok(Input::None)
    }
}
