use std::{
    io::{self, Stdout},
    time::Duration,
};

use anyhow::{Context, Result};
use crossterm::{
    event::{self, Event, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{
    backend::CrosstermBackend,
    terminal::{Frame, Terminal},
    widgets::{Paragraph, Block, Borders},
};

pub fn tutorial(level: &mut u8) -> Result<()> {
    let mut terminal = setup_terminal().context("setup failed")?;
    run(&mut terminal, level).context("app loop failed")?;
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

fn run(terminal: &mut Terminal<CrosstermBackend<Stdout>>, level: &mut u8) -> Result<()> {
    loop {
        terminal.draw(|f| render_app(f, level))?;
        if ()? {
            break;
        }
    }
    Ok(())
}

fn render_app(frame: &mut Frame, level: &mut u8) {
    let window = match level {
        0 => {Block::new().borders(Borders::ALL)},
        _ => unreachable!(),
    }.title(format!("Level {}", *level + 1));
    frame.render_widget(window, frame.size());
}

#[derive(Debug, PartialEq, Eq)]
enum Input {
    None,
    Quit,
    Letter(char)
}

fn poll_input() -> Result<Input> {
    if event::poll(Duration::from_millis(250)).context("event poll failed")? {
        if let Event::Key(key) = event::read().context("event read failed")? {
            Ok(match key.code {
             KeyCode::Esc =>
                Input::Quit,
                KeyCode::Char(c) => Input::Letter(c),
                _ => Input::None
            })
        }
    } else {
        
    Ok(Input::None)
    }
}
