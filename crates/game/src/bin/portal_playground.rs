use euclid::point2;
use itertools::Itertools;
use rgb::RGB8;
use game::{graphics::Graphics, set_up_input_thread};
use utility::*;
use terminal_rendering::*;
use std::io::{stdin, stdout, Write};
use std::thread;
use std::time::{Duration, Instant};
use termion::screen::{IntoAlternateScreen, ToAlternateScreen};
use termion::{
    event::{Event, Key},
    input::{MouseTerminal, TermRead},
    raw::IntoRawMode,
};

struct GameState {
    width: u16,
    height: u16,

    mouse_pos: Option<WorldSquare>,
    last_frame: Option<Frame>,
}
impl GameState {
    pub fn new(width: u16, height: u16) -> Self {
        GameState {
            width,
            height,
            mouse_pos: None,
            last_frame: None,
        }
    }
    pub fn process_event(&mut self, event: Event) {
        match event {
            Event::Key(key) => match key {
                Key::Char('q') => std::process::exit(0),
                // Key::Backspace => todo!(),
                // Key::Left => todo!(),
                // Key::Right => todo!(),
                // Key::Up => todo!(),
                // Key::Down => todo!(),
                // Key::Home => todo!(),
                // Key::End => todo!(),
                // Key::PageUp => todo!(),
                // Key::PageDown => todo!(),
                // Key::BackTab => todo!(),
                // Key::Delete => todo!(),
                // Key::Insert => todo!(),
                // Key::F(_) => todo!(),
                // Key::Alt(_) => todo!(),
                // Key::Ctrl(_) => todo!(),
                // Key::Null => todo!(),
                // Key::Esc => todo!(),
                _ => {}
            },
            Event::Mouse(mouse_event) => todo!(),
            Event::Unsupported(items) => todo!(),
        }
    }
    pub fn render(&self) -> Frame {
        Frame {
            grid: (0..self.width)
                .map(|col| {
                    (0..self.height)
                        .map(|row| {
                            let x = col;
                            let y = self.height - row - 1;
                            DoubleGlyph::solid_color(
                                board_color(point2(x as i32, y as i32)).unwrap(),
                            )
                        })
                        .collect_vec()
                })
                .collect_vec(),
        }
    }
}

fn grey(x: u8) -> RGB8 {
    RGB8::new(x, x, x)
}

struct Frame {
    grid: Vec<Vec<DoubleGlyph>>,
}

impl Frame {
    pub fn width(&self) -> usize {
        self.grid[0].len()
    }
    pub fn height(&self) -> usize {
        self.grid.len()
    }
    pub fn diff(&self, maybe_old_frame: &Option<Frame>) -> String {
        let mut output = String::new();

        let rows = self.grid.len();
        let cols = self.grid[0].len();
        let mut prev_written_row_col: Option<[usize; 2]> = None;
        for row in 0..rows {
            for col in 0..cols {
                let new_glyphs = self.grid[row][col].to_string();

                if let Some(old_frame) = maybe_old_frame {
                    let old_glyphs = old_frame.grid[row][col].to_string();
                    let this_square_is_same = new_glyphs == old_glyphs;
                    if this_square_is_same {
                        continue;
                    }
                }
                let just_next_horizontally = prev_written_row_col
                    .is_some_and(|[prev_row, prev_col]| row == prev_row && prev_col == col - 1);

                let should_do_linewrap = prev_written_row_col.is_some_and(|[prev_row, prev_col]| {
                    prev_row + 1 == row && col == 0 && prev_col + 1 == cols
                });
                let directly_below = prev_written_row_col
                    .is_some_and(|[prev_row, prev_col]| prev_row + 1 == row && col == prev_col);

                if just_next_horizontally {
                    // Do nothing
                } else if should_do_linewrap {
                    output += "\n\r";
                } else if directly_below {
                    output += "\n";
                } else {
                    output +=
                        &termion::cursor::Goto((col + 1) as u16, (row + 1) as u16).to_string();
                }

                output += &new_glyphs.to_string();

                prev_written_row_col = Some([row, col]);
            }
        }
        output
    }
}

fn draw_frame(writable: &mut impl Write, new_frame: &Frame, maybe_old_frame: &Option<Frame>) {
    write!(writable, "{}", new_frame.diff(maybe_old_frame));
}

fn main() {
    let (width, height) = termion::terminal_size().unwrap();

    let mut game_state = GameState {
        width: width / 2,
        height,
        mouse_pos: None,
        last_frame: None,
    };
    let mut graphics = Graphics::new(width, height, Instant::now());

    let mut writable =
        termion::cursor::HideCursor::from(MouseTerminal::from(stdout().into_raw_mode().unwrap()))
            .into_alternate_screen()
            .unwrap();

    let event_receiver = set_up_input_thread();

    loop {
        while let Ok(event) = event_receiver.try_recv() {
            game_state.process_event(event);
        }
        let frame = game_state.render();
        draw_frame(&mut writable, &frame, &game_state.last_frame);
        game_state.last_frame = Some(frame);
        thread::sleep(Duration::from_millis(21));
    }
}

fn board_color(square: WorldSquare) -> Option<RGB8> {
    let is_white = ((square.x / 3) % 2 == 0) == ((square.y / 3) % 2 == 0);
    Some(if is_white { grey(191) } else { grey(127) })
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn test_simple_output() {
        let state = GameState::new(10, 10);
        let frame = state.render();
        assert_eq!(frame.width(), 10);
        assert_eq!(frame.height(), 10);
    }
}
