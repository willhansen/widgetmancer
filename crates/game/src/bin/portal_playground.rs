use euclid::point2;
use game::{graphics::Graphics, set_up_input_thread};
use itertools::Itertools;
use rgb::RGB8;
use std::fmt::Display;
use std::io::{stdin, stdout, Write};
use std::path::PathBuf;
use std::thread;
use std::time::{Duration, Instant};
use terminal_rendering::glyph_constants::named_colors;
use terminal_rendering::*;
use termion::screen::{IntoAlternateScreen, ToAlternateScreen};
use termion::{
    event::{Event, Key},
    input::{MouseTerminal, TermRead},
    raw::IntoRawMode,
};
use utility::*;

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
    pub fn process_events(&mut self, events: impl IntoIterator<Item = Event>) {
        events.into_iter().for_each(|e| self.process_event(e))
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
            Event::Mouse(mouse_event) => match mouse_event {
                termion::event::MouseEvent::Press(mouse_button, x, y) => {
                    self.mouse_pos = Some(point2(x as i32 - 1, y as i32 - 1))
                }
                termion::event::MouseEvent::Release(x, y) => self.mouse_pos = None,
                termion::event::MouseEvent::Hold(x, y) => {
                    self.mouse_pos = Some(point2(x as i32 - 1, y as i32 - 1))
                }
            },
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
                            let pos = point2(x as i32, y as i32);
                            DoubleGlyph::solid_color(if self.mouse_pos == Some(pos) {
                                named_colors::RED
                            } else {
                                board_color(pos).unwrap()
                            })
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

#[derive(PartialEq)]
struct Frame {
    grid: Vec<Vec<DoubleGlyph>>,
}

impl Frame {
    pub fn save_to_file(&self, path: PathBuf) {
        todo!();
    }
    pub fn load_from_file(path: PathBuf) -> Option<Frame> {
        todo!();
    }
    pub fn load_from_string(string: String) -> Frame {
        todo!();
    }
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

impl Display for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
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

    fn press_left(x: u16, y: u16) -> termion::event::Event {
        termion::event::Event::Mouse(termion::event::MouseEvent::Press(
            termion::event::MouseButton::Left,
            x,
            y,
        ))
    }
    fn drag_mouse(x: u16, y: u16) -> termion::event::Event {
        termion::event::Event::Mouse(termion::event::MouseEvent::Hold(x, y))
    }
    fn release_mouse(x: u16, y: u16) -> termion::event::Event {
        termion::event::Event::Mouse(termion::event::MouseEvent::Release(x, y))
    }

    macro_rules! compare_frame_for_test {
        ($frame:ident) => {
            let test_name: String = todo!();
            let correct_frame_path: PathBuf = todo!();

            let maybe_correct_frame: Option<Frame> = Frame::load_from_file(correct_frame_path);

            let Some(correct_frame) = maybe_correct_frame else {
                let blessed = todo!();
                if blessed {
                    $frame.save_to_file(correct_frame_path);
                    return;
                }
                panic!("No correct frame found.  Bless tests to lock-in current frame as correct.\n\n{}", $frame);
            };

            if $frame == correct_frame {
                return;
            }

            panic!("Frames do not match.\n\nCorrect:\n{correct_frame}\n\nIncorrect:\n{}\n\nDiff:\n{}", $frame, Frame::load_from_string(correct_frame.diff(&Some($frame))));

        }
    }

    #[test]
    fn test_place_portal() {
        let mut game = GameState::new(10, 10);
        game.process_events([
            press_left(3, 3),
            drag_mouse(3, 2),
            drag_mouse(7, 2),
            release_mouse(7, 4),
        ]);
        let frame = game.render();
        compare_frame_for_test!(frame);
    }
}
