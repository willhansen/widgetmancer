use euclid::point2;
use game::{graphics::Graphics, set_up_input_thread};
use itertools::Itertools;
use rgb::RGB8;
use std::collections::HashMap;
use std::fmt::Display;
use std::fs::File;
use std::io::Read;
use std::io::{stdin, stdout, Write};
use std::option_env;
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

type IPoint = [i32; 2];
type Quadrant = i32;
type Portal = (IPoint, Quadrant);

struct GameState {
    width: usize,
    height: usize,

    portals: HashMap<Portal, Portal>,

    last_mouse_screen_pos: Option<WorldSquare>,
    last_frame: Option<Frame>,
}
impl GameState {
    pub fn new(width: usize, height: usize) -> Self {
        GameState {
            width,
            height,
            portals: Default::default(),
            last_mouse_screen_pos: None,
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
                    self.last_mouse_screen_pos = Some(point2(x as i32 - 1, y as i32 - 1))
                }
                termion::event::MouseEvent::Release(x, y) => self.last_mouse_screen_pos = None,
                termion::event::MouseEvent::Hold(x, y) => {
                    self.last_mouse_screen_pos = Some(point2(x as i32 - 1, y as i32 - 1))
                }
            },
            Event::Unsupported(items) => todo!(),
        }
    }
    pub fn render(&self) -> Frame {
        Frame {
            grid: (0..self.height as i32)
                .map(|world_row| {
                    let world_y = self.height as i32 - world_row - 1;
                    (0..self.width as i32)
                        .map(|world_x| {
                            // let x = col;
                            // let y = self.height - row - 1;
                            let world_pos = point2(world_x as i32, world_y as i32);
                            let mouse_is_here =
                                self.last_mouse_screen_pos.is_some_and(|mouse_pos| {
                                    let [screen_x, screen_y] = mouse_pos.to_array();
                                    screen_x / 2 == world_x && screen_y == world_y
                                });

                            DoubleGlyph::solid_color(if mouse_is_here {
                                named_colors::RED
                            } else {
                                board_color(world_pos).unwrap()
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

fn draw_frame(writable: &mut impl Write, new_frame: &Frame, maybe_old_frame: &Option<Frame>) {
    writable.write(&new_frame.bytes_for_raw_display_over(maybe_old_frame));
}

fn main() {
    let (width, height) = termion::terminal_size().unwrap();

    let mut game_state = GameState {
        width: width as usize / 2,
        height: height as usize,
        portals: Default::default(),
        last_mouse_screen_pos: None,
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
    use stdext::function_name;

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

    macro_rules! compare_frame_to_file {
        ($frame:ident, $prefix:expr) => {
            let test_name: String = function_name!().replace(":", "_");
            compare_frame_for_test($frame, format!("{}_{}", $prefix, test_name))
        };
        ($frame:ident) => {
            compare_frame_to_file!($frame, "")
        };
    }

    fn compare_frame_for_test(frame: Frame, file_prefix: String) {
        let file_directory: PathBuf = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/");
        assert!(file_directory.is_dir());
        let correct_frame_path: PathBuf = file_directory.join(file_prefix + "_good_frame.txt");

        let maybe_correct_frame_string: Option<String> =
            std::fs::read_to_string(correct_frame_path.clone()).ok();

        if let Some(correct_frame_string) = maybe_correct_frame_string {
            let frame_string = frame.string_for_regular_display();
            assert_eq!(frame_string, correct_frame_string,
                "Frames do not match.\n\nCorrect:\n{}\n\nIncorrect:\n{}\n\nCorrect:\n{}\n\nIncorrect:\n{}",
                correct_frame_string,
                frame_string,
                correct_frame_string.escape_debug(),
                frame_string.escape_debug()
            );
        } else {
            let blessed = option_env!("BLESS_TESTS").is_some();
            if blessed {
                frame.save_to_file(correct_frame_path);
            } else {
                panic!("No correct frame found.  Set the BLESS_TESTS env var to lock-in current frame as correct.\n\n{}", frame);
            }
        };
    }

    #[test]
    fn test_click_a() {
        let mut game = GameState::new(12, 12);
        game.process_event(press_left(0, 0));
        let frame = game.render();
        compare_frame_to_file!(frame);
    }
    #[ignore]
    #[test]
    fn test_click_b() {
        let mut game = GameState::new(12, 12);
        game.process_event(press_left(2, 3));
        let frame = game.render();
        compare_frame_to_file!(frame);
    }
    #[ignore]
    #[test]
    fn test_place_portal() {
        let mut game = GameState::new(12, 12);
        game.process_events([
            press_left(3, 3),
            drag_mouse(3, 2),
            drag_mouse(7, 2),
            release_mouse(7, 4),
        ]);
        let frame = game.render();
        compare_frame_to_file!(frame);
        panic!();
    }
}
