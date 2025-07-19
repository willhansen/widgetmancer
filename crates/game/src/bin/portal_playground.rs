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

    last_mouse_screen_row_col: Option<[u16;2]>,
}
impl GameState {
    pub fn new(width: usize, height: usize) -> Self {
        GameState {
            width,
            height,
            portals: Default::default(),
            last_mouse_screen_row_col: None,
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
                termion::event::MouseEvent::Press(mouse_button, col, row) => {
                    self.last_mouse_screen_row_col = Some([row, col])
                }
                termion::event::MouseEvent::Release(col, row) => self.last_mouse_screen_row_col = None,
                termion::event::MouseEvent::Hold(col, row) => {
                    self.last_mouse_screen_row_col = Some([row, col])
                }
            },
            Event::Unsupported(items) => todo!(),
        }
    }
    pub fn render(&self) -> Frame {
        (0..self.height as i32)
            .map(|world_row| {
                let world_y = self.height as i32 - world_row - 1;
                (0..self.width as i32)
                    .map(|world_x| {
                        // let x = col;
                        // let y = self.height - row - 1;
                        let world_pos = point2(world_x as i32, world_y as i32);
                        let mouse_is_here = self.last_mouse_screen_row_col.is_some_and(|[screen_row, screen_col]| {
                            i32::from(screen_col / 2)-1 == world_x && i32::from(screen_row)-1 == world_y
                        });

                        DoubleGlyph::solid_color(if mouse_is_here {
                            named_colors::RED
                        } else {
                            board_color(world_pos).unwrap()
                        })
                    })
                    .collect_vec()
            })
            .collect_vec()
            .into()
    }
}

fn grey(x: u8) -> RGB8 {
    RGB8::new(x, x, x)
}

fn draw_frame(writable: &mut impl Write, new_frame: &Frame, maybe_old_frame: &Option<Frame>) {
    writable.write(&new_frame.bytes_for_raw_display_over(maybe_old_frame));
}

fn main() {
    let (term_width, term_height) = termion::terminal_size().unwrap();
    let mut screen_frame = Frame::blank(term_width as usize, term_height as usize);
    let (width, height) = (30,15);


    let mut game_state = GameState {
        width: width as usize / 2,
        height: height as usize,
        portals: Default::default(),
        last_mouse_screen_row_col: None,
    };
    let mut graphics = Graphics::new(width, height, Instant::now());

    let mut writable =
        termion::cursor::HideCursor::from(MouseTerminal::from(stdout().into_raw_mode().unwrap()))
            .into_alternate_screen()
            .unwrap();

    let event_receiver = set_up_input_thread();

    let mut prev_drawn = None;
    loop {
        while let Ok(event) = event_receiver.try_recv() {
            game_state.process_event(event);
        }
        let frame = game_state.render();
        screen_frame.blit(&frame, [0,0]);
        screen_frame.draw_text(format!("{:?}", game_state.last_mouse_screen_row_col), [16, 0]);
        draw_frame(&mut writable, &screen_frame, &prev_drawn);
        prev_drawn = Some(screen_frame.clone());
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
        assert_eq!(frame.width(), 20);
        assert_eq!(frame.height(), 10);
    }

    fn press_left(screen_x: u16, screen_y: u16) -> termion::event::Event {
        termion::event::Event::Mouse(termion::event::MouseEvent::Press(
            termion::event::MouseButton::Left,
            screen_x,
            screen_y,
        ))
    }
    fn drag_mouse(x: u16, y: u16) -> termion::event::Event {
        termion::event::Event::Mouse(termion::event::MouseEvent::Hold(x, y))
    }
    fn release_mouse(x: u16, y: u16) -> termion::event::Event {
        termion::event::Event::Mouse(termion::event::MouseEvent::Release(x, y))
    }

    macro_rules! compare_string_to_file {
        ($string:ident, $prefix:expr) => {
            let test_name: String = function_name!().replace(":", "_");
            compare_string_for_test($string, format!("{}_{}", $prefix, test_name))
        };
        ($frame:ident) => {
            compare_string_to_file!($string, "")
        };
    }
    macro_rules! compare_frame_to_file {
        ($frame:ident, $prefix:expr) => {
            let string = $frame.string_for_regular_display();
            compare_string_to_file!(string, $prefix)
        };
        ($frame:ident) => {
            compare_frame_to_file!($frame, "")
        };
    }

    fn compare_string_for_test(candidate_string: String, file_prefix: String) {
        let file_directory: PathBuf = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/");
        assert!(file_directory.is_dir());
        let correct_frame_path: PathBuf = file_directory.join(file_prefix + "_good_frame.txt");

        let maybe_correct_string: Option<String> =
            std::fs::read_to_string(correct_frame_path.clone()).ok();

        let blessed = option_env!("BLESS_TESTS").is_some();
        if blessed || maybe_correct_string.is_none() {
            std::fs::write(correct_frame_path, candidate_string).unwrap();
            return;
        }

        let correct_string = maybe_correct_string.unwrap();
        assert_eq!(candidate_string, correct_string,
            "Frames do not match.  Set the BLESS_TESTS env var to lock-in current string as correct.\n\nCorrect:\n{}\n\nGiven:\n{}\n❌\nCorrect:\n{}\n\nGiven:\n{}",
            correct_string,
            candidate_string,
            correct_string.escape_debug(),
            candidate_string.escape_debug()
        );

        println!("{}\n✅", candidate_string);
    }

    #[test]
    fn test_click_a() {
        let mut game = GameState::new(12, 12);
        game.process_event(press_left(0, 0));
        let frame = game.render();
        compare_frame_to_file!(frame);
    }
    #[test]
    fn test_click_b() {
        let mut game = GameState::new(12, 12);
        game.process_event(press_left(3, 3));
        let frame = game.render();
        dbg!(&frame);
        println!("{}", frame.string_for_regular_display());
        let red_map: String = (0..frame.height())
            .map(|row| {
                let y = frame.row_to_y(row);
                (0..frame.width())
                    .map(|col| {
                        let x = col;
                        if frame.get_xy([x, y]).bg_color == named_colors::RED {
                            "1"
                        } else {
                            "0"
                        }
                    })
                    .join("")
            })
            .join("\n");
        compare_string_to_file!(red_map, "red_map");
        assert_eq!(frame.get_xy([2, 2]).bg_color, named_colors::RED);
        assert_eq!(frame.get_xy([3, 2]).bg_color, named_colors::RED);
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
