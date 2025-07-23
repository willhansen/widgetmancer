use euclid::point2;
use game::{graphics::Graphics, set_up_input_thread};
use itertools::Itertools;
use rgb::RGB8;
use std::collections::{HashMap, VecDeque};
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
type OrthoDir = i32;
type SquareEdge = (IPoint, OrthoDir);
type PortalSide = SquareEdge;

enum PortalRenderingOption {
    LineOnFloor,
    Absolute,
    LineOfSight,
}

const DIR_RIGHT: i32 = 0;
const DIR_UP: i32 = 1;
const DIR_LEFT: i32 = 2;
const DIR_DOWN: i32 = 3;

const STEP_RIGHT: IPoint = [1,0];
const STEP_UP: IPoint = [0,1];
const STEP_LEFT: IPoint = [-1,0];
const STEP_DOWN: IPoint = [0,-1];

fn step_in_direction(dir: OrthoDir) -> IPoint {
    match dir {
        0 => [1,0],
        1 => [0,1],
        2 => [-1,0],
        3 => [0,-1],
        _ => panic!("invalid direction: {dir}")
    }
}
fn closest_ortho_dir(square: IPoint) -> Option<OrthoDir> {
    if square[0].abs() == square[1].abs() {
        return None
    }

    Some(if square[0].abs() > square[1].abs() {
        if square[0] > 0 {
            0
        } else {2}

    } else {
        if square[1] > 0 {
            1
        } else {3}
    })

}

fn other_side_of_edge(edge: SquareEdge) -> SquareEdge {
    let step = step_in_direction(edge.1);
    let reverse_dir = (edge.1 + 2) %4;
    ([edge.0[0] + step[0], edge.0[1] + step[1]], reverse_dir)
}

// struct PortalUnderConstruction {
//     start_square: IPoint,
//     entrance_direction: Option<OrthoDir>,
//     extension_direction: Option<OrthoDir>,
//     extension_length: Option<u32>
// }

struct GameState {
    running: bool,
    width: usize,
    height: usize,

    portals: HashMap<PortalSide, PortalSide>,

    last_mouse_screen_row_col: Option<[u16; 2]>,
    pub portal_rendering: PortalRenderingOption,
}
impl GameState {
    pub fn new(width: usize, height: usize) -> Self {
        GameState {
            running: true,
            width,
            height,
            portals: Default::default(),
            last_mouse_screen_row_col: None,
            portal_rendering: PortalRenderingOption::LineOnFloor,
        }
    }
    pub fn process_events(&mut self, events: impl IntoIterator<Item = Event>) {
        events.into_iter().for_each(|e| self.process_event(e))
    }

    pub fn place_portal(&mut self, entrance: PortalSide, reverse_entrance: PortalSide ) {
        self.portals.insert(entrance, reverse_entrance);
        self.portals.insert(reverse_entrance, entrance);
        self.portals.insert(other_side_of_edge(entrance), other_side_of_edge(reverse_entrance));
        self.portals.insert(other_side_of_edge(reverse_entrance), other_side_of_edge(entrance));
    }
    pub fn process_event(&mut self, event: Event) {
        match event {
            Event::Key(key) => match key {
                Key::Char('q') => self.running = false,
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
                    self.last_mouse_screen_row_col = Some([row - 1, col - 1])
                }
                termion::event::MouseEvent::Release(col, row) => {
                    self.last_mouse_screen_row_col = None
                }
                termion::event::MouseEvent::Hold(col, row) => {
                    self.last_mouse_screen_row_col = Some([row - 1, col - 1])
                }
            },
            Event::Unsupported(items) => todo!(),
        }
    }
    pub fn render(&self) -> Frame {
        let mouse_is_on_left_half_of_square = self
            .last_mouse_screen_row_col
            .is_some_and(|[row, col]| col % 2 == 0);
        (0..self.height as i32)
            .map(|world_row| {
                let world_y = self.height as i32 - world_row - 1;
                (0..self.width as i32)
                    .map(|world_x| {
                        // let x = col;
                        // let y = self.height - row - 1;
                        let world_pos = point2(world_x as i32, world_y as i32);
                        let mouse_is_here = self.last_mouse_screen_row_col.is_some_and(
                            |[screen_row, screen_col]| {
                                let screen_y: i32 = self.height as i32 - i32::from(screen_row) - 1;
                                (i32::from(screen_col)) / 2 == world_x && screen_y == world_y
                            },
                        );
                        let board_color = board_color(world_pos).unwrap();
                        if mouse_is_here {
                            if mouse_is_on_left_half_of_square {
                                [
                                    Glyph::solid_color(named_colors::RED),
                                    Glyph::solid_color(board_color),
                                ]
                            } else {
                                [
                                    Glyph::solid_color(board_color),
                                    Glyph::solid_color(named_colors::RED),
                                ]
                            }
                        } else {
                            DoubleGlyph::solid_color(board_color)
                        }
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
    writable.flush();
}

fn main() {
    let (term_width, term_height) = termion::terminal_size().unwrap();
    let mut screen_frame = Frame::blank(term_width as usize, term_height as usize);
    let (width, height) = (30, 15);

    let mut game_state = GameState::new(width/2, height);

    let mut writable =
        termion::cursor::HideCursor::from(MouseTerminal::from(stdout().into_raw_mode().unwrap()))
            .into_alternate_screen()
            .unwrap();

    let event_receiver = set_up_input_thread();

    let mut prev_drawn = None;
    let mut event_log = VecDeque::new();
    while game_state.running {
        while let Ok(event) = event_receiver.try_recv() {
            event_log.push_front(event.clone());
            event_log.truncate(5);
            game_state.process_event(event);
        }
        let frame = game_state.render();
        screen_frame.blit(&frame, [0, 0]);
        screen_frame.draw_text(
            format!("{:?}", game_state.last_mouse_screen_row_col),
            [(height + 1).into(), 0],
        );
        for (i, event) in event_log.iter().enumerate() {
            screen_frame.draw_text(format!("{:-<30?}", event), [height as usize + 3 + i, 0]);
        }
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
    use pretty_assertions::assert_str_eq;
    use std::{assert_eq, assert_ne};
    use stdext::function_name;

    #[test]
    fn test_simple_output() {
        let state = GameState::new(10, 10);
        let frame = state.render();
        assert_eq!(frame.width(), 20);
        assert_eq!(frame.height(), 10);
    }

    fn press_left(col: u16, row: u16) -> termion::event::Event {
        termion::event::Event::Mouse(termion::event::MouseEvent::Press(
            termion::event::MouseButton::Left,
            col + 1,
            row + 1,
        ))
    }
    fn drag_mouse(col: u16, row: u16) -> termion::event::Event {
        termion::event::Event::Mouse(termion::event::MouseEvent::Hold(col + 1, row + 1))
    }
    fn release_mouse(col: u16, row: u16) -> termion::event::Event {
        termion::event::Event::Mouse(termion::event::MouseEvent::Release(col + 1, row + 1))
    }

    macro_rules! compare_frame_to_file {
        ($frame:ident, $prefix:expr) => {
            let string = $frame.string_for_regular_display();
            eprintln!("{:?}", $frame);
            let test_name: String = function_name!().replace(":", "_");
            compare_string_for_test(string, format!("{}_{}", $prefix, test_name))
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

        // eprintln!("{}", &candidate_string);

        let blessed = option_env!("BLESS_TESTS").is_some();
        if blessed {
            std::fs::write(correct_frame_path, candidate_string).unwrap();
            return;
        }

        let correct_string = maybe_correct_string
            .expect("No existing test output found.  Set BLESS_TESTS to canonize current output.");
        assert_eq!(candidate_string, correct_string,
            "Frames do not match.  Set the BLESS_TESTS env var to lock-in current string as correct.\n\nCorrect:\n{}\n\nGiven:\n{}\n❌\nCorrect:\n{}\n\nGiven:\n{}",
            correct_string,
            candidate_string,
            frame::display_string_to_readable_string(correct_string.clone()),
            frame::display_string_to_readable_string(candidate_string.clone())
        );

        eprintln!("{}\n✅", candidate_string);
    }

    #[test]
    fn test_click_a() {
        let mut game = GameState::new(12, 12);
        game.process_event(press_left(0, 0));
        let frame = game.render();
        // let no_color = frame.uncolored_regular_string();
        dbg!(&frame);
        compare_frame_to_file!(frame);
    }
    #[test]
    fn test_click_a_small() {
        let mut game = GameState::new(2, 2);
        game.process_event(press_left(0, 0));
        let frame = game.render();
        compare_frame_to_file!(frame);
    }
    #[test]
    fn test_click_b() {
        let mut game = GameState::new(12, 12);
        game.process_event(press_left(3, 9));
        let frame = game.render();
        dbg!(&frame);
        eprintln!("{}", frame.string_for_regular_display());
        assert_ne!(frame.get_xy([2, 2]).bg_color, named_colors::RED);
        assert_eq!(frame.get_xy([3, 2]).bg_color, named_colors::RED);
        compare_frame_to_file!(frame);
    }
    #[test]
    fn test_drag_mouse() {
        let mut game = GameState::new(12, 12);
        game.process_events([press_left(3, 3)]);
        let frame_1 = game.render();
        game.process_events([drag_mouse(4, 3)]);
        let frame_2 = game.render();
        game.process_events([drag_mouse(5, 3)]);
        let frame_3 = game.render();
        dbg!(&frame_1, &frame_2, &frame_3);
        compare_frame_to_file!(frame_1, "1");
        compare_frame_to_file!(frame_2, "2");
        compare_frame_to_file!(frame_3, "3");
    }
    #[test]
    fn test_render_portal_edges() {
        let mut game = GameState::new(12, 12);
        game.place_portal(([6,9],DIR_UP), ([10,7], DIR_RIGHT));
        let frame = game.render();
        compare_frame_to_file!(frame);
        panic!();
    }
}
