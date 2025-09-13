#![allow(dead_code)]
#![allow(deprecated)]

use color_hex::color_from_hex;
use euclid::point2;
use game::fov_stuff::{
    FieldOfViewResult, MappedSquare, PositionedSquareVisibilityInFov, SquareVisibility,
};
use game::set_up_input_thread_given_sender;
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
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread::{self, sleep_ms};
use std::time::{Duration, Instant};
use terminal_rendering::drawable_glyph::*;
use terminal_rendering::glyph_constants::named_colors::*;
use terminal_rendering::*;
use termion::screen::{IntoAlternateScreen, ToAlternateScreen};
use termion::{
    event::{Event, Key},
    input::{MouseTerminal, TermRead},
    raw::IntoRawMode,
};
use utility::*;

type PortalSide = SquareEdge;

#[derive(Clone, Debug, Eq, PartialEq)]
enum PortalRenderingOption {
    LineOnFloor,
    Absolute,
    LineOfSight,
}

// TODO
// enum OrthoDir {
//     Right,
//     Up,
//     Left,
//     Down,
// }
//
fn set_up_panic_hook() {
    std::panic::set_hook(Box::new(move |panic_info| {
        stdout().flush().expect("flush stdout");

        write!(stdout(), "{}", termion::screen::ToMainScreen).expect("switch to main screen");
        let string_out = format!("{:#?}\n\n", panic_info).replace("\n", "\n\r");
        write!(stdout(), "{string_out:?}").expect("display panic info");

        if let Some(s) = panic_info.payload().downcast_ref::<&str>() {
            println!("panic occurred: {s:?}");
        } else if let Some(s) = panic_info.payload().downcast_ref::<String>() {
            println!("panic occurred: {s:?}");
        } else {
            println!("panic occurred");
        }
    }));
}

use geometry2::FPoint;
use geometry2::IPoint;
use geometry2::*;

const UI_BACKGROUND_RGB: [u8; 3] = color_from_hex!("#1b305b");
const ETERNAL_VOID_CHAR: char = '.';
const OUT_OF_FOV_RANGE_CHAR: char = '╲';
const SHADOW_CHAR: char = '╳';

struct Game {
    pub world_state: WorldState,
    pub ui_handler: UiHandler,
}
impl Game {
    pub fn new(width: usize, height: usize) -> Self {
        Game {
            world_state: WorldState::new(width, height),
            ui_handler: UiHandler::new(),
        }
    }
    pub fn new_headless_square(side_length_in_squares: usize) -> Self {
        Self::new_headless(
            side_length_in_squares as u16,
            side_length_in_squares as u16 * 2,
            side_length_in_squares,
            side_length_in_squares,
        )
    }
    pub fn new_headless(
        screen_height: u16,
        screen_width: u16,
        world_width: usize,
        world_height: usize,
    ) -> Self {
        Game {
            world_state: WorldState::new(world_width, world_height),
            ui_handler: UiHandler::new_headless(screen_height, screen_width),
        }
    }
    pub fn process_event(&mut self, event: Event) {
        match event {
            Event::Key(key) => match key {
                Key::Char('q') => self.world_state.running = false,
                Key::Char('t') => {
                    self.world_state.portal_rendering = match self.world_state.portal_rendering {
                        PortalRenderingOption::LineOnFloor => PortalRenderingOption::Absolute,
                        PortalRenderingOption::Absolute => PortalRenderingOption::LineOfSight,
                        PortalRenderingOption::LineOfSight => PortalRenderingOption::LineOnFloor,
                    }
                }
                Key::Char('w') => self.try_move_player([0, 1]),
                Key::Char('a') => self.try_move_player([-1, 0]),
                Key::Char('s') => self.try_move_player([0, -1]),
                Key::Char('d') => self.try_move_player([1, 0]),
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
                    self.ui_handler.last_mouse_screen_row_col = Some([row, col]);
                    self.ui_handler.smoothed_mouse_screen_row_col = Some([row as f32, col as f32]);
                }
                termion::event::MouseEvent::Release(col, row) => {
                    self.ui_handler.last_mouse_screen_row_col = None;
                    self.ui_handler.smoothed_mouse_screen_row_col = None;
                }
                termion::event::MouseEvent::Hold(col, row) => {
                    self.ui_handler.last_mouse_screen_row_col = Some([row, col])
                }
            },
            Event::Unsupported(items) => todo!(),
        };
    }
    pub fn advance_time_by(&mut self, dt_s: f32) {
        assert!(dt_s > 0.0, "dt must be positive: {dt_s}");
        let s_from_start = self.ui_handler.s_from_start + dt_s;
        self.advance_time_to(s_from_start);
    }
    pub fn advance_time_to(&mut self, s_from_start: f32) {
        self.process_events();
        self.ui_handler.advance_time_to(s_from_start);
        self.world_state.advance_time_to(s_from_start);
        self.process_events();
    }

    pub fn advance_time_n_steps(&mut self, step_dt_s: f32, n: u32) {
        (0..n).for_each(|_n| {
            self.process_events();
            self.advance_time_by(step_dt_s);
        });
    }

    pub fn process_events(&mut self) -> usize {
        self.ui_handler.receive_events();
        let to_process = self.ui_handler.take_unprocessed_past_events();
        let n = to_process.len();
        to_process.into_iter().for_each(|e| {
            self.process_event(e.1.clone());
            self.ui_handler.event_log.push_back(e);
        });
        n
    }

    pub fn try_move_player_at_time(&mut self, step: IPoint, s_from_start: f32) {
        assert!(step.squared_length() == 1);
        let dir = closest_ortho_dir(step).unwrap();
        let (new_pos, end_dir) = self
            .world_state
            .portal_step(self.world_state.player_square, dir);
        if self.world_state.on_board(new_pos) {
            self.world_state
                .player_step_history
                .push_back((s_from_start, step));
            while self.world_state.player_step_history.len() > 5 {
                self.world_state.player_step_history.pop_front();
            }
            self.world_state.player_square = new_pos;
        }
    }
    pub fn try_move_player(&mut self, step: IPoint) {
        self.try_move_player_at_time(step, self.ui_handler.now_as_s_from_start())
    }
    pub fn now_as_s_from_start(&self) -> f32 {
        self.ui_handler.now_as_s_from_start()
    }
    pub fn render(&mut self) -> Frame {
        self.ui_handler.render(&self.world_state)
    }
}

#[derive(Clone, Debug, Copy, Eq, PartialEq)]
struct Camera {
    lower_left_square: [i32; 2],
    upper_right_square: [i32; 2],
}

// TODO: frame coordinates should be distinct from screen coordinates, and the camera should only
// deal with frame coordinates.
// Most coordinates are in squares unless noted
impl Camera {
    pub fn new_from_screen_size(size_on_screen_rows_cols: USizePoint) -> Self {
        let size_in_squares_width_height =
            [size_on_screen_rows_cols[1] / 2, size_on_screen_rows_cols[0]];
        Self::new_from_world_size(size_in_squares_width_height)
    }
    pub fn new_from_world_size(size_in_squares_width_height: USizePoint) -> Self {
        let names = ["width", "height"];
        size_in_squares_width_height
            .iter()
            .zip(names.iter())
            .for_each(|(size, name)| {
                assert!(
                    size % 2 == 1,
                    "{name} must be odd to have integer center.  {name}: {size}"
                );
            });
        Self::new_pos_and_squares_width_height([0, 0], size_in_squares_width_height)
    }
    pub fn new_square(s: usize) -> Self {
        Self::new_pos_and_squares_width_height([0, 0], [s; 2])
    }
    pub fn new_pos_and_squares_width_height(lower_left: IPoint, width_height: USizePoint) -> Self {
        Camera {
            lower_left_square: lower_left,
            upper_right_square: lower_left.add(width_height.to_int().sub([1, 1])),
        }
    }
    pub fn translate_to_set_bottom_left_square(&mut self, bottom_left_square: IPoint) {
        let new = self.with_bottom_left_square(bottom_left_square);
        self.lower_left_square = new.lower_left_square;
        self.upper_right_square = new.upper_right_square;
    }
    pub fn top_right_local_square(&self) -> IPoint {
        self.upper_right_square.sub(self.lower_left_square)
    }
    pub fn top_left_local_square(&self) -> IPoint {
        rect_corner_by_quadrant(self.top_right_local_square(), 1)
    }
    pub fn translate_local_square_to_absolute_square(
        &mut self,
        local_square: IPoint,
        absolute_square: IPoint,
    ) {
        let new_absolute_bottom_left = absolute_square.sub(local_square);
        self.translate_to_set_bottom_left_square(new_absolute_bottom_left);
    }
    pub fn with_bottom_left_square(&self, bottom_left_square: IPoint) -> Self {
        Self::from_bottom_left_and_size(bottom_left_square, self.size_in_world())
    }
    pub fn from_bottom_left_and_size(
        bottom_left_square: IPoint,
        width_height_in_squares: [u32; 2],
    ) -> Self {
        Self {
            lower_left_square: bottom_left_square,
            upper_right_square: bottom_left_square
                .add(width_height_in_squares.to_signed())
                .sub([1, 1]),
        }
    }
    pub fn bottom_left_square(&self) -> IPoint {
        self.lower_left_square
    }
    pub fn translate(&mut self, movement: IPoint) {
        self.translate_to_set_bottom_left_square(self.bottom_left_square().add(movement))
    }
    pub fn size_in_world(&self) -> [u32; 2] {
        [
            (self.upper_right_square[0] - self.lower_left_square[0]) as u32 + 1,
            (self.upper_right_square[1] - self.lower_left_square[1]) as u32 + 1,
        ]
    }
    pub fn width_in_world(&self) -> u32 {
        self.size_in_world()[0]
    }
    pub fn height_in_world(&self) -> u32 {
        self.size_in_world()[1]
    }
    pub fn size_on_screen_rows_cols(&self) -> [usize; 2] {
        [
            self.height_in_world() as usize,
            self.width_in_world() as usize * 2,
        ]
    }
    pub fn local_to_absolute_world_square(&self, local_v: IPoint) -> IPoint {
        rotate_quarter_turns(local_v, self.quarter_turns_ccw_from_world())
            .add(self.lower_left_square)
    }
    pub fn absolute_to_local_world_square(&self, absolute_world_square: IPoint) -> IPoint {
        // TODO: rotation
        absolute_world_square.sub(self.lower_left_square)
    }
    pub fn local_to_world_ortho_dir(&self, dir: OrthoDir) -> OrthoDir {
        (dir + self.quarter_turns_ccw_from_world()).rem_euclid(4)
    }
    pub fn quarter_turns_ccw_from_world(&self) -> OrthoDir {
        let [x1, y1] = self.lower_left_square;
        let [x2, y2] = self.upper_right_square;

        match (x1 < x2, y1 < y2) {
            (true, true) => 0,
            (true, false) => 3,
            (false, true) => 1,
            (false, false) => 2,
        }
    }
    // zero indexed
    pub fn frame_row_col_point_to_local_world_point(&self, screen_row_col_point: FPoint) -> FPoint {
        [
            (screen_row_col_point[1] - 1.0) / 2.0,
            self.height_in_world() as f32 - (screen_row_col_point[0] - 1.0),
        ]
    }
    // Not one-to-one.  each square has two characters that map to it
    // zero indexed
    pub fn frame_row_col_char_to_local_world_square(
        &self,
        camera_local_row_col_char: [u16; 2],
    ) -> IPoint {
        let [row, col] = camera_local_row_col_char.map(|x| x as i32);
        [col / 2, self.height_in_world() as i32 - row - 1]
    }
    // zero indexed
    pub fn frame_row_col_char_to_absolute_world_square(
        &self,
        frame_row_col_char: [u16; 2],
    ) -> IPoint {
        self.local_to_absolute_world_square(
            self.frame_row_col_char_to_local_world_square(frame_row_col_char),
        )
    }
    pub fn absolute_world_square_to_left_frame_row_col(
        &self,
        absolute_world_square: IPoint,
    ) -> IPoint {
        self.local_world_square_to_left_frame_row_col(
            self.absolute_to_local_world_square(absolute_world_square),
        )
    }
    pub fn local_world_square_to_left_frame_row_col(&self, local_world_square: IPoint) -> IPoint {
        [local_world_square[0] * 2, local_world_square[1]]
    }
    pub fn local_world_square_in_frame(&self, local_world_square: IPoint) -> bool {
        todo!();
    }
    pub fn char_row_col_in_frame(&self, char_row_col: IPoint) -> bool {
        todo!();
    }

    pub fn render_world(&self, world_state: &WorldState, fov_center: FPoint) -> Frame {
        let radius = self.width_in_world() / 2;
        let frame = self.render_world_with_radius(&world_state, fov_center, radius);
        assert_eq!(frame.size_rows_cols()[0], radius as usize * 2 + 1);
        assert_eq!(frame.size_rows_cols()[1], (radius as usize * 2 + 1) * 2);
        assert_eq!(self.size_on_screen_rows_cols(), frame.size_rows_cols());
        frame
    }
    fn blank_frame(&self) -> Frame {
        let size = self.size_on_screen_rows_cols();
        let camera_background_default_glyph = DrawableGlyph::new('🮖', None, None);
        Frame::new_from_repeated_glyph(size[1], size[0], camera_background_default_glyph)
    }
    pub fn render_world_with_radius(
        &self,
        world_state: &WorldState,
        fov_center: FPoint,
        fov_range: u32,
    ) -> Frame {
        let (fov_frame, debug_layers) =
            world_state.render_with_options(false, fov_center, fov_range);
        assert_eq!(fov_frame.size_rows_cols()[0], fov_range as usize * 2 + 1);
        assert_eq!(
            fov_frame.size_rows_cols()[1],
            (fov_range as usize * 2 + 1) * 2
        );
        // assert_eq!(self.size_on_screen_rows_cols(), result.0.size_rows_cols());

        // Have rendered world fov frame, but now where to put it in the camera frame?
        // Camera corners in world are known.  fov center and radius in world are known.
        // to blit, we need the top-left character pos of the fov
        // Can get that from the top-left square of the fov in the camera's local world frame
        let mut camera_frame = self.blank_frame();

        let fov_center_absolute_square = fov_center.rounded();
        let fov_top_left_absolute_square =
            fov_center_absolute_square.add([-(fov_range as i32), fov_range as i32]);
        let fov_top_left_char_row_col =
            self.absolute_world_square_to_left_frame_row_col(fov_top_left_absolute_square);

        camera_frame.blit(&fov_frame, fov_top_left_char_row_col.map(|x| x as usize));

        camera_frame
    }
}

#[cfg(test)]
mod camera_tests {
    use super::*;
    #[test]
    fn test_frame_to_world_integers() {
        let s: i32 = 32;
        let camera = Camera::new_square(s as usize);

        let f = |x| camera.frame_row_col_char_to_absolute_world_square(x);
        assert_eq!(f([0, 0]), [0, s - 1]);

        assert_eq!(f([0, 0]), [0, s - 1]);
        assert_eq!(f([0, 1]), [0, s - 1]);

        assert_eq!(f([1, 0]), [0, s - 2]);
        assert_eq!(f([1, 1]), [0, s - 2]);

        assert_eq!(f([0, 2]), [1, s - 1]);
        assert_eq!(f([0, 3]), [1, s - 1]);

        assert_eq!(
            camera.local_to_absolute_world_square([0, s - 1]),
            [0, s - 1]
        );
        assert_eq!(camera.local_to_absolute_world_square([0, 0]), [0, 0]);
    }
    #[test]
    fn test_frame_to_world_floating_point() {
        let camera = Camera::new_square(10);

        assert_eq!(
            camera.frame_row_col_point_to_local_world_point([1.0, 1.0]),
            [0.0, camera.height_in_world() as f32]
        );
    }
    #[test]
    fn test_size() {
        let camera = Camera::new_square(25);
        assert_eq!(camera.size_in_world(), [25, 25]);
        assert_eq!(camera.size_on_screen_rows_cols(), [25, 50]);
    }
    #[test]
    fn test_local_to_absolute_world_squares_simple() {
        let camera = Camera::new_square(10);
        let p = [3, 5];
        assert_eq!(
            p,
            camera.local_to_absolute_world_square(camera.absolute_to_local_world_square(p))
        );
        assert_eq!(
            p,
            camera.absolute_to_local_world_square(camera.local_to_absolute_world_square(p))
        );
    }
    #[test]
    fn test_get_set_bottom_left_invariant() {
        let mut camera = Camera::new_square(10);
        assert_eq!(
            camera,
            camera.with_bottom_left_square(camera.bottom_left_square())
        );
    }
    #[test]
    fn test_local_to_absolute_world_squares_after_translation() {
        let mut camera = Camera::new_square(10);
        let p = [3, 5];
        dbg!(&camera);
        camera.translate([-2, 0]);
        dbg!(camera);
        assert_eq!(camera.local_to_absolute_world_square(p), [1, 5]);
        assert_eq!(
            p,
            camera.local_to_absolute_world_square(camera.absolute_to_local_world_square(p))
        );
        assert_eq!(
            p,
            camera.absolute_to_local_world_square(camera.local_to_absolute_world_square(p))
        );
    }
}

// struct PortalUnderConstruction {
//     start_square: IPoint,
//     entrance_direction: Option<OrthoDir>,
//     extension_direction: Option<OrthoDir>,
//     extension_length: Option<u32>
// }

struct UiHandler {
    // This instant is the link between the real world and "seconds from start".  The game world
    // has no use for real-time reference points that are instants
    pub start_time: Instant,
    pub s_from_start: f32,
    pub screen_size_rows_cols: [usize; 2],
    // 1-indexed
    pub last_mouse_screen_row_col: Option<[u16; 2]>,
    pub smoothed_mouse_screen_row_col: Option<FPoint>,
    pub output_writable: Option<Box<dyn Write>>,
    pub copy_of_event_sender: Sender<(Instant, Event)>,

    pub event_receiver: Receiver<(Instant, Event)>,
    // This stores events between receiving and processing
    pub event_queue: VecDeque<(f32, Event)>,
    // This stores events after processing
    // newest events are added via `push_back`
    pub event_log: VecDeque<(f32, Event)>,
    pub prev_drawn: Option<Frame>,
    pub enable_mouse_smoothing: bool,
    pub camera: Camera,
    pub default_fov_range: u32,
}
impl UiHandler {
    fn smoothed_mouse_position_screen_row_col(&mut self) -> Option<FPoint> {
        self.smoothed_mouse_screen_row_col
    }

    fn advance_smoothed_mouse(&mut self, dt_s: f32) {
        let Some(mouse_screen_square) = self.last_mouse_screen_row_col else {
            self.smoothed_mouse_screen_row_col = None;
            return;
        };
        let target_pos = mouse_screen_square.map(|x| x as f32);
        let Some(prev_pos) = self.smoothed_mouse_screen_row_col else {
            self.smoothed_mouse_screen_row_col = Some(target_pos);
            return;
        };

        self.smoothed_mouse_screen_row_col = Some(exponential_approach_with_min_speed(
            prev_pos, target_pos, dt_s, 0.05, 50.0,
        ));
    }

    pub fn advance_time_to(&mut self, new_s_from_start: f32) {
        let dt_s = new_s_from_start - self.s_from_start;
        assert!(dt_s > 0.0, "dt must be positive. dt: {dt_s}");
        self.advance_smoothed_mouse(dt_s);
        self.s_from_start = new_s_from_start;
    }
    pub fn advance_time_by(&mut self, dt_s: f32) {
        self.advance_time_to(self.s_from_start + dt_s)
    }

    pub fn screen_height(&self) -> usize {
        self.screen_size_rows_cols[0]
    }
    pub fn screen_width(&self) -> usize {
        self.screen_size_rows_cols[1]
    }
    pub fn new() -> UiHandler {
        let (term_width, term_height) = termion::terminal_size().unwrap();
        let output_writable = Box::new(
            termion::cursor::HideCursor::from(MouseTerminal::from(
                stdout().into_raw_mode().unwrap(),
            ))
            .into_alternate_screen()
            .unwrap(),
        );
        let ui_handler = Self::new_maybe_headless(term_height, term_width, Some(output_writable));
        set_up_input_thread_given_sender(ui_handler.copy_of_event_sender.clone());
        ui_handler
    }
    // xy order from bottom left of screen
    fn mouse_screen_xy_square(&self) -> Option<IPoint> {
        self.mouse_screen_xy_char().map(|p| [p[0] / 2, p[1]])
    }
    fn mouse_screen_xy_char(&self) -> Option<IPoint> {
        self.last_mouse_screen_row_col
            .map(|x| self.screen_row_col_char_to_screen_xy_char(x))
    }
    fn screen_row_col_char_to_screen_xy_char(&self, row_col_char: ScreenRowColCharPos) -> IPoint {
        let [screen_row, screen_col] = row_col_char;
        let screen_y: i32 = self.screen_height() as i32 - i32::from(screen_row) - 1;
        [i32::from(screen_col), screen_y]
    }
    fn screen_row_col_point_to_screen_xy_point(&self, row_col_point: FPoint) -> FPoint {
        [
            row_col_point[1] - 1.0, // No longer one-indexed
            self.screen_height() as f32 - (row_col_point[0] - 1.0),
        ]
    }
    fn screen_xy_point_to_screen_row_col_point(&self, xy_point: FPoint) -> FPoint {
        [
            self.screen_height() as f32 - (xy_point[1]) + 1.0,
            xy_point[0] + 1.0, // back to 1-indexed
        ]
    }
    fn mouse_world_square(&self) -> Option<IPoint> {
        self.last_mouse_screen_row_col
            .map(|row_col| self.screen_row_col_char_to_world_square(row_col))
    }
    fn mouse_world_point(&mut self) -> Option<FPoint> {
        let screen_mouse_point = self.smoothed_mouse_position_screen_row_col();
        screen_mouse_point.map(|p| self.screen_row_col_point_to_world_point(p))
    }
    pub fn new_headless(screen_height: u16, screen_width: u16) -> UiHandler {
        Self::new_maybe_headless(screen_height, screen_width, None)
    }
    pub fn new_maybe_headless(
        screen_height: u16,
        screen_width: u16,
        output_writable: Option<Box<dyn Write>>,
    ) -> UiHandler {
        let (event_sender, event_receiver) = channel();
        let screen_size_rows_cols = [screen_height as usize, screen_width as usize];
        // want to be square (for now)
        let camera_side_length = (screen_height as usize).min((screen_width as usize) / 2);
        // Needs to be odd size (because radius-based)
        // let camera_side_length = camera_side_length - (camera_side_length + 1) % 2;
        UiHandler {
            start_time: Instant::now(),
            s_from_start: 0.0,
            screen_size_rows_cols,
            last_mouse_screen_row_col: None,
            smoothed_mouse_screen_row_col: None,
            output_writable,
            event_receiver,
            copy_of_event_sender: event_sender,
            event_queue: Default::default(),
            event_log: Default::default(),
            prev_drawn: None,
            enable_mouse_smoothing: false,
            camera: Camera::new_square(camera_side_length),
            default_fov_range: camera_side_length as u32 / 2,
        }
    }
    pub fn draw_mouse(&mut self, mut screen_buffer: &mut Frame) {
        if self.enable_mouse_smoothing {
            self.draw_smoothed_mouse(&mut screen_buffer);
        } else {
            self.draw_mouse_square(&mut screen_buffer);
        }
    }
    pub fn draw_smoothed_mouse(&mut self, screen_buffer: &mut Frame) {
        if let Some(smoothed_mouse_pos_row_col) = self.smoothed_mouse_position_screen_row_col() {
            let smoothed_mouse_pos_xy =
                self.screen_row_col_point_to_screen_xy_point(smoothed_mouse_pos_row_col);

            let the_char: char = character_grid_point_xy_to_braille_char(smoothed_mouse_pos_xy);
            let [row_1i, col_1i] = smoothed_mouse_pos_row_col.rounded();
            assert!(row_1i > 0, "{row_1i}");
            assert!(row_1i <= self.screen_height() as i32, "{row_1i}");
            assert!(col_1i > 0, "{col_1i}");
            assert!(col_1i <= self.screen_width() as i32, "{col_1i}");
            let [row, col] = [row_1i as usize - 1, col_1i as usize - 1];
            screen_buffer.grid[row][col].character = the_char;
            screen_buffer.grid[row][col].fg_color = BLACK.into();
        }
    }
    pub fn draw_mouse_square(&mut self, screen_buffer: &mut Frame) {
        if let Some([row, col]) = self.last_mouse_screen_row_col {
            assert!(row > 0 && col > 0, "row: {row}, col: {col}");
            screen_buffer.grid[row as usize - 1][col as usize - 1] =
                DrawableGlyph::solid_color(RED);
        }
    }
    pub fn draw_screen(&mut self, screen_frame: Frame) {
        assert_eq!(screen_frame.size_rows_cols(), self.screen_size_rows_cols);

        draw_frame(
            &mut self.output_writable.as_mut().unwrap(),
            &screen_frame,
            &self.prev_drawn,
        );
        self.prev_drawn = Some(screen_frame);
    }

    pub fn receive_events(&mut self) {
        self.event_receiver.try_iter().for_each(|(instant, event)| {
            let t = instant.duration_since(self.start_time).as_secs_f32();
            self.event_queue.push_back((t, event.clone()))
        });
    }

    pub fn take_unprocessed_past_events(&mut self) -> Vec<(f32, Event)> {
        let mut past_events: Vec<(f32, Event)> = Default::default();
        loop {
            if let Some(next_event) = self.event_queue.pop_front() {
                if next_event.0 <= self.s_from_start {
                    past_events.push(next_event);
                } else {
                    self.event_queue.push_front(next_event);
                    break;
                }
            } else {
                break;
            }
        }
        past_events
    }

    pub fn give_future_event_absolute(&mut self, event: Event, s_from_start: f32) {
        assert!(s_from_start >= self.s_from_start);
        let t = self.s_from_start_to_instant(s_from_start);
        self.copy_of_event_sender.send((t, event)).unwrap()
    }
    pub fn give_future_event_relative(&mut self, event: Event, dt_s: f32) {
        assert!(dt_s >= 0.0);
        let s_from_start = self.s_from_start + dt_s;
        let t = self.s_from_start_to_instant(s_from_start);
        self.copy_of_event_sender.send((t, event)).unwrap()
    }
    pub fn give_event(&mut self, event: Event) {
        self.give_future_event_absolute(event, self.s_from_start);
    }
    pub fn now_as_s_from_start(&self) -> f32 {
        self.instant_to_s_from_start(Instant::now())
    }
    pub fn instant_to_s_from_start(&self, instant: Instant) -> f32 {
        instant.duration_since(self.start_time).as_secs_f32()
    }
    pub fn s_from_start_to_instant(&self, s_after_start: f32) -> Instant {
        self.start_time + Duration::from_secs_f32(s_after_start)
    }
    pub fn screen_row_col_point_to_world_point(&self, screen_row_col_point: FPoint) -> FPoint {
        self.camera
            .frame_row_col_point_to_local_world_point(screen_row_col_point)
    }

    pub fn screen_row_col_char_to_camera_frame_row_col_char(
        &self,
        screen_row_col_char: [u16; 2],
    ) -> [u16; 2] {
        screen_row_col_char
    }

    pub fn screen_row_col_char_to_world_square(&self, screen_row_col_char: [u16; 2]) -> IPoint {
        dbg!(&screen_row_col_char);
        let p = self.screen_row_col_char_to_camera_frame_row_col_char(screen_row_col_char);
        dbg!("camera frame row_col: ", &p);
        let p = self.camera.frame_row_col_char_to_local_world_square(p);
        dbg!("local_world_square: ", &p);
        dbg!("camera: ", &self.camera);
        let p = self.camera.local_to_absolute_world_square(p);
        dbg!("absolute_world_square: ", &p);
        p
    }

    pub fn render(&mut self, world_state: &WorldState) -> Frame {
        let fov_center = self
            .mouse_world_point()
            .unwrap_or_else(|| world_state.player_square.to_float());
        let fov_range = self.default_fov_range;

        let world_frame = self
            .camera
            .render_world_with_radius(world_state, fov_center, fov_range);

        let mut screen_buffer = Frame::solid_color(
            self.screen_width(),
            self.screen_height(),
            UI_BACKGROUND_RGB.into(),
        );
        screen_buffer.blit(&world_frame, [0, 0]);
        self.draw_mouse(&mut screen_buffer);
        screen_buffer
    }
}
fn press_char(c: char) -> Event {
    Event::Key(Key::Char(c))
}

#[cfg(test)]
mod ui_handler_tests {
    use super::*;
    use termion::event::*;
}

#[derive(Clone, Debug, PartialEq)]
struct WorldState {
    running: bool,
    s_from_start: f32,
    width: usize,
    height: usize,
    player_square: IPoint,
    smoothed_player_pos: FPoint,
    // seconds from start and recent steps
    player_step_history: VecDeque<(f32, IPoint)>,
    player_is_alive: bool,

    portals: HashMap<PortalSide, PortalSide>,

    pub portal_rendering: PortalRenderingOption,
    board_color_function: fn(&WorldState, IPoint) -> Option<RGB8>,
    portal_tint_function: fn(RGB8, u32) -> RGB8,
}
impl WorldState {
    pub fn new(width: usize, height: usize) -> Self {
        let player_square = [width, height].to_int().div(2);
        let mut state = WorldState {
            running: true,
            s_from_start: 0.0,
            width,
            height,
            player_square,
            smoothed_player_pos: player_square.to_float(),
            player_step_history: Default::default(),
            player_is_alive: false,
            portals: Default::default(),
            portal_rendering: PortalRenderingOption::LineOnFloor,
            board_color_function: Self::default_board_color,
            portal_tint_function: Self::default_portal_tint,
        };
        state
    }
    fn default_board_color(&self, square: IPoint) -> Option<RGB8> {
        let is_white = ((square[0] / 3).rem_euclid(2) == 0) == ((square[1] / 3).rem_euclid(2) == 0);
        Some(if is_white { grey(191) } else { grey(127) })
    }
    fn radial_sin_board_colors(&self, square: IPoint) -> Option<RGB8> {
        let [cx, cy] = self.player_square;
        let dx = square[0] - cx;
        let dy = square[1] - cy;
        let d = ((dx.pow(2) + dy.pow(2)) as f32).sqrt();
        let wave_length = 7.0;
        let mid = 120.0;
        let ampl = 50.0;

        let val = mid + ampl * (d / wave_length * std::f32::consts::TAU).cos();

        if self.on_board(square) {
            Some(grey(val.round() as u8))
        } else {
            None
        }
    }
    fn default_portal_tint(color: RGB8, depth: u32) -> RGB8 {
        let tint = RED;
        let strength = (0.1 * depth as f32).min(1.0);
        tint_color(color, tint, strength)
    }
    fn rainbow_tint(color: RGB8, depth: u32) -> RGB8 {
        if depth == 0 {
            return color;
        }
        let rainbow = [RED, ORANGE, YELLOW, GREEN, CYAN, BLUE, MAGENTA];
        let tint = rainbow[(depth.saturating_sub(1) as usize).rem_euclid(rainbow.len())];
        let strength = (0.1 * depth as f32).min(1.0);
        tint_color(color, tint, strength)
    }
    fn rainbow_solid(color: RGB8, depth: u32) -> RGB8 {
        if depth == 0 {
            return color;
        }
        let rainbow = [RED, ORANGE, YELLOW, GREEN, CYAN, BLUE, MAGENTA];
        rainbow[(depth.saturating_sub(1) as usize).rem_euclid(rainbow.len())]
    }
    pub fn size_width_height(&self) -> UPoint {
        [self.width as u32, self.height as u32]
    }

    pub fn on_board(&self, square: IPoint) -> bool {
        square[0] >= 0
            && square[0] < self.width as i32
            && square[1] >= 0
            && square[1] < self.height as i32
    }

    pub fn place_portal(&mut self, entrance: PortalSide, reverse_entrance: PortalSide) {
        self.portals.insert(entrance, reverse_entrance);
        self.portals.insert(reverse_entrance, entrance);
        self.portals.insert(
            other_side_of_edge(entrance),
            other_side_of_edge(reverse_entrance),
        );
        self.portals.insert(
            other_side_of_edge(reverse_entrance),
            other_side_of_edge(entrance),
        );
    }

    fn naive_glyphs_for_rotated_world_square(
        &self,
        square: IPoint,
        rotation: i32,
    ) -> DoubleGlyphWithTransparency {
        assert!(rotation >= 0 && rotation < 4);
        todo!();
    }
    // Simple top-down, no rotation, no portals (except for entrance/exit)
    fn naive_glyphs_for_world_square(&self, square: IPoint) -> DoubleGlyphWithTransparency {
        self.naive_glyphs_for_rotated_world_square(square, 0)
    }
    fn mouse_square_xy_in_world_frame(&self) -> Option<IPoint> {
        todo!()
    }

    // first part of output tuple: frame to render
    // Second part of output tuple: list of frames that combine to the output frame.  Each frame in
    // the list is a different coherent view through portals
    //   - Same portal depth
    //   - Same Portal tint (currently identical to portal depth) (may also count non-portal light
    //   sources in future)
    //   - Same transform from local to absolute frame, even if the portals are separated
    //   physically
    fn render_with_debug_layers(&self, fov_center: FPoint, radius: u32) -> (Frame, Vec<Frame>) {
        self.render_with_options(true, fov_center, radius)
    }
    fn render_with_options(
        &self,
        is_debug: bool,
        fov_center: FPoint,
        radius: u32,
    ) -> (Frame, Vec<Frame>) {
        let portal_geometry =
            game::portal_geometry::PortalGeometry::from_entrances_and_reverse_entrances(
                self.portals.clone(), // .iter()
                                      // .map(|(&entrance, &reverse_exit)| (entrance, reverse_exit.rotate(2)))
                                      // .collect(),
            );
        // panic!();

        let fov = game::fov_stuff::portal_aware_field_of_view_from_point(
            fov_center.into(),
            radius,
            &Default::default(),
            &portal_geometry,
        );

        let shadow_glyph = GlyphWithTransparency::from_char(SHADOW_CHAR);
        let out_of_range_glyph = GlyphWithTransparency::from_char(OUT_OF_FOV_RANGE_CHAR);

        // Key is (depth, absolute_position, rotation from portal)
        let mut debug_portal_visualizer_frames: HashMap<(u32, [i32; 2], i32), Frame> =
            Default::default();

        let camera_width = radius as usize * 2 + 1;
        let camera_height = camera_width;
        // let fov_center_square = fov_center.rounded();
        // let camera_min_x = fov_center_square[0] - radius as i32;
        // let camera_max_x = fov_center_square[0] + radius as i32;
        // let camera_min_y = fov_center_square[1] - radius as i32;
        // let camera_max_y = fov_center_square[1] + radius as i32;

        let mut transpareny_frame: Vec<Vec<DoubleGlyphWithTransparency>> = (0..camera_height)
            .map(|row_in_camera_frame| {
                let y_in_camera_frame = self.height as i32 - row_in_camera_frame as i32 - 1;
                (0..camera_width as i32)
                    .map(|x_in_camera_frame| {
                        let col_in_camera_frame = x_in_camera_frame as usize;
                        // let x = col;
                        // let y = self.height - row - 1;
                        let pos_in_camera_frame: WorldSquare = [x_in_camera_frame, y_in_camera_frame].into();
                        let camera_pos_relative_to_fov_center =
                            pos_in_camera_frame - WorldSquare::from(fov_center.rounded());
                        let visible_portions_at_relative_square: Vec<
                            PositionedSquareVisibilityInFov,
                        > = match self.portal_rendering {
                            PortalRenderingOption::LineOfSight => {
                                FieldOfViewResult::sorted_by_draw_order(
                                    fov.visibilities_of_relative_square(
                                        camera_pos_relative_to_fov_center,
                                    ),
                                )
                            }
                            PortalRenderingOption::LineOnFloor => {
                                vec![PositionedSquareVisibilityInFov::new_in_top_view(
                                    SquareVisibility::new_fully_visible(),
                                    pos_in_camera_frame,
                                    camera_pos_relative_to_fov_center,
                                )]
                            }
                            PortalRenderingOption::Absolute => todo!(),
                        };
                        let current_radius = camera_pos_relative_to_fov_center.to_array().iter().map(|x|x.abs() as u32).max().unwrap();
                        if !visible_portions_at_relative_square.is_empty() {
                            assert!(current_radius <= radius, "rel_pos: {camera_pos_relative_to_fov_center:?}, fov_radius: {radius}");
                        }

                        let glyph_layers_to_combine: Vec<DoubleGlyphWithTransparency> =
                            visible_portions_at_relative_square
                                .clone()
                                .into_iter()
                                .map(|square_viz| self.render_one_view_of_a_square(&square_viz))
                                .collect_vec();
                        if is_debug {
                            visible_portions_at_relative_square
                                .iter()
                                .zip(glyph_layers_to_combine.iter())
                                .for_each(|(square_viz, double_glyph)| {
                                    let debug_frames_key: (u32, IPoint, i32) = (
                                        square_viz.portal_depth(),
                                        square_viz.absolute_fov_center_square().into(),
                                        square_viz
                                            .portal_rotation_from_relative_to_absolute()
                                            .quarter_turns()
                                            .rem_euclid(4),
                                    );
                                    if !debug_portal_visualizer_frames
                                        .contains_key(&debug_frames_key)
                                    {
                                        debug_portal_visualizer_frames.insert(
                                            debug_frames_key,
                                            Frame::blank(self.width * 2, self.height),
                                        );
                                    }
                                    let mut debug_frame = debug_portal_visualizer_frames
                                        .get_mut(&debug_frames_key)
                                        .unwrap();
                                    debug_frame.set_by_double_wide_grid(
                                        row_in_camera_frame,
                                        col_in_camera_frame,
                                        double_glyph
                                            .map(|g| g.to_drawable_with_transparent_as_default()),
                                    );
                                });
                        }

                        let default = if current_radius <= radius {shadow_glyph} else {out_of_range_glyph};
                        glyph_layers_to_combine
                            .into_iter()
                            .rev()
                            .reduce(|below, above| [0, 1].map(|i| above[i].over(below[i])))
                            .unwrap_or([default; 2])
                    })
                    .collect_vec()
            })
            .collect_vec();

        let frame = transpareny_frame
            .into_iter()
            .map(|row| {
                row.into_iter()
                    .flatten()
                    .map(|g| g.to_drawable_with_transparent_as_default())
                    .collect_vec()
            })
            .collect_vec()
            .into();

        (
            frame,
            debug_portal_visualizer_frames.into_values().collect_vec(),
        )
    }
    fn portal_entrance_glyphs(
        &self,
        mapped_square: MappedSquare,
    ) -> Option<DoubleGlyphWithTransparency> {
        let absolute_internal_faces_with_portal_entrance = ALL_ORTHODIRS.map(|dir| {
            self.portals
                .contains_key(&(mapped_square.absolute_square.into(), dir))
        });
        let internal_faces_with_visible_portal_entrances = match self.portal_rendering {
            PortalRenderingOption::LineOfSight => {
                let visible_relative_internal_faces = ALL_ORTHODIRS.map(|dir| {
                    !mapped_square
                        .relative_square
                        .to_array()
                        .has_component_against_direction(dir)
                });
                let mut relative_internal_faces_with_portal_entrance =
                    absolute_internal_faces_with_portal_entrance.clone();
                relative_internal_faces_with_portal_entrance.rotate_right(
                    mapped_square
                        .quarter_turns_ccw_from_absolute_to_relative()
                        .quarter_turns() as usize,
                );
                array_zip(
                    visible_relative_internal_faces,
                    relative_internal_faces_with_portal_entrance,
                )
                .map(|(a, b)| a && b)
            }
            _ => absolute_internal_faces_with_portal_entrance,
        };
        let any_entrances_visible_here = internal_faces_with_visible_portal_entrances
            .iter()
            .any(|x| *x);
        if any_entrances_visible_here {
            let visible_portal_entrance_characters =
                chars_for_square_walls(internal_faces_with_visible_portal_entrances);
            let visible_portal_entrance_glyphs = visible_portal_entrance_characters.map(|c| {
                GlyphWithTransparency::from_char(c)
                    .with_primary_rgb(RED)
                    .with_primary_only()
            });
            return Some(visible_portal_entrance_glyphs);
        }
        None
    }
    pub fn advance_time_to(&mut self, s_from_start: f32) {
        let dt_s = s_from_start - self.s_from_start;
        assert!(dt_s > 0.0, "dt must be positive: {dt_s}");

        self.advance_smoothed_player_pos(dt_s);
        self.s_from_start = s_from_start;
    }

    pub fn render(&self, fov_center: FPoint, radius: u32) -> Frame {
        self.render_with_options(false, fov_center, radius).0
    }
    fn render_one_view_of_a_square(
        &self,
        square_viz: &PositionedSquareVisibilityInFov,
    ) -> [GlyphWithTransparency; 2] {
        let abs_square = square_viz.absolute_square();
        let square_is_in_world = self.on_board(abs_square.to_array());
        let default_glyphs = [GlyphWithTransparency::from_char(ETERNAL_VOID_CHAR); 2];

        let mut glyphs = if square_is_in_world {
            if let Some(board_color) = (self.board_color_function)(&self, abs_square.into()) {
                DoubleGlyphWithTransparency::solid_color(board_color)
            } else {
                default_glyphs
            }
        } else {
            default_glyphs
        };

        // draw visible portal entrances

        if let Some(visible_portal_entrance_glyphs) =
            self.portal_entrance_glyphs(square_viz.mapped_square())
        {
            glyphs = visible_portal_entrance_glyphs.over(glyphs);
        }

        if square_viz.absolute_square().to_array() == self.player_square && self.player_is_alive {
            glyphs = DoubleGlyphWithTransparency::solid_color(color_from_hex!("#1688f0").into());
        }

        // apply tint
        glyphs.iter_mut().for_each(|glyph| {
            *glyph.primary_color.rgb_mut() =
                (self.portal_tint_function)(glyph.primary_color.rgb(), square_viz.portal_depth());
            *glyph.secondary_color.rgb_mut() =
                (self.portal_tint_function)(glyph.secondary_color.rgb(), square_viz.portal_depth())
        });

        // apply obscurement
        if !square_viz
            .square_visibility_in_absolute_frame()
            .is_fully_visible()
        {
            let bias_direction = square_viz
                .square_visibility_in_relative_frame()
                .visible_portion()
                .unwrap()
                .direction_away_from_plane();
            glyphs = glyphs
                .into_iter()
                .zip(
                    square_viz
                        .square_visibility_in_relative_frame()
                        .split_into_character_visibilities()
                        .into_iter(),
                )
                .map(
                    |(glyph, visible_portion_of_glyph)| match visible_portion_of_glyph {
                        None => glyph,
                        Some(visible_portion) => {
                            let window = half_plane_to_angled_block_character(
                                visible_portion,
                                bias_direction,
                            );
                            glyph.seen_through_window(window)
                        }
                    },
                )
                .collect_vec()
                .try_into()
                .unwrap()
        }
        glyphs
    }
    // given single square step from the start point in the direction, where do you end up, and
    // from what direction did you come?
    pub fn portal_step(&self, start: IPoint, dir: OrthoDir) -> (IPoint, OrthoDir) {
        let pose = (start, dir);
        if let Some(reverse_entrance) = self.portals.get(&pose) {
            reverse_entrance.reversed()
        } else {
            pose.stepped()
        }
    }

    pub fn advance_smoothed_player_pos(&mut self, dt_s: f32) {
        self.smoothed_player_pos = exponential_approach_with_min_speed(
            self.smoothed_player_pos,
            self.player_square.to_float(),
            dt_s,
            0.2,
            15.0,
        );
    }
}

fn draw_frame(writable: &mut impl Write, new_frame: &Frame, maybe_old_frame: &Option<Frame>) {
    writable.write(
        &new_frame
            .string_for_raw_display_over(maybe_old_frame)
            .into_bytes(),
    );
    writable.flush();
}

fn main() {
    let n = 5;
    let mut game = Game::new(n, n);
    game.world_state.player_is_alive = true;
    game.world_state.portal_rendering = PortalRenderingOption::LineOfSight;
    // game.world_state
    //     .place_portal(([10, 5], DIR_UP), ([15, 15], DIR_RIGHT));
    game.ui_handler.enable_mouse_smoothing = true;

    set_up_panic_hook();

    while game.world_state.running {
        let now = std::time::Instant::now();
        let s_from_start = game.ui_handler.instant_to_s_from_start(now);
        game.ui_handler.advance_time_to(s_from_start);
        game.process_events();

        let mut frame = game.render();
        frame.draw_text(
            format!(
                "{:<30}",
                game.ui_handler.last_mouse_screen_row_col.to_debug()
            ),
            [(game.ui_handler.screen_height() + 1).into(), 0],
        );
        for (i, event) in game.ui_handler.event_log.iter().enumerate() {
            frame.draw_text(
                format!("{:<30}", format!("{:?}", event)),
                [game.ui_handler.screen_height() as usize + 3 + i, 0],
            );
        }
        game.ui_handler.draw_screen(frame);
        thread::sleep(Duration::from_millis(21));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use game::fov_stuff::LocalSquareHalfPlane;
    use ordered_float::OrderedFloat;
    use pretty_assertions::assert_str_eq;
    use std::str::FromStr;
    use std::{assert_eq, assert_ne, f32::consts::TAU, iter::once, ops::Sub};
    use terminal_rendering::assert_array_not_more_than_past;
    use terminal_rendering::test_utils::*;
    use terminal_rendering::*;
    use termion::event::MouseEvent;
    use utility::geometry2::STEP_RIGHT;

    #[test]
    fn test_simple_output() {
        let state = WorldState::new(10, 10);
        let frame = state.render([5.0, 5.0], 5);
        assert_eq!(frame.width(), 22);
        assert_eq!(frame.height(), 11);
    }

    fn press_left_row_col_screen_pos(screen_pos_row_col: [u16; 2]) -> termion::event::Event {
        press_left(screen_pos_row_col[1], screen_pos_row_col[0])
    }
    fn press_left(col: u16, row: u16) -> termion::event::Event {
        termion::event::Event::Mouse(termion::event::MouseEvent::Press(
            termion::event::MouseButton::Left,
            col,
            row,
        ))
    }
    fn drag_mouse_to_screen_pos(row_col: [u16; 2]) -> termion::event::Event {
        drag_mouse_to(row_col[1], row_col[0])
    }
    fn drag_mouse_to(col: u16, row: u16) -> termion::event::Event {
        termion::event::Event::Mouse(termion::event::MouseEvent::Hold(col, row))
    }
    fn release_mouse(col: u16, row: u16) -> termion::event::Event {
        termion::event::Event::Mouse(termion::event::MouseEvent::Release(col, row))
    }

    #[test]
    fn test_click_a() {
        let mut game = Game::new_headless_square(9);

        game.ui_handler.give_event(press_left(1, 1));
        game.process_events();
        let frame = game.render();
        // let no_color = frame.uncolored_regular_string();
        dbg!(&frame);
        assert_frame_same_as_past!(frame, "a");
    }
    #[test]
    fn test_click_a_small() {
        let mut game = Game::new_headless_square(3);
        game.ui_handler.give_event(press_left(1, 1));
        game.process_events();
        let frame = game.render();
        // dbg!(&frame);
        // dbg!(&frame.grid);
        assert_frame_same_as_past!(frame, "a", true);
    }
    #[test]
    fn test_click_b() {
        let mut game = Game::new_headless_square(13);
        game.ui_handler.give_event(press_left(4, 11));
        game.process_events();
        let frame = game.render();
        dbg!(&frame);
        eprintln!("{}", frame.string_for_regular_display());
        assert_ne!(frame.get_xy([2, 2]).bg_color, RED.into());
        assert_eq!(frame.get_xy([3, 2]).bg_color, RED.into());
        assert_frame_same_as_past!(frame, "a");
    }
    #[test]
    fn test_drag_mouse() {
        let mut game = Game::new_headless(11, 22, 12, 12);
        game.ui_handler.give_event(press_left(4, 4));
        game.process_events();
        let frame_1 = game.render();
        game.ui_handler.give_event(drag_mouse_to(5, 4));
        game.process_events();
        let frame_2 = game.render();
        game.ui_handler.give_event(drag_mouse_to(6, 4));
        game.process_events();
        let frame_3 = game.render();
        // dbg!(&frame_1, &frame_2, &frame_3);
        assert_frame_same_as_past!(frame_1, "1");
        assert_frame_same_as_past!(frame_2, "2");
        assert_frame_same_as_past!(frame_3, "3");
    }
    #[test]
    fn test_render_portal_edges() {
        let mut game = Game::new_headless_square(13);
        game.world_state
            .place_portal(([1, 1], DIR_UP), ([1, 3], DIR_UP));
        game.world_state
            .place_portal(([3, 1], DIR_UP), ([3, 3], DIR_UP));
        game.world_state
            .place_portal(([3, 1], DIR_DOWN), ([3, 3], DIR_DOWN));
        game.world_state
            .place_portal(([5, 1], DIR_UP), ([5, 3], DIR_UP));
        game.world_state
            .place_portal(([5, 1], DIR_DOWN), ([5, 3], DIR_DOWN));
        game.world_state
            .place_portal(([5, 1], DIR_RIGHT), ([5, 3], DIR_RIGHT));
        game.world_state
            .place_portal(([7, 1], DIR_UP), ([7, 3], DIR_UP));
        game.world_state
            .place_portal(([7, 1], DIR_DOWN), ([7, 3], DIR_DOWN));
        game.world_state
            .place_portal(([7, 1], DIR_RIGHT), ([7, 3], DIR_RIGHT));
        game.world_state
            .place_portal(([7, 1], DIR_LEFT), ([7, 3], DIR_LEFT));
        game.world_state
            .place_portal(([9, 1], DIR_DOWN), ([9, 3], DIR_DOWN));
        game.world_state
            .place_portal(([9, 1], DIR_RIGHT), ([9, 3], DIR_RIGHT));
        game.world_state
            .place_portal(([9, 1], DIR_LEFT), ([9, 3], DIR_LEFT));
        game.world_state.portal_rendering = PortalRenderingOption::LineOnFloor;
        let frame = game.render();
        assert_frame_same_as_past!(frame, "a");
    }
    #[test]
    fn test_render_part_of_square() {
        let mut game = Game::new_headless_square(13);
        game.world_state.board_color_function = |_state, _square| Some(GREEN);

        let visible_portion = PositionedSquareVisibilityInFov {
            square_visibility_in_absolute_frame: SquareVisibility::from_visible_half_plane(
                LocalSquareHalfPlane::from_clockwise_sweeping_line([[3.0, 2.0], [0.0, 0.0]].into()),
            )
            .unwrap(),
            relative_square: [2, 2].into(),
            absolute_square: [2, 2].into(),
            portal_depth: 0,
            portal_rotation_from_relative_to_absolute: QuarterTurnsAnticlockwise::new(0),
        };
        let glyphs = game
            .world_state
            .render_one_view_of_a_square(&visible_portion);
        println!("{}{}", glyphs[0].to_string(), glyphs[1].to_string());
        assert_eq!(glyphs[0].character, '🭞');
        assert_eq!(glyphs[0].fg_color(), GREEN.into());
        assert_eq!(glyphs[0].bg_color(), Glyph::default_bg_color.with_alpha(0));
        assert_eq!(glyphs[1].character, '🭜');
        assert_eq!(glyphs[1].fg_color(), GREEN.into());
        assert_eq!(glyphs[1].bg_color(), Glyph::default_bg_color.with_alpha(0));
        // println!("{}",glyphs.to_clean_string());
    }
    #[test]
    fn test_render_part_of_square_with_rotation() {
        let mut game = Game::new_headless_square(13);
        game.world_state.board_color_function = |_state, _square| Some(GREEN);

        let mut frame = Frame::blank(20, 3);

        for i in 0..4 {
            let visible_portion = PositionedSquareVisibilityInFov {
                square_visibility_in_absolute_frame: SquareVisibility::from_visible_half_plane(
                    LocalSquareHalfPlane::from_clockwise_sweeping_line(
                        [[3.0, 2.0], [0.0, 0.0]].into(),
                    ),
                )
                .unwrap(),
                relative_square: [2, 2].into(),
                absolute_square: [2, 2].into(),
                portal_depth: 0,
                portal_rotation_from_relative_to_absolute: i.into(),
            };
            let glyphs = game
                .world_state
                .render_one_view_of_a_square(&visible_portion)
                .map(|g| g.to_drawable_with_transparent_as_default());
            frame.set_by_double_wide_grid(1, 2 * i as usize + 1, glyphs);
        }

        assert_frame_same_as_past!(frame, "a");
    }
    #[test]
    fn test_render_one_line_of_sight_portal() {
        let mut game = Game::new_headless_square(13);
        game.world_state.player_square = [5, 5];
        game.world_state.portal_rendering = PortalRenderingOption::LineOfSight;
        game.world_state.board_color_function = WorldState::radial_sin_board_colors;
        game.world_state
            .place_portal(([5, 7], DIR_UP), ([7, 10], DIR_UP));
        // game.portal_tint_function = GameState::rainbow_solid;
        // dbg!(game.render(None));
        game.world_state.portal_tint_function = WorldState::rainbow_tint;
        let frame = game.render();
        let (_, debug_layers) = game
            .world_state
            .render_with_debug_layers(game.world_state.smoothed_player_pos, 5);
        debug_layers.into_iter().for_each(|frame| {
            dbg!(frame);
        });
        assert_frame_same_as_past!(frame, "a");
    }
    #[test]
    fn test_portal_with_rotation() {
        let mut game = Game::new_headless_square(13);
        game.world_state.player_square = [5, 5];
        game.world_state.portal_rendering = PortalRenderingOption::LineOfSight;
        game.world_state.board_color_function = |world_state, square| {
            let n = 10;
            let frac = square.y().rem_euclid(n) as f32 / n as f32;
            let val = 100 + (100.0 * frac) as u8;
            Some(grey(val))
        };

        WorldState::radial_sin_board_colors;
        game.world_state
            .place_portal(([5, 7], DIR_UP), ([7, 10], DIR_RIGHT));
        // game.portal_tint_function = GameState::rainbow_solid;
        // dbg!(game.render(None));
        game.world_state.portal_tint_function = WorldState::rainbow_tint;
        let (frame, layers) = game.world_state.render_with_debug_layers(
            game.world_state.smoothed_player_pos,
            game.ui_handler.default_fov_range,
        );
        layers.into_iter().for_each(|frame| {
            dbg!(frame);
        });

        assert_frame_same_as_past!(frame, "a");
    }

    fn sim_mouse_path(
        path_fn: impl Fn(f32) -> FPoint,
        sample_rate: f32,
        end_t: f32,
    ) -> Vec<(f32, FPoint)> {
        let n = (end_t * sample_rate).round() as u32;
        (0..=n)
            .map(|i| {
                let t = i as f32 / sample_rate;
                (t, path_fn(t))
            })
            .collect_vec()
    }
    fn path_to_square_entry_events(path: &[(f32, FPoint)]) -> Vec<(f32, IPoint)> {
        let moves = path.iter().tuple_windows().filter_map(|(a, b)| {
            if a.1.rounded() != b.1.rounded() {
                Some((b.0, b.1.rounded()))
            } else {
                None
            }
        });
        once((path[0].0, path[0].1.rounded()))
            .chain(moves)
            .collect_vec()
    }

    fn smoothed_mouse_path(
        square_entry_events: &[(f32, IPoint)],
        sample_rate: f32,
        end_t: f32,
        smoothing_function: fn(&[(f32, IPoint)], f32) -> FPoint,
    ) -> Vec<(f32, FPoint)> {
        assert!(square_entry_events.len() > 0);
        let t0 = square_entry_events[0].0;
        let mut t = t0;
        let mut i = 0;
        let mut out = vec![];
        while t < end_t + 0.0001 {
            while t >= square_entry_events[i].0 && i < square_entry_events.len() - 1 {
                i += 1;
            }
            out.push((t, smoothing_function(&square_entry_events[0..i], t)));
            t += 1.0 / sample_rate;
        }
        out
    }
    #[test]
    fn test_screen_row_col_to_xy_points() {
        let mut game = Game::new_headless_square(5);

        // .....
        // .....
        // .....
        // .....
        // .....

        let row_col_point_upper_left = [1.0, 1.0];
        let xy_point_upper_left = [0.0, 5.0];

        let row_col_point_lower_left = [6.0, 1.0];
        let xy_point_lower_left = [0.0, 0.0];

        assert_eq!(
            game.ui_handler
                .screen_row_col_point_to_screen_xy_point(row_col_point_upper_left),
            xy_point_upper_left
        );
        assert_eq!(
            game.ui_handler
                .screen_xy_point_to_screen_row_col_point(xy_point_upper_left),
            row_col_point_upper_left
        );

        assert_eq!(
            game.ui_handler
                .screen_row_col_point_to_screen_xy_point(row_col_point_lower_left),
            xy_point_lower_left
        );
        assert_eq!(
            game.ui_handler
                .screen_xy_point_to_screen_row_col_point(xy_point_lower_left),
            row_col_point_lower_left
        );
    }

    #[test]
    fn test_render_smoothed_mouse_stationary() {
        let mut game = Game::new_headless_square(3);
        game.ui_handler.enable_mouse_smoothing = true;
        game.ui_handler.give_event(press_left(1, 1));
        game.process_events();
        assert_eq!(game.ui_handler.event_log.len(), 1);
        assert!(game.ui_handler.smoothed_mouse_screen_row_col.is_some());
        let frame = game.render();
        println!("{}", &frame.escaped_regular_display_string());
        assert!(
            char_is_braille(frame.grid[0][0].character),
            "Char is not braille:\n\n{frame:?}"
        );
        assert_frame_same_as_past!(frame, "a", true);
    }
    #[test]
    fn test_smoothed_mouse_linear_move() {
        let mut game = Game::new_headless_square(3);
        game.ui_handler.enable_mouse_smoothing = true;
        game.ui_handler.give_event(press_left(1, 1));
        game.process_events();
        game.ui_handler.advance_time_to(0.2);
        game.ui_handler.give_event(drag_mouse_to(2, 1));
        game.process_events();

        game.ui_handler.advance_time_by(0.0001);
        let pos = game
            .ui_handler
            .smoothed_mouse_position_screen_row_col()
            .unwrap();
        dbg!(pos);
        assert!(pos[1] > 1.0);
        assert!(pos[1] < 1.1);
        game.ui_handler.advance_time_to(5.0);
        let pos = game
            .ui_handler
            .smoothed_mouse_position_screen_row_col()
            .unwrap();
        dbg!(pos);
        assert!(pos[0] - 2.0 < 0.0001);
        // assert_frame_same_as_past!(frame, "a", true);
    }

    #[test]
    fn test_player_step_through_portal() {
        let mut game = Game::new_headless_square(5);
        game.world_state
            .place_portal(([1, 2], DIR_RIGHT), ([3, 2], DIR_LEFT));
        game.world_state.player_square = [1, 2];

        game.try_move_player([1, 0]);
        assert_eq!(game.world_state.player_square, [3, 2]);
    }
    #[test]
    fn test_player_step_history_starts_empty() {
        let mut game = Game::new_headless_square(5);
        assert!(game.world_state.player_step_history.is_empty());
    }
    #[ignore]
    #[test]
    fn test_draw_smoothed_player_position() {
        let mut game = Game::new_headless_square(5);
        game.world_state.player_square = [0, 2];
        game.try_move_player_at_time(STEP_RIGHT, 0.5);
        game.try_move_player_at_time(STEP_RIGHT, 1.0);
        let frame = game.render();
        dbg!(&frame);
        let player_pos_in_frame = game
            .ui_handler
            .camera
            .absolute_world_square_to_left_frame_row_col(game.world_state.player_square);
        let [row, left_col] = player_pos_in_frame.map(|x| x as usize);
        let right_player_glyph: DrawableGlyph = frame.grid[row][left_col + 1];
        assert!(char_is_braille(right_player_glyph.character));
    }
    #[test]
    fn test_render_with_center_offset() {
        let mut game = Game::new_headless_square(25);
        game.ui_handler
            .give_future_event_absolute(press_left(5, 6), 1.0);
        game.advance_time_to(1.0);
        game.process_events();
        game.ui_handler
            .give_future_event_absolute(drag_mouse_to(6, 6), 2.0);
        game.advance_time_to(2.0);
        game.process_events();
        let n = 43;
        (0..n).for_each(|x| {
            let t = 1.5 + x as f32 / 20.0;
            let p = [5.0 + x as f32 / 20.0, 8.0 + x as f32 / n as f32 * 5.0];
            let frame = game.world_state.render(p, 10);
            // dbg!( &p);
            frame.glyphs().for_each(|g| assert!(g.looks_solid()));
        });
    }
    #[test]
    fn test_give_and_process_event_with_no_time_advancement() {
        let mut game = Game::new_headless_square(9);
        game.process_events();
        game.ui_handler.give_event(press_char('a'));
        game.ui_handler.receive_events();
        assert_eq!(game.ui_handler.event_queue.len(), 1);
        assert_eq!(game.ui_handler.event_log.len(), 0);

        game.process_events();
        assert_eq!(game.ui_handler.event_queue.len(), 0);
        assert_eq!(game.ui_handler.event_log.len(), 1);
    }
    #[test]
    fn test_give_events_and_advance_time() {
        let mut game = Game::new_headless_square(9);
        game.advance_time_by(1.0);
        game.ui_handler.give_event(press_char('a'));
        game.ui_handler.receive_events();
        assert_eq!(game.ui_handler.event_queue.len(), 1);
        game.advance_time_by(1.0);
        game.process_events();
        assert_eq!(game.ui_handler.event_queue.len(), 0);
        assert_eq!(game.ui_handler.event_log.len(), 1);
        game.ui_handler
            .give_future_event_absolute(press_char('b'), 2.5);
        game.ui_handler
            .give_future_event_relative(press_char('c'), 0.7);
        game.process_events();
        assert_eq!(game.ui_handler.event_queue.len(), 2);
        assert_eq!(game.ui_handler.event_log.len(), 1);
        game.advance_time_by(0.6);
        game.process_events();
        assert_eq!(game.ui_handler.event_log.len(), 2);
        assert_eq!(game.ui_handler.event_queue.len(), 1);
        game.advance_time_by(300.0);
        game.process_events();
        assert_eq!(game.ui_handler.event_log.len(), 3);
        assert_eq!(game.ui_handler.event_queue.len(), 0);
    }
    #[test]
    fn test_odd_screen_sizes() {
        Game::new_headless_square(9); // odd square
        Game::new_headless_square(8); // even square
        Game::new_headless(8, 16, 3, 4);
        Game::new_headless(10, 20, 30, 30);
    }
    #[test]
    fn test_smoothed_mouse_time_step_length_independence() {
        let steps = [10, 2];
        let steps_and_times = steps.map(|n| {
            let mut game = Game::new_headless_square(40);
            game.ui_handler
                .give_event(press_left_row_col_screen_pos([10, 3]));
            let end_time = 1.0;
            game.advance_time_n_steps(end_time / n as f32, n);
            (
                game.ui_handler
                    .smoothed_mouse_position_screen_row_col()
                    .unwrap(),
                game.ui_handler.s_from_start,
            )
        });

        dbg!(&steps_and_times);
        assert!(steps_and_times[0].0.dist(steps_and_times[1].0) < 0.0001);
    }
    #[test]
    fn test_smoothed_mouse_is_fast() {
        let mut game = Game::new_headless_square(40);
        game.ui_handler
            .give_event(press_left_row_col_screen_pos([10, 3]));
        game.advance_time_by(0.5);
        assert_eq!(
            game.ui_handler.smoothed_mouse_screen_row_col,
            Some([10.0, 3.0])
        );
        game.ui_handler
            .give_event(drag_mouse_to_screen_pos([10, 30]));
        // Mouse must move really fast
        game.advance_time_by(0.5);
        assert_eq!(
            game.ui_handler.smoothed_mouse_screen_row_col,
            Some([10.0, 30.0])
        );
    }
    // #[ignore]
    #[test]
    fn test_big_screen_small_world_click() {
        let mut game = Game::new_headless(10, 30, 4, 4);
        game.ui_handler.default_fov_range = 3;
        game.ui_handler.give_event(press_left(5, 6));
        game.process_events();
        let frame = game.render();
        dbg!(&frame);
        assert_frame_same_as_past!(frame, "a");
    }
    #[test]
    fn test_camera_local_world_squares_invariant_to_camera_motion() {
        let mut game = Game::new_headless(10, 30, 4, 4);
        let before = game.ui_handler.camera.top_left_local_square();
        game.ui_handler
            .camera
            .translate_local_square_to_absolute_square(
                game.ui_handler.camera.top_left_local_square(),
                [0, 3],
            );
        let after = game.ui_handler.camera.top_left_local_square();
        assert_eq!(before, after);
    }
    #[test]
    fn test_screen_to_world() {
        let mut game = Game::new_headless(10, 30, 4, 4);
        // let frame = game.render();
        // game.ui_handler.draw_screen(frame);
        game.ui_handler
            .camera
            .translate_local_square_to_absolute_square(
                game.ui_handler.camera.top_left_local_square(),
                [0, 3],
            );
        dbg!(game.ui_handler.camera.top_left_local_square());
        dbg!(game.ui_handler.camera, game.world_state.size_width_height());
        let f = |p| game.ui_handler.screen_row_col_char_to_world_square(p);
        let g = |p| game.ui_handler.screen_row_col_point_to_world_point(p);
        assert_eq!(f([0, 0]), [0, 3]);
        assert_eq!(f([0, 1]), [0, 3]);
        assert_eq!(f([0, 2]), [1, 3]);
        assert_eq!(f([0, 3]), [1, 3]);
        assert_eq!(f([1, 0]), [0, 2]);
        assert_eq!(f([1, 1]), [0, 2]);

        assert_eq!(g([0.0, 0.0]), [0.0, 3.0]);
        assert_eq!(g([0.0, 1.0]), [0.0, 3.0]);
        assert_eq!(g([0.0, 2.0]), [1.0, 3.0]);
        assert_eq!(g([0.0, 3.0]), [1.0, 3.0]);
        assert_eq!(g([1.0, 0.0]), [0.0, 2.0]);
        assert_eq!(g([1.0, 1.0]), [0.0, 2.0]);
    }
}
