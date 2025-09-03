#![allow(dead_code)]
#![allow(deprecated)]

use color_hex::color_from_hex;
use euclid::point2;
use game::fov_stuff::{
    FieldOfViewResult, MappedSquare, PositionedSquareVisibilityInFov, SquareVisibility,
};
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
        write!(stdout(), "{string_out}").expect("display panic info");

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
    pub fn new_headless_one_to_one_square(s: usize) -> Self {
        Self::new_headless(s as u16, s as u16 * 2, s, s)
    }
    pub fn new_headless(
        screen_height: u16,
        screen_width: u16,
        width: usize,
        height: usize,
    ) -> Self {
        Game {
            world_state: WorldState::new(width, height),
            ui_handler: UiHandler::new_headless(screen_height, screen_width),
        }
    }
    pub fn give_and_process_fake_event_now(&mut self, event: Event) {
        self.ui_handler.give_fake_event_now(event);
        assert!(self.try_process_next_event());
    }
    pub fn process_event(&mut self, time: f32, event: Event) {
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
                    self.ui_handler.last_mouse_screen_row_col = Some([row, col])
                }
                termion::event::MouseEvent::Release(col, row) => {
                    self.ui_handler.last_mouse_screen_row_col = None
                }
                termion::event::MouseEvent::Hold(col, row) => {
                    self.ui_handler.last_mouse_screen_row_col = Some([row, col])
                }
            },
            Event::Unsupported(items) => todo!(),
        };
    }
    pub fn process_next_event(&mut self) {
        let (time, event) = self.ui_handler.next_event();
        self.process_event(time, event)
    }
    pub fn try_process_next_event(&mut self) -> bool {
        // true if processed, false if no next found
        let Some((time, event)) = self.ui_handler.try_get_next_event() else {
            return false;
        };
        self.process_event(time, event);

        true
    }
    pub fn process_events_in_queue(&mut self) -> usize {
        let mut n = 0;
        while self.try_process_next_event() {
            n += 1;
        }
        n
    }
    pub fn render_with_mouse(&mut self, fov_center: Option<FPoint>) -> Frame {
        self.render_with_mouse_now(fov_center)
    }
    pub fn render_with_mouse_at_time(
        &mut self,
        fov_center: Option<FPoint>,
        time_from_start_s: f32,
    ) -> Frame {
        let frame = self.render(fov_center);
        self.ui_handler.screen_buffer.blit(&frame, [0, 0]);
        self.ui_handler.draw_mouse_at_time(time_from_start_s);
        self.ui_handler.screen_buffer.clone()
    }
    pub fn render_with_mouse_now(&mut self, fov_center: Option<FPoint>) -> Frame {
        self.render_with_mouse_at_time(
            fov_center,
            Instant::now()
                .duration_since(self.ui_handler.start_time)
                .as_secs_f32(),
        )
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
    pub fn render_now_with_debug(&self, fov_center: Option<FPoint>) -> (Frame, Vec<Frame>) {
        self.world_state
            .render_with_options(true, fov_center, self.now_as_s_from_start())
    }
    pub fn render(&self, fov_center: Option<FPoint>) -> Frame {
        self.world_state
            .render_with_options(false, fov_center, self.now_as_s_from_start())
            .0
    }
    pub fn render_at_time(&self, fov_center: Option<FPoint>, s_from_start: f32) -> Frame {
        self.world_state
            .render_with_options(false, fov_center, s_from_start)
            .0
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
    pub fn new() -> Self {
        Self::new_square(25)
    }
    pub fn new_square(s: usize) -> Self {
        Camera {
            lower_left_square: [0, 0],
            upper_right_square: [s as i32 - 1; 2],
        }
    }
    pub fn set_bottom_left_square(&mut self, bottom_left_square: IPoint) {
        let new = self.with_bottom_left_square(bottom_left_square);
        self.lower_left_square = new.lower_left_square;
        self.upper_right_square = new.upper_right_square;
    }
    pub fn with_bottom_left_square(&self, bottom_left_square: IPoint) -> Self {
        Self::from_bottom_left_and_size(bottom_left_square, self.size_in_world())
    }
    pub fn from_bottom_left_and_size(bottom_left_square: IPoint, width_height: [u32; 2]) -> Self {
        Self {
            lower_left_square: bottom_left_square,
            upper_right_square: bottom_left_square
                .add(width_height.to_signed())
                .sub([1, 1]),
        }
    }
    pub fn bottom_left_square(&self) -> IPoint {
        self.lower_left_square
    }
    pub fn translate(&mut self, movement: IPoint) {
        self.set_bottom_left_square(self.bottom_left_square().add(movement))
    }
    pub fn size_in_world(&self) -> [u32; 2] {
        [
            (self.upper_right_square[0] - self.lower_left_square[0])
                as u32
                + 1,
            (self.upper_right_square[1] - self.lower_left_square[1])
                as u32
                + 1,
        ]
    }
    pub fn width_in_world(&self) -> u32 {
        self.size_in_world()[0]
    }
    pub fn height_in_world(&self) -> u32 {
        self.size_in_world()[1]
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
    pub fn frame_row_col_point_to_local_world_point(&self, screen_row_col_point: FPoint) -> FPoint {
        [
            (screen_row_col_point[1] - 1.0) / 2.0,
            self.height_in_world() as f32 - (screen_row_col_point[0] - 1.0),
        ]
    }
    // Not one-to-one.  each square has two characters that map to it
    pub fn frame_row_col_char_to_local_world_square(
        &self,
        camera_local_row_col_char: [u16; 2],
    ) -> IPoint {
        [
            (camera_local_row_col_char[1] as i32 - 1) / 2,
            self.height_in_world() as i32 - (camera_local_row_col_char[0] as i32 - 1) - 1,
        ]
    }
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
        self.local_world_square_to_left_frame_row_col(self.absolute_to_local_world_square(absolute_world_square))
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
}

#[cfg(test)]
mod camera_tests {
    use super::*;
    #[test]
    fn test_frame_to_world_integers() {
        let s: i32 = 32;
        let camera = Camera::new_square(s as usize);

        assert_eq!(
            camera.frame_row_col_char_to_absolute_world_square([1, 1]),
            [0, s - 1]
        );

        assert_eq!(
            camera.frame_row_col_char_to_local_world_square([1, 1]),
            [0, s - 1]
        );
        assert_eq!(
            camera.frame_row_col_char_to_local_world_square([1, 2]),
            [0, s - 1]
        );

        assert_eq!(
            camera.frame_row_col_char_to_local_world_square([2, 1]),
            [0, s - 2]
        );
        assert_eq!(
            camera.frame_row_col_char_to_local_world_square([2, 2]),
            [0, s - 2]
        );

        assert_eq!(
            camera.frame_row_col_char_to_local_world_square([1, 3]),
            [1, s - 1]
        );
        assert_eq!(
            camera.frame_row_col_char_to_local_world_square([1, 4]),
            [1, s - 1]
        );

        assert_eq!(
            camera.local_to_absolute_world_square([0, s - 1]),
            [0, s - 1]
        );
        assert_eq!(camera.local_to_absolute_world_square([0, 0]), [0, 0]);
    }
    fn test_frame_to_world_floating_point() {
        let camera = Camera::new();

        assert_eq!(
            camera.frame_row_col_point_to_local_world_point([1.0, 1.0]),
            [0.0, camera.height_in_world() as f32]
        );
    }
    #[test]
    fn test_size() {
        let camera = Camera::new();
        assert_eq!(camera.size_in_world(), [25, 25]);
    }
    #[test]
    fn test_local_to_absolute_world_squares_simple() {
        let camera = Camera::new();
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
        let mut camera = Camera::new();
        assert_eq!(
            camera,
            camera.with_bottom_left_square(camera.bottom_left_square())
        );
    }
    #[test]
    fn test_local_to_absolute_world_squares_translation() {
        let mut camera = Camera::new();
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
    // newest events are added via `push_back`
    pub event_log: VecDeque<(f32, Event)>,
    // 1-indexed
    pub last_mouse_screen_row_col: Option<[u16; 2]>,
    pub output_writable: Option<Box<dyn Write>>,
    pub event_receiver: Receiver<(Instant, Event)>,
    pub fake_event_sender: Option<Sender<(Instant, Event)>>,
    pub prev_drawn: Option<Frame>,
    pub screen_buffer: Frame,
    pub enable_mouse_smoothing: bool,
    pub camera: Camera,
}
impl UiHandler {
    fn smoothed_mouse_position_screen_row_col(&self, t: f32) -> Option<FPoint> {
        let recent_events = self.recent_mouse_screen_char_entry_events_row_col();
        if recent_events.is_empty() {
            return None;
        }
        if recent_events.len() == 1 {
            let e = recent_events.first().unwrap();
            return Some([e.1 as f32, e.2 as f32]);
        }
        let now = std::time::Instant::now();
        let t0 = recent_events.first().unwrap().0;
        let formatted: Vec<(f32, IPoint)> = recent_events
            .into_iter()
            .map(|(t, row, col)| (t - t0, [row as i32, col as i32]))
            .collect_vec();

        Some(smoothed_mouse_position(&formatted, t - t0))
    }

    // time and screen positions of square entry events.  Including intial click.
    // Does not extend past last mouse click
    fn recent_mouse_screen_char_entry_events_row_col(&self) -> Vec<(f32, u16, u16)> {
        let mut mouse_events_since_last_release = self
            .event_log
            .iter()
            .rev() // newer to older
            .take_while(|(_t, e)| {
                !matches!(e, Event::Mouse(termion::event::MouseEvent::Release(_, _)))
            })
            .collect_vec();
        mouse_events_since_last_release.reverse(); // older to newer

        mouse_events_since_last_release
            .iter()
            .filter_map(|(t, e)| {
                let (row, col) = match e {
                    Event::Mouse(termion::event::MouseEvent::Press(
                        termion::event::MouseButton::Left,
                        col,
                        row,
                    )) => (row, col),
                    Event::Mouse(termion::event::MouseEvent::Hold(col, row)) => (row, col),
                    _ => return None,
                };
                Some((*t, *row, *col))
            })
            .collect_vec()
    }
    pub fn height(&self) -> usize {
        self.screen_buffer.height()
    }
    pub fn width(&self) -> usize {
        self.screen_buffer.width()
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
        let event_receiver = set_up_input_thread();
        Self::new_maybe_headless(
            term_height,
            term_width,
            Some(output_writable),
            event_receiver,
        )
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
        let screen_y: i32 = self.height() as i32 - i32::from(screen_row) - 1;
        [i32::from(screen_col), screen_y]
    }
    fn screen_row_col_point_to_screen_xy_point(&self, row_col_point: FPoint) -> FPoint {
        [
            row_col_point[1] - 1.0, // No longer one-indexed
            self.height() as f32 - (row_col_point[0] - 1.0),
        ]
    }
    fn screen_xy_point_to_screen_row_col_point(&self, xy_point: FPoint) -> FPoint {
        [
            self.height() as f32 - (xy_point[1]) + 1.0,
            xy_point[0] + 1.0, // back to 1-indexed
        ]
    }
    fn mouse_world_square(&self) -> Option<IPoint> {
        self.last_mouse_screen_row_col
            .map(|row_col| self.screen_row_col_char_to_world_square(row_col))
    }
    fn mouse_world_point(&self, s_from_start: f32) -> Option<FPoint> {
        let screen_mouse_point = self.smoothed_mouse_position_screen_row_col(s_from_start);
        screen_mouse_point.map(|p|self.screen_row_col_point_to_world_point(p))
    }
    pub fn new_headless(screen_height: u16, screen_width: u16) -> UiHandler {
        let (sender, receiver) = channel();
        let mut handler = Self::new_maybe_headless(screen_height, screen_width, None, receiver);
        handler.fake_event_sender = Some(sender);
        handler
    }
    pub fn new_maybe_headless(
        screen_height: u16,
        screen_width: u16,
        output_writable: Option<Box<dyn Write>>,
        event_receiver: Receiver<(Instant, Event)>,
    ) -> UiHandler {
        UiHandler {
            start_time: Instant::now(),
            event_log: Default::default(),
            last_mouse_screen_row_col: None,
            output_writable,
            event_receiver,
            fake_event_sender: None,
            prev_drawn: None,
            screen_buffer: Frame::blank(screen_width as usize, screen_height as usize),
            enable_mouse_smoothing: false,
            camera: Camera::new(),
        }
    }
    pub fn draw_mouse(&mut self) {
        let t = (Instant::now() - self.start_time).as_secs_f32();
        self.draw_mouse_at_time(t)
    }
    pub fn draw_mouse_at_time(&mut self, time_from_start_s: f32) {
        // draw mouse position on top
        if self.enable_mouse_smoothing {
            self.draw_smoothed_mouse(time_from_start_s);
        } else {
            self.draw_mouse_square();
        }
    }
    pub fn draw_smoothed_mouse(&mut self, time_from_start_s: f32) {
        if let Some(smoothed_mouse_pos_row_col) =
            self.smoothed_mouse_position_screen_row_col(time_from_start_s)
        {
            let smoothed_mouse_pos_xy =
                self.screen_row_col_point_to_screen_xy_point(smoothed_mouse_pos_row_col);

            let the_char: char = character_grid_point_xy_to_braille_char(smoothed_mouse_pos_xy);
            let [row_1i, col_1i] = smoothed_mouse_pos_row_col.rounded();
            assert!(row_1i > 0, "{row_1i}");
            assert!(row_1i <= self.height() as i32, "{row_1i}");
            assert!(col_1i > 0, "{col_1i}");
            assert!(col_1i <= self.width() as i32, "{col_1i}");
            let [row, col] = [row_1i as usize - 1, col_1i as usize - 1];
            self.screen_buffer.grid[row][col].character = the_char;
            self.screen_buffer.grid[row][col].fg_color = BLACK.into();
        }
    }
    pub fn draw_mouse_square(&mut self) {
        if let Some([row, col]) = self.last_mouse_screen_row_col {
            assert!(row > 0 && col > 0, "row: {row}, col: {col}");
            self.screen_buffer.grid[row as usize - 1][col as usize - 1] =
                DrawableGlyph::solid_color(RED);
        }
    }
    pub fn draw_screen(&mut self) {
        draw_frame(
            &mut self.output_writable.as_mut().unwrap(),
            &self.screen_buffer,
            &self.prev_drawn,
        );
        self.prev_drawn = Some(self.screen_buffer.clone());
    }

    pub fn try_get_next_event(&mut self) -> Option<(f32, Event)> {
        self.event_receiver.try_recv().ok().map(|(instant, event)| {
            let t = instant.duration_since(self.start_time).as_secs_f32();
            self.log_event((t, event.clone()));
            (t, event)
        })
    }
    pub fn next_event(&mut self) -> (f32, Event) {
        self.event_receiver
            .recv()
            .ok()
            .map(|(instant, event)| {
                let t = instant.duration_since(self.start_time).as_secs_f32();
                self.log_event((t, event.clone()));
                (t, event)
            })
            .unwrap()
    }
    fn log_event(&mut self, e: (f32, Event)) {
        self.event_log.push_back(e);
        while self.event_log.len() > 20 {
            self.event_log.pop_front();
        }
    }

    pub fn give_fake_event(&mut self, event: (f32, Event)) {
        let t = self.s_from_start_to_instant(event.0).clone();
        self.fake_event_sender
            .as_mut()
            .unwrap()
            .send((t, event.1))
            .unwrap()
    }
    pub fn give_fake_event_now(&mut self, event: Event) {
        self.fake_event_sender
            .as_mut()
            .unwrap()
            .send((Instant::now(), event))
            .unwrap()
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
        let p = self.screen_row_col_char_to_camera_frame_row_col_char(screen_row_col_char);
        let p = self.camera.frame_row_col_char_to_local_world_square(p);
        self.camera.local_to_absolute_world_square(p)
    }
}

struct WorldState {
    running: bool,
    width: usize,
    height: usize,
    player_square: IPoint,
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
        let mut state = WorldState {
            running: true,
            width,
            height,
            player_square: [5, 5],
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
    fn render_with_debug_deconstruction(
        &self,
        is_debug: bool,
        fov_center: Option<FPoint>,
        t: f32,
    ) -> (Frame, Vec<Frame>) {
        self.render_with_options(is_debug, fov_center, t)
    }
    fn render_with_options(
        &self,
        is_debug: bool,
        fov_center: Option<FPoint>,
        s_from_start: f32,
    ) -> (Frame, Vec<Frame>) {
        let portal_geometry =
            game::portal_geometry::PortalGeometry::from_entrances_and_reverse_entrances(
                self.portals.clone(), // .iter()
                                      // .map(|(&entrance, &reverse_exit)| (entrance, reverse_exit.rotate(2)))
                                      // .collect(),
            );
        // panic!();

        let fov_center = match fov_center {
            Some(x) => x,
            None => self.player_square.to_float(),
        };
        let fov = game::fov_stuff::portal_aware_field_of_view_from_point(
            fov_center.into(),
            10,
            &Default::default(),
            &portal_geometry,
        );

        let shadow_glyph = GlyphWithTransparency::from_char('.');

        // Key is (depth, absolute_position, rotation from portal)
        let mut debug_portal_visualizer_frames: HashMap<(u32, [i32; 2], i32), Frame> =
            Default::default();

        let mut transpareny_frame: Vec<Vec<DoubleGlyphWithTransparency>> = (0..self.height)
            .map(|camera_row| {
                let camera_y = self.height as i32 - camera_row as i32 - 1;
                (0..self.width as i32)
                    .map(|camera_x| {
                        let camera_col = camera_x as usize;
                        // let x = col;
                        // let y = self.height - row - 1;
                        let camera_pos: WorldSquare = [camera_x, camera_y].into();
                        let camera_pos_relative_to_fov_center =
                            camera_pos - WorldSquare::from(fov_center.rounded());
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
                                    camera_pos,
                                    camera_pos_relative_to_fov_center,
                                )]
                            }
                            PortalRenderingOption::Absolute => todo!(),
                        };

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
                                        camera_row,
                                        camera_col,
                                        double_glyph
                                            .map(|g| g.to_drawable_with_transparent_as_default()),
                                    );
                                });
                        }
                        glyph_layers_to_combine
                            .into_iter()
                            .rev()
                            .reduce(|below, above| [0, 1].map(|i| above[i].over(below[i])))
                            .unwrap_or([shadow_glyph; 2])
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

    pub fn render(&self, fov_center: Option<FPoint>, s_from_start: f32) -> Frame {
        self.render_with_options(false, fov_center, s_from_start).0
    }
    fn render_one_view_of_a_square(
        &self,
        square_viz: &PositionedSquareVisibilityInFov,
    ) -> [GlyphWithTransparency; 2] {
        let mut glyphs = if let Some(board_color) =
            (self.board_color_function)(&self, square_viz.absolute_square().into())
        {
            DoubleGlyphWithTransparency::solid_color(board_color)
        } else {
            let g = GlyphWithTransparency::from_char('.');
            [g, g]
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
    pub fn smoothed_player_pos_at_time(&self, s_from_start: f32) -> IPoint {
        todo!();
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
    let mut game = Game::new(25, 25);
    game.world_state.player_is_alive = true;
    game.world_state.portal_rendering = PortalRenderingOption::LineOfSight;
    game.world_state
        .place_portal(([10, 5], DIR_UP), ([15, 15], DIR_RIGHT));
    game.ui_handler.enable_mouse_smoothing = true;

    set_up_panic_hook();

    while game.world_state.running {
        let now = std::time::Instant::now();
        game.process_events_in_queue();

        let frame = game.render(game.ui_handler.mouse_world_point(game.ui_handler.instant_to_s_from_start(now)));
        game.ui_handler.screen_buffer.blit(&frame, [0, 0]);
        game.ui_handler.screen_buffer.draw_text(
            format!(
                "{:<30}",
                game.ui_handler.last_mouse_screen_row_col.to_debug()
            ),
            [(game.ui_handler.height() + 1).into(), 0],
        );
        for (i, event) in game.ui_handler.event_log.iter().enumerate() {
            game.ui_handler.screen_buffer.draw_text(
                format!("{:<30}", format!("{:?}", event)),
                [game.ui_handler.height() as usize + 3 + i, 0],
            );
        }
        game.ui_handler.draw_mouse();
        game.ui_handler.draw_screen();
        thread::sleep(Duration::from_millis(21));
    }
}

fn smoothed_mouse_position(recent_square_entries: &[(f32, IPoint)], now: f32) -> FPoint {
    assert!(recent_square_entries.len() > 0);
    if recent_square_entries.len() == 1 {
        return recent_square_entries[0].1.to_float();
    }

    // simple interpolation for now
    let l = recent_square_entries.len();
    let last = recent_square_entries[l - 1];
    let prev = recent_square_entries[l - 2];
    let dt = last.0 - prev.0;
    let step = last.1.sub(prev.1);
    let next = last.1.add(step);

    if now > last.0 + dt {
        return next.to_float();
    }

    last.1
        .to_float()
        .add(step.to_float().mul((now - last.0) / dt))
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
        let frame = state.render(None, 5.0);
        assert_eq!(frame.width(), 20);
        assert_eq!(frame.height(), 10);
    }

    fn press_left(col: u16, row: u16) -> termion::event::Event {
        termion::event::Event::Mouse(termion::event::MouseEvent::Press(
            termion::event::MouseButton::Left,
            col,
            row,
        ))
    }
    fn drag_mouse_to(col: u16, row: u16) -> termion::event::Event {
        termion::event::Event::Mouse(termion::event::MouseEvent::Hold(col, row))
    }
    fn release_mouse(col: u16, row: u16) -> termion::event::Event {
        termion::event::Event::Mouse(termion::event::MouseEvent::Release(col, row))
    }

    #[test]
    fn test_click_a() {
        let mut game = Game::new_headless_one_to_one_square(12);

        game.give_and_process_fake_event_now(press_left(1, 1));
        let frame = game.render_with_mouse(None);
        // let no_color = frame.uncolored_regular_string();
        dbg!(&frame);
        assert_frame_same_as_past!(frame, "a");
    }
    #[test]
    fn test_click_a_small() {
        let mut game = Game::new_headless_one_to_one_square(2);
        game.give_and_process_fake_event_now(press_left(1, 1));
        let frame = game.render_with_mouse(None);
        // dbg!(&frame);
        // dbg!(&frame.grid);
        assert_frame_same_as_past!(frame, "a", true);
    }
    #[test]
    fn test_click_b() {
        let mut game = Game::new_headless_one_to_one_square(12);
        game.give_and_process_fake_event_now(press_left(4, 10));
        let frame = game.render_with_mouse(None);
        dbg!(&frame);
        eprintln!("{}", frame.string_for_regular_display());
        assert_ne!(frame.get_xy([2, 2]).bg_color, RED.into());
        assert_eq!(frame.get_xy([3, 2]).bg_color, RED.into());
        assert_frame_same_as_past!(frame, "a");
    }
    #[test]
    fn test_drag_mouse() {
        let mut game = Game::new_headless(12, 24, 12, 12);
        game.give_and_process_fake_event_now(press_left(4, 4));
        let frame_1 = game.render_with_mouse(None);
        game.give_and_process_fake_event_now(drag_mouse_to(5, 4));
        let frame_2 = game.render_with_mouse(None);
        game.give_and_process_fake_event_now(drag_mouse_to(6, 4));
        let frame_3 = game.render_with_mouse(None);
        // dbg!(&frame_1, &frame_2, &frame_3);
        assert_frame_same_as_past!(frame_1, "1");
        assert_frame_same_as_past!(frame_2, "2");
        assert_frame_same_as_past!(frame_3, "3");
    }
    #[test]
    fn test_render_portal_edges() {
        let mut game = Game::new_headless_one_to_one_square(12);
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
        let frame = game.render(None);
        assert_frame_same_as_past!(frame, "a");
    }
    #[test]
    fn test_render_part_of_square() {
        let mut game = Game::new_headless_one_to_one_square(12);
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
        let mut game = Game::new_headless_one_to_one_square(12);
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
        let mut game = Game::new_headless_one_to_one_square(12);
        game.world_state.portal_rendering = PortalRenderingOption::LineOfSight;
        game.world_state.board_color_function = WorldState::radial_sin_board_colors;
        game.world_state
            .place_portal(([5, 7], DIR_UP), ([7, 10], DIR_UP));
        // game.portal_tint_function = GameState::rainbow_solid;
        // dbg!(game.render(None));
        game.world_state.portal_tint_function = WorldState::rainbow_tint;
        let (frame, layers) = game.render_now_with_debug(None);
        layers.into_iter().for_each(|frame| {
            dbg!(frame);
        });
        assert_frame_same_as_past!(frame, "a");
    }
    #[test]
    fn test_portal_with_rotation() {
        let mut game = Game::new_headless_one_to_one_square(12);
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
        let (frame, layers) = game.render_now_with_debug(None);
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
        let mut game = Game::new_headless_one_to_one_square(5);

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
        let mut game = Game::new_headless_one_to_one_square(2);
        game.ui_handler.enable_mouse_smoothing = true;
        game.give_and_process_fake_event_now(press_left(1, 1));
        let frame = game.render_with_mouse(None);
        println!("{}", &frame.escaped_regular_display_string());
        assert!(
            char_is_braille(frame.grid[0][0].character),
            "Char is not braille:\n\n{frame:?}"
        );
        assert_frame_same_as_past!(frame, "a", true);
    }
    #[test]
    fn test_render_smoothed_mouse_linear_move() {
        let mut game = Game::new_headless_one_to_one_square(3);
        game.ui_handler.enable_mouse_smoothing = true;
        game.ui_handler.give_fake_event((0.0, press_left(1, 1)));
        game.ui_handler.give_fake_event((0.2, drag_mouse_to(2, 1)));
        let n = game.process_events_in_queue();
        assert_eq!(n, 2);

        let frame = game.render_with_mouse_at_time(None, 0.4);
        assert!(char_is_braille(frame.grid[0][2].character), "{frame:?}");
        assert_frame_same_as_past!(frame, "a", true);
    }

    #[test]
    fn test_smooth_mouse() {
        let xy = smoothed_mouse_position(&[(0.1, [1, 1]), (0.2, [2, 1])], 0.3);
        assert!(xy.dist([3.0, 1.0]) < 0.1, "{xy:?}");
    }

    // #[ignore]
    #[test]
    fn test_smoothed_mouse_motion_accuracy() {
        let path_funcs: &[(&str, fn(f32) -> FPoint)] = &[
            ("horiz", |t: f32| [t * 5.0, 0.0]),
            ("horiz_fast", |t: f32| [t * 15.0, 0.0]),
            ("diag", |t: f32| [t * 15.0, t * 10.0]),
            ("sin", |t: f32| [t * 10.0, (t * 5.0).sin() * 3.0]),
            ("arc", |t: f32| [3.0 * (t).cos(), 3.0 * (t).sin()]),
        ];
        for (name, path_func) in path_funcs {
            let sim_path: Vec<(f32, FPoint)> = sim_mouse_path(path_func, 6.0, 3.0);
            let get_drawn_path = |path: &Vec<(f32, FPoint)>| {
                draw_points_in_character_grid(&path.iter().cloned().map(|(t, p)| p).collect_vec())
                    .framed()
            };

            let square_entry_events: Vec<(f32, IPoint)> = path_to_square_entry_events(&sim_path);
            assert_eq!(square_entry_events[0].0, sim_path[0].0);

            let naive_smoothing_function =
                |entry_events: &[(f32, IPoint)], t| entry_events.last().unwrap().1.to_float();

            let naive_smoothed_path =
                smoothed_mouse_path(&square_entry_events, 6.0, 3.0, naive_smoothing_function);
            let smoothed_path =
                smoothed_mouse_path(&square_entry_events, 6.0, 3.0, smoothed_mouse_position);

            println!(
                "{}",
                horiz_concat_strings(
                    &[
                        format!("Truth:\n{}", get_drawn_path(&sim_path)),
                        format!(
                            "Naive (rounded to last character):\n{}",
                            get_drawn_path(&naive_smoothed_path)
                        ),
                        format!("\"Smoothed\":\n{}", get_drawn_path(&smoothed_path))
                    ],
                    3
                )
            );

            assert_eq!(
                sim_path.len(),
                smoothed_path.len(),
                "sim_path:\n{:?}\n\nsmoothed_path:\n{:?}",
                &sim_path.iter().map(|(t, p)| t).collect_vec(),
                &smoothed_path.iter().map(|(t, p)| t).collect_vec()
            );

            let get_dists_and_avg_dist = |candidate_path: Vec<(f32, FPoint)>| {
                let dists = sim_path
                    .iter()
                    .zip(candidate_path.iter())
                    .map(|(&(t1, p1), &(t2, p2))| p1.dist(p2))
                    .collect_vec();

                let avg = &dists.iter().sum::<f32>() / dists.len() as f32;
                (dists, avg)
            };

            let (dists, avg_dist): (Vec<f32>, f32) = get_dists_and_avg_dist(smoothed_path);
            let (naive_dists, naive_avg_dist): (Vec<f32>, f32) =
                get_dists_and_avg_dist(naive_smoothed_path);

            assert_array_not_more_than_past!(&dists, name.to_string() + "_dists");

            let blessed_dists: Vec<f32> = get_past_array!(name.to_string() + "_dists");

            // TODO: less verbose elementwise operators
            let vs_naive = dists
                .iter()
                .zip(naive_dists.iter())
                .map(|(a, b)| a - b)
                .collect_vec();
            let vs_blessed = dists
                .iter()
                .zip(blessed_dists.iter())
                .map(|(a, b)| a - b)
                .collect_vec();

            println!("\nVs Naive:\n{}", signed_bargraph(&vs_naive, 5, None, None));
            println!(
                "\nVs Blessed:\n{}",
                signed_bargraph(&vs_blessed, 5, None, None)
            );

            let max_dist = *dists.iter().max_by_key(|&&x| OrderedFloat(x)).unwrap();
            let max_naive_dist = *naive_dists
                .iter()
                .max_by_key(|&&x| OrderedFloat(x))
                .unwrap();
            let max_any_dist = max_dist.max(max_naive_dist);

            let a = format!(
                "Dist error:\n{}\n\n\tAvg: {avg_dist}\n\n",
                bargraph(&dists, 5, Some(max_any_dist))
            )
            .indent();

            let b = format!(
                "Naive path dist error:\n{}\n\n\tAvg: {naive_avg_dist}\n\n",
                bargraph(&naive_dists, 5, Some(max_any_dist))
            )
            .indent();
            println!("{a}\n{b}");
            assert_value_not_more_than_past!(max_dist, name.to_string() + "_max_dist");
            assert_value_not_more_than_past!(avg_dist, name.to_string() + "_avg_dist");
        }
        // panic!();
    }
    #[test]
    fn test_player_step_through_portal() {
        let mut game = Game::new_headless_one_to_one_square(5);
        game.world_state
            .place_portal(([1, 2], DIR_RIGHT), ([3, 2], DIR_LEFT));
        game.world_state.player_square = [1, 2];

        game.try_move_player([1, 0]);
        assert_eq!(game.world_state.player_square, [3, 2]);
    }
    #[test]
    fn test_player_step_history_starts_empty() {
        let mut game = Game::new_headless_one_to_one_square(5);
        assert!(game.world_state.player_step_history.is_empty());
    }
    #[ignore]
    #[test]
    fn test_draw_smoothed_player_position() {
        let mut game = Game::new_headless_one_to_one_square(5);
        game.world_state.player_square = [0, 2];
        game.try_move_player_at_time(STEP_RIGHT, 0.5);
        game.try_move_player_at_time(STEP_RIGHT, 1.0);
        let frame = game.render_at_time(None, 1.5);
        let player_pos_in_frame = game
            .ui_handler
            .camera
            .absolute_world_square_to_left_frame_row_col(game.world_state.player_square);
        let [row, left_col] = player_pos_in_frame.map(|x| x as usize);
        let right_player_glyph: DrawableGlyph = frame.grid[row][left_col + 1];
        assert!(char_is_braille(right_player_glyph.character));
    }
}
