//#![allow(non_snake_case)]
#![feature(is_sorted)]
#![allow(warnings)]
mod glyph;
mod jumpproperties;
mod player;
mod utility;

extern crate geo;
extern crate line_drawing;
extern crate num;
extern crate std;
extern crate termion;
#[macro_use]
extern crate approx;

use ntest::timeout;
use player::Player;

// use assert2::{assert, check};
use enum_as_inner::EnumAsInner;
use geo::algorithm::euclidean_distance::EuclideanDistance;
use geo::Point;
use num::traits::FloatConst;
use std::char;
use std::cmp::{max, min};
use std::collections::hash_map::Entry;
use std::collections::{HashMap, VecDeque};
use std::fmt::Debug;
use std::io::{stdin, stdout, Write};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread;
use std::time::{Duration, Instant};
use num::Integer;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use termion::event::{Event, Key, MouseButton, MouseEvent};
use termion::input::{MouseTerminal, TermRead};
use termion::raw::{IntoRawMode, RawTerminal};

use crate::jumpproperties::JumpProperties;
use crate::BoostBehavior::AddButInstantTurnAround;
use glyph::*;
use utility::*;

//const DEFAULT_PARTICLE_DENSITY_FOR_AMALGAMATION: i32 = 6; // just more than a diagonal line


#[derive(PartialEq, Debug, Copy, Clone)]
struct StepFoe {
    square: IPoint,
}

struct Game {
    grid: Vec<Vec<i32>>, // (x,y), left to right, top to bottom
    output_buffer: Vec<Vec<Glyph>>, // (x,y), left to right, top to bottom
    output_on_screen: Vec<Vec<Glyph>>, // (x,y), left to right, top to bottom
    step_foes: Vec<StepFoe>,
    terminal_size: (u16, u16),  // (width, height)
    prev_mouse_pos: (i32, i32), // where mouse was last frame (if pressed)
    running: bool,              // set false to quit
}

impl Game {
    fn new(width: u16, height: u16) -> Game {
        Game {
            grid: vec![vec![Block::Air; height as usize]; width as usize],
            output_buffer: vec![vec![Glyph::from_char(' '); height as usize]; width as usize],
            output_on_screen: vec![vec![Glyph::from_char('x'); height as usize]; width as usize],
            step_foes: Vec::<StepFoe>::new(),
            terminal_size: (width, height),
            prev_mouse_pos: (1, 1),
            running: true,
        }
    }


    fn screen_to_world(&self, terminal_position: &(u16, u16)) -> (i32, i32) {
        // terminal indexes from 1, and the y axis goes top to bottom
        (
            terminal_position.0 as i32 - 1,
            self.terminal_size.1 as i32 - terminal_position.1 as i32,
        )
    }

    fn world_to_screen(&self, world_position: &(i32, i32)) -> (u16, u16) {
        // terminal indexes from 1, and the y axis goes top to bottom
        // world indexes from 0, origin at bottom left
        (
            world_position.0 as u16 + 1,
            (self.terminal_size.1 as i32 - world_position.1) as u16,
        )
    }
    fn get_block(&self, square: Point<i32>) -> Block {
        if !self.square_is_in_world(square) {
            panic!("square {:?} is not in world", square);
        }
        return self.grid[square.x() as usize][square.y() as usize];
    }
    fn try_get_block(&self, square: Point<i32>) -> Option<Block> {
        return if self.square_is_in_world(square) {
            Some(self.grid[square.x() as usize][square.y() as usize])
        } else {
            None
        };
    }
    fn set_block(&mut self, pos: Point<i32>, block: Block) {
        self.grid[pos.x() as usize][pos.y() as usize] = block;
    }

    fn place_line_of_blocks(&mut self, pos0: (i32, i32), pos1: (i32, i32), block: Block) {
        for (x1, y1) in line_drawing::Bresenham::new(pos0, pos1) {
            self.grid[x1 as usize][y1 as usize] = block;
        }
    }

    fn place_boundary_wall(&mut self) {
        let xmax = self.grid.len() as i32 - 1;
        let ymax = self.grid[0].len() as i32 - 1;
        let xmin = 0;
        let ymin = 0;
        self.place_line_of_blocks((xmin, ymin), (xmax, ymin), Block::Wall);
        self.place_line_of_blocks((xmax, ymin), (xmax, ymax), Block::Wall);
        self.place_line_of_blocks((xmax, ymax), (xmin, ymax), Block::Wall);
        self.place_line_of_blocks((xmin, ymax), (xmin, ymin), Block::Wall);
    }

    fn place_wall_rect(&mut self, corner1: IPoint, corner2: IPoint) {
        let xmin = min(corner1.x(), corner2.x());
        let xmax = max(corner1.x(), corner2.x());
        let ymin = min(corner1.y(), corner2.y());
        let ymax = max(corner1.y(), corner2.y());

        for y in ymin..=ymax {
            self.place_line_of_blocks((xmin, y), (xmax, y), Block::Wall);
        }
    }

    fn place_block(&mut self, pos: Point<i32>, block: Block) {
        if !self.square_is_in_world(pos) {
            println!("tried placing block out of world: {:?}", pos);
            return;
        }
        self.grid[pos.x() as usize][pos.y() as usize] = block;
    }

    fn place_wall_block(&mut self, pos: Point<i32>) {
        self.place_block(pos, Block::Wall);
    }


    fn braille_bresenham_line_points(
        start_pos: Point<f32>,
        end_pos: Point<f32>,
    ) -> Vec<Point<f32>> {
        let braille_pos0 = Glyph::world_pos_to_braille_pos(start_pos);
        let braille_pos1 = Glyph::world_pos_to_braille_pos(end_pos);

        line_drawing::Bresenham::new(
            snap_to_grid(braille_pos0).x_y(),
            snap_to_grid(braille_pos1).x_y(),
        )
        .map(|(x, y)| Glyph::braille_pos_to_world_pos(p(x as f32, y as f32)))
        .collect()
    }

    fn count_braille_dots_in_square(&self, square: Point<i32>) -> i32 {
        return if self.square_is_in_world(square) {
            Glyph::count_braille_dots(self.get_buffered_glyph(square).character)
        } else {
            0
        };
    }

    fn draw_visual_braille_point(&mut self, pos: Point<f32>, color: ColorName) {
        self.draw_visual_braille_line(pos, pos, color);
    }

    fn draw_visual_braille_line(
        &mut self,
        start_pos: Point<f32>,
        end_pos: Point<f32>,
        color: ColorName,
    ) {
        let squares_to_place =
            Glyph::get_glyphs_for_colored_braille_line(start_pos, end_pos, color);

        let start_grid_square = snap_to_grid(start_pos);
        let end_grid_square = snap_to_grid(end_pos);
        let bottom_square_y = min(start_grid_square.y(), end_grid_square.y());
        let left_square_x = min(start_grid_square.x(), end_grid_square.x());

        for i in 0..squares_to_place.len() {
            for j in 0..squares_to_place[0].len() {
                if let Some(new_glyph) = squares_to_place[i][j] {
                    let grid_square = p(left_square_x + i as i32, bottom_square_y + j as i32);
                    if !self.square_is_in_world(grid_square) {
                        continue;
                    }
                    let grid_glyph =
                        &mut self.output_buffer[grid_square.x() as usize][grid_square.y() as usize];
                    if Glyph::is_braille(grid_glyph.character) {
                        let combined_braille =
                            Glyph::add_braille(grid_glyph.character, new_glyph.character);
                        *grid_glyph = new_glyph;
                        grid_glyph.character = combined_braille;
                    } else {
                        *grid_glyph = new_glyph;
                    }
                }
            }
        }
    }

    fn clear(&mut self) {
        let (width, height) = termion::terminal_size().unwrap();
        self.grid = vec![vec![Block::Air; height as usize]; width as usize];
    }


    fn place_step_foe(&mut self, square: IPoint) {
        if !self.square_is_empty(square) {
            panic!("Tried to place step foe in occupied square: {:?}", square);
        }
        let mut step_foe = StepFoe { square };
        self.step_foes.push(step_foe);
        self.set_block(square, Block::StepFoe);
    }

    fn try_get_player_square_adjacency(&self) -> Option<LocalBlockOccupancy> {
        self.get_square_adjacency(self.player_square())
    }



    fn handle_event(&mut self, evt: Event) {
        match evt {
            Event::Key(ke) => match ke {
                Key::Char('q') => self.running = false,
                Key::Char('1') => self.selected_block = Block::Air,
                Key::Char('2') => self.selected_block = Block::Wall,
                Key::Char('3') => self.selected_block = Block::Brick,
                Key::Char('c') => self.particles.clear(),
                Key::Char('k') => self.kill_player(),
                Key::Char('r') => self.place_player(
                    self.terminal_size.0 as f32 / 2.0,
                    self.terminal_size.1 as f32 / 2.0,
                ),
                Key::Char(' ') => self.player_jump_if_possible(),
                Key::Char('f') => self.player_dash(),
                Key::Char('g') => self.toggle_bullet_time(),
                Key::Char('w') | Key::Up => self.player.desired_direction = up_i(),
                Key::Char('a') | Key::Left => self.player.desired_direction = left_i(),
                Key::Char('s') | Key::Down => self.player.desired_direction = down_i(),
                Key::Char('d') | Key::Right => self.player.desired_direction = right_i(),
                _ => {}
            },
            Event::Mouse(me) => match me {
                MouseEvent::Press(MouseButton::Left, term_x, term_y) => {
                    let (x, y) = self.screen_to_world(&(term_x, term_y));
                    self.place_block(p(x, y), self.selected_block);
                    self.prev_mouse_pos = (x, y);
                }
                MouseEvent::Press(MouseButton::Right, term_x, term_y) => {
                    let (x, y) = self.screen_to_world(&(term_x, term_y));
                    self.place_player(x as f32, y as f32);
                }
                MouseEvent::Hold(term_x, term_y) => {
                    let (x, y) = self.screen_to_world(&(term_x, term_y));
                    self.place_line_of_blocks(self.prev_mouse_pos, (x, y), self.selected_block);
                    self.prev_mouse_pos = (x, y);
                }
                _ => {}
            },
            _ => {}
        }
    }

    fn get_player_color(&self) -> ColorName {
        if self.player_is_in_boost() {
            PLAYER_HIGH_SPEED_COLOR
        } else {
            PLAYER_COLOR
        }
    }

    fn update_output_buffer(&mut self) {
        self.fill_output_buffer_with_black();
        self.draw_particles();
        self.draw_turrets();
        self.draw_non_air_blocks();

        if self.player.alive {
            self.draw_player();
        }
    }

    fn width(&self) -> usize {
        self.grid.len()
    }
    fn height(&self) -> usize {
        self.grid[0].len()
    }

    fn mid_square(&self) -> IPoint {
        p(self.width() as i32 / 2, self.height() as i32 / 2)
    }
    fn x_max(&self) -> i32 {
        self.width() as i32 - 1
    }
    fn y_max(&self) -> i32 {
        self.height() as i32 - 1
    }

    fn fill_output_buffer_with_black(&mut self) {
        let width = self.grid.len();
        let height = self.grid[0].len();
        for x in 0..width {
            for y in 0..height {
                self.output_buffer[x][y] = Glyph::from_char(' ');
            }
        }
    }


    fn get_buffered_glyph(&self, pos: Point<i32>) -> &Glyph {
        return &self.output_buffer[pos.x() as usize][pos.y() as usize];
    }
    fn set_buffered_glyph(&mut self, pos: Point<i32>, new_glyph: Glyph) {
        self.output_buffer[pos.x() as usize][pos.y() as usize] = new_glyph;
    }
    #[allow(dead_code)]
    fn get_glyph_on_screen(&self, pos: Point<i32>) -> &Glyph {
        return &self.output_on_screen[pos.x() as usize][pos.y() as usize];
    }

    fn print_output_buffer(&self) {
        for y in 0..self.height() {
            let reverse_y = self.height() - 1 - y;
            let mut row_string = String::new();
            for x in 0..self.width() {
                row_string += &self.output_buffer[x][reverse_y].to_string();
            }
            row_string += &Glyph::reset_colors();
            if reverse_y % 5 == 0 {
                row_string += &format!("-- {}", reverse_y);
            }
            println!("{}", row_string);
        }
    }

    fn update_screen(&mut self, stdout: &mut MouseTerminal<RawTerminal<std::io::Stdout>>) {
        let width = self.grid.len();
        let height = self.grid[0].len();
        // Now update the graphics where applicable
        for x in 0..width {
            for y in 0..height {
                if self.output_buffer[x][y] != self.output_on_screen[x][y] {
                    let (term_x, term_y) = self.world_to_screen(&(x as i32, y as i32));
                    write!(stdout, "{}", termion::cursor::Goto(term_x, term_y)).unwrap();
                    write!(stdout, "{}", self.output_buffer[x][y].to_string()).unwrap();
                }
            }
        }
        write!(
            stdout,
            "{}{}",
            termion::cursor::Goto(1, 1),
            self.fps() as i32
        )
        .unwrap();
        stdout.flush().unwrap();
        self.output_on_screen = self.output_buffer.clone();
    }

    fn fps(&self) -> f32 {
        if self.recent_tick_durations_s.is_empty() {
            return 0.0;
        }
        let avg_s_per_frame = self.recent_tick_durations_s.iter().sum::<f32>()
            / self.recent_tick_durations_s.len() as f32;
        let fps = 1.0 / avg_s_per_frame;
        if fps > 100.0 {
            0.0
        } else {
            fps
        }
    }

    fn apply_gravity_to_blocks(&mut self) {
        // We want to count from bottom to top, because things fall down
        for x in 0..self.terminal_size.0 as usize {
            for y in 0..self.terminal_size.1 as usize {
                let block = self.grid[x][y];
                if block.subject_to_block_gravity() {
                    let is_bottom_row = y == 0;
                    let has_direct_support = !is_bottom_row && self.grid[x][y - 1] != Block::Air;
                    if is_bottom_row {
                        self.grid[x][y] = Block::Air;
                    } else if !has_direct_support {
                        self.grid[x][y - 1] = block;
                        self.grid[x][y] = Block::Air;
                    }
                }
            }
        }
    }


    fn get_square_adjacency(&self, square: IPoint) -> Option<LocalBlockOccupancy> {
        let mut local_block_occupancy = empty_local_block_occupancy();
        for rel_square in get_3x3_squares() {
            let abs_square = rel_square + square;
            let abs_square_blocks_player = if let Some(block) = self.try_get_block(abs_square) {
                block.can_collide_with_player()
            } else {
                return None;
            };
            let index_2d = rel_square + p(1, 1);
            local_block_occupancy[index_2d.x() as usize][index_2d.y() as usize] =
                abs_square_blocks_player;
        }
        Some(local_block_occupancy)
    }

    // Where the player can move to in a line
    // tries to draw a line in air
    // returns None if out of bounds
    // returns the start position if start is not Block::Air
    fn unit_squarecast(&self, start_pos: Point<f32>, end_pos: Point<f32>) -> SquarecastResult {
        self.squarecast_for_player_collision(start_pos, end_pos, 1.0)
    }

    fn linecast_laser(&self, start_pos: Point<f32>, end_pos: Point<f32>) -> SquarecastResult {
        let filter = Box::new(|block: Block| block.can_be_hit_by_laser());
        first_hit(vec![
            self.linecast_with_block_filter(start_pos, end_pos, filter),
            self.linecast_particles_only(start_pos, end_pos),
        ])
    }
    fn linecast_walls_only(&self, start_pos: Point<f32>, end_pos: Point<f32>) -> SquarecastResult {
        self.squarecast_one_block_type(start_pos, end_pos, 0.0, Block::Wall)
    }
    fn linecast_particles_only(&self, start_pos: FPoint, end_pos: FPoint) -> SquarecastResult {
        let LINECAST_PARTICLE_DETECTION_RADIUS = 0.2;
        // todo: maybe precalculate the location map?
        self.squarecast_particles_only(
            start_pos,
            end_pos,
            LINECAST_PARTICLE_DETECTION_RADIUS * 2.0,
            Some(self.get_particle_location_map()),
        )
    }

    fn squarecast_for_player_collision(
        &self,
        start_pos: Point<f32>,
        end_pos: Point<f32>,
        moving_square_side_length: f32,
    ) -> SquarecastResult {
        self.squarecast_with_block_filter(
            start_pos,
            end_pos,
            moving_square_side_length,
            Box::new(|block| block.can_collide_with_player()),
        )
    }

    fn linecast_with_block_filter(
        &self,
        start_pos: Point<f32>,
        end_pos: Point<f32>,
        block_filter: BlockFilter,
    ) -> SquarecastResult {
        self.squarecast_with_block_filter(start_pos, end_pos, 0.0, block_filter)
    }

    fn squarecast_with_block_filter(
        &self,
        start_pos: Point<f32>,
        end_pos: Point<f32>,
        moving_square_side_length: f32,
        block_filter: BlockFilter,
    ) -> SquarecastResult {
        self.squarecast(
            start_pos,
            end_pos,
            moving_square_side_length,
            block_filter,
            None,
        )
    }



    fn square_is_in_world(&self, pos: Point<i32>) -> bool {
        pos.x() >= 0
            && pos.x() < self.terminal_size.0 as i32
            && pos.y() >= 0
            && pos.y() < self.terminal_size.1 as i32
    }
    fn square_is_empty(&mut self, square: IPoint) -> bool {
        self.square_is_in_world(square) && matches!(self.get_block(square), Block::Air)
    }

}

fn init_platformer_test_world(width: u16, height: u16) -> Game {
    let mut game = Game::new(width, height);
    game.place_line_of_blocks((2, 3), (8, 3), Block::Wall);
    game.place_player(5.0, 5.0);

    game
}

fn set_up_panic_hook() {
    std::panic::set_hook(Box::new(move |panic_info| {
        write!(stdout(), "{}", termion::screen::ToMainScreen);
        write!(stdout(), "{:?}", panic_info);
    }));
}

fn set_up_input_thread() -> Receiver<Event> {
    let (tx, rx) = channel();
    thread::spawn(move || {
        for c in stdin().events() {
            let evt = c.unwrap();
            tx.send(evt).unwrap();
        }
    });
    rx
}

fn main() {
    let (width, height) = termion::terminal_size().unwrap();
    let mut game = init_test_world_1(width, height);
    //let mut game = init_platformer_test_world(width, height);

    let mut terminal = termion::screen::AlternateScreen::from(termion::cursor::HideCursor::from(
        MouseTerminal::from(stdout().into_raw_mode().unwrap()),
    ));

    set_up_panic_hook();

    // Separate thread for reading input
    let event_receiver = set_up_input_thread();

    let mut prev_start_time = Instant::now();
    while game.running {
        let start_time = Instant::now();
        let prev_tick_duration_ms = start_time.duration_since(prev_start_time).as_millis();
        let prev_tick_duration_s: f32 = prev_tick_duration_ms as f32 / 1000.0;
        prev_start_time = start_time;

        game.recent_tick_durations_s
            .push_front(prev_tick_duration_s);
        if game.recent_tick_durations_s.len() > 10 {
            game.recent_tick_durations_s.pop_back();
        }

        while let Ok(event) = event_receiver.try_recv() {
            game.handle_event(event);
        }
        game.tick_physics();
        game.update_output_buffer();
        game.update_screen(&mut terminal);
        let tick_duration_so_far_ms = start_time.elapsed().as_millis();
        if tick_duration_so_far_ms < IDEAL_FRAME_DURATION_MS {
            thread::sleep(Duration::from_millis(
                (IDEAL_FRAME_DURATION_MS - tick_duration_so_far_ms) as u64,
            ));
        }
    }
}
