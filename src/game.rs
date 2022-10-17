use std::cmp::{max, min};
use std::collections::{HashMap, HashSet};
use std::io::Write;
use std::time::Duration;

use euclid::*;
use line_drawing::Point;

use crate::graphics::Graphics;
use crate::piece::{Piece, PieceType};
use crate::{
    point_to_string, round_to_king_step, Glyph, IPoint, IVector, SquareGridInWorldFrame,
    SquareList, Step, WorldSquare, LEFT_I,
};

pub struct Game {
    board_width: usize,
    board_height: usize,
    // (x,y), left to right, top to bottom
    //step_foes: Vec<StepFoe>,
    pub(crate) running: bool,
    // set false to quit
    player_position: WorldSquare,
    player_faced_direction: Step,
    player_is_dead: bool,
    graphics: Graphics,
    pieces: HashMap<WorldSquare, Piece>,
}

impl Game {
    pub fn new(terminal_width: u16, terminal_height: u16) -> Game {
        let board_width: usize = (terminal_width / 2) as usize;
        let board_height: usize = terminal_height as usize;
        Game {
            board_width,
            board_height,
            running: true,
            player_position: point2((board_width / 2) as i32, (board_height / 2) as i32),
            player_faced_direction: LEFT_I.cast_unit(),
            player_is_dead: false,
            graphics: Graphics::new(terminal_width, terminal_height),
            pieces: HashMap::new(),
        }
    }

    pub fn board_width(&self) -> usize {
        self.board_width
    }
    pub fn board_height(&self) -> usize {
        self.board_height
    }
    pub fn player_is_dead(&self) -> bool {
        self.player_is_dead
    }

    fn mid_square(&self) -> IPoint {
        point2(
            self.board_width() as i32 / 2,
            self.board_height() as i32 / 2,
        )
    }
    fn x_max(&self) -> i32 {
        self.board_width() as i32 - 1
    }
    fn y_max(&self) -> i32 {
        self.board_height() as i32 - 1
    }

    fn square_is_on_board(&self, pos: WorldSquare) -> bool {
        pos.x >= 0
            && pos.x < self.board_width() as i32
            && pos.y >= 0
            && pos.y < self.board_height() as i32
    }

    pub fn quit(&mut self) {
        self.running = false;
    }

    pub fn move_player(&mut self, movement: Step) -> Result<(), ()> {
        let new_pos = self.player_position + movement;
        self.set_player_faced_direction(round_to_king_step(movement));
        self.set_player_position(new_pos)
    }

    pub fn player_position(&self) -> WorldSquare {
        return self.player_position.clone();
    }

    pub fn set_player_position(&mut self, pos: WorldSquare) -> Result<(), ()> {
        if self.is_piece_at(pos) {
            self.capture_piece_at(pos);
        }

        if self.square_is_on_board(pos) {
            self.player_position = pos.clone();
        } else {
            return Err(());
        }

        return Ok(());
    }

    pub fn player_faced_direction(&self) -> Step {
        self.player_faced_direction
    }
    pub fn set_player_faced_direction(&mut self, new_dir: Step) {
        self.player_faced_direction = new_dir.clone()
    }

    pub fn borrow_graphics_mut(&mut self) -> &mut Graphics {
        return &mut self.graphics;
    }

    pub fn draw_headless(&mut self, delta: Duration) {
        self.draw(&mut None, delta)
    }

    pub fn draw(&mut self, mut writer: &mut Option<Box<dyn Write>>, delta: Duration) {
        self.graphics
            .draw_empty_board(self.board_width, self.board_height);
        for (&square, &piece) in &self.pieces {
            self.graphics.draw_piece(piece, square);
        }
        self.graphics.draw_all_lasers(delta);
        self.graphics
            .draw_player(self.player_position(), self.player_faced_direction());
        self.graphics.display(&mut writer);
    }

    fn square_is_empty(&self, pos: WorldSquare) -> bool {
        (self.player_position != pos || self.player_is_dead) && !self.is_piece_at(pos)
    }

    pub fn place_piece(&mut self, piece: Piece, square: WorldSquare) -> Result<(), ()> {
        if !self.square_is_empty(square) || !self.square_is_on_board(square) {
            return Err(());
        }
        self.pieces.insert(square, piece);
        Ok(())
    }
    pub fn get_piece_at(&self, square: WorldSquare) -> Option<&Piece> {
        self.pieces.get(&square)
    }

    pub fn is_piece_at(&self, square: WorldSquare) -> bool {
        self.get_piece_at(square).is_some()
    }

    pub fn piece_type_count(&self, piece_type: PieceType) -> u32 {
        self.pieces
            .values()
            .filter(|&&piece| piece.piece_type == piece_type)
            .count() as u32
    }

    pub fn move_all_pieces(&mut self) {
        let mut moved_piece_locations = HashSet::<WorldSquare>::new();
        let piece_start_locations: SquareList = self.pieces.keys().cloned().collect();
        for piece_square in piece_start_locations {
            // already moved this one
            if moved_piece_locations.contains(&piece_square) {
                continue;
            }
            if let Some(end_pos) = self.move_piece_at(piece_square) {
                moved_piece_locations.insert(end_pos);
            }
        }
    }

    fn move_piece(&mut self, start: WorldSquare, end: WorldSquare) {
        if !self.is_piece_at(start) {
            panic!("No piece at {}", point_to_string(start));
        }
        if !self.square_is_empty(end) {
            panic!("Square is occupied: {}", point_to_string(end));
        }

        let piece = self.pieces.remove(&start).unwrap();
        self.pieces.insert(end, piece);
    }

    // returns where the piece moves to, if applicable
    pub fn move_piece_at(&mut self, pos: WorldSquare) -> Option<WorldSquare> {
        if !self.is_piece_at(pos) {
            return None;
        }

        let piece = self.get_piece_at(pos).unwrap().clone();

        // want to capture the player
        let capture_option: Option<WorldSquare> = piece
            .get_relative_capture_steps()
            .into_iter()
            .map(|step| pos + step)
            .find(|&square| square == self.player_position);

        if let Some(square) = capture_option {
            self.player_is_dead = true;
            self.move_piece(pos, square);
            return Some(square);
        }

        // want to move closer to player
        let move_option: Option<WorldSquare> = piece
            .relative_move_steps()
            .into_iter()
            .map(|step| pos + step)
            .filter(|&square| self.square_is_empty(square) && self.square_is_on_board(square))
            .min_by_key(|&square| (square - self.player_position).square_length());

        if let Some(square) = move_option {
            self.move_piece(pos, square);
            return Some(square);
        }

        None
    }

    pub fn player_shoot(&mut self) {
        let range = 5;
        let line_start = self.player_position();
        let line_end = line_start + self.player_faced_direction() * range;

        for (x, y) in line_drawing::Bresenham::new(line_start.to_tuple(), line_end.to_tuple()) {
            let square = WorldSquare::new(x, y);
            self.capture_piece_at(square).ok();
        }

        self.graphics
            .add_laser(line_start.to_f32(), line_end.to_f32());
    }

    pub fn capture_piece_at(&mut self, square: WorldSquare) -> Result<(), ()> {
        if !self.square_is_on_board(square) || !self.is_piece_at(square) {
            return Err(());
        }
        self.pieces.remove(&square);
        Ok(())
    }
}
