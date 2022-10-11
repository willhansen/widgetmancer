use std::cmp::{max, min};
use std::collections::HashMap;
use std::io::Write;

use euclid::*;

use crate::{ColorName, Glyph, IPoint, IVector, point_to_string, WorldSpace};
use crate::graphics::Graphics;
use crate::piece::{Piece, PieceType};

pub struct Game {
    board_width: usize,
    board_height: usize,
    // (x,y), left to right, top to bottom
    //step_foes: Vec<StepFoe>,
    pub(crate) running: bool,
    // set false to quit
    player_position: Point2D<i32, WorldSpace>,
    graphics: Graphics,
    pieces: HashMap<Point2D<i32, WorldSpace>, PieceType>,
}

impl Game {
    pub fn new(terminal_width: u16, terminal_height: u16) -> Game {
        let board_width: usize = (terminal_width / 2) as usize;
        let board_height: usize = terminal_height as usize;
        Game {
            board_width,
            board_height,
            //step_foes: Vec::<StepFoe>::new(),
            running: true,
            player_position: point2((board_width / 2) as i32, (board_height / 2) as i32),
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

    fn mid_square(&self) -> IPoint {
        point2(self.board_width() as i32 / 2, self.board_height() as i32 / 2)
    }
    fn x_max(&self) -> i32 {
        self.board_width() as i32 - 1
    }
    fn y_max(&self) -> i32 {
        self.board_height() as i32 - 1
    }

    fn square_is_on_board(&self, pos: &Point2D<i32, WorldSpace>) -> bool {
        pos.x >= 0
            && pos.x < self.board_width() as i32
            && pos.y >= 0
            && pos.y < self.board_height() as i32
    }

    pub fn quit(&mut self) {
        self.running = false;
    }

    pub fn move_player(&mut self, movement: Vector2D<i32, WorldSpace>) -> Result<(), ()> {
        let new_pos = self.player_position + movement;
        self.set_player_position(&new_pos)
    }
    pub fn get_player_position(&self) -> Point2D<i32, WorldSpace> {
        return self.player_position.clone();
    }
    pub fn set_player_position(&mut self, pos: &Point2D<i32, WorldSpace>) -> Result<(), ()> {

        if self.pieces.contains_key(pos) {
            self.pieces.remove(pos);
        }

        if self.square_is_on_board(pos) {
            self.player_position = pos.clone();
        } else {
            return Err(());
        }

        return Ok(())
    }

    pub fn borrow_graphics_mut(&mut self) -> &mut Graphics {
        return &mut self.graphics;
    }

    pub fn draw_headless(&mut self) {
        self.draw(&mut None)
    }

    pub fn draw(&mut self, mut writer: &mut Option<Box<dyn Write>>) {
        self.graphics.fill_output_buffer_with_checker();
        self.graphics.draw_player(&self.player_position);
        for (pos, piece) in &self.pieces {
            if !self.square_is_on_board(&pos) {
                panic!("Found piece out of bounds: {}", point_to_string(&pos));
            }
            self.graphics.draw_piece(piece, pos);
        }
        self.graphics.display(&mut writer);
    }

    fn square_is_empty(&self, pos: &Point2D<i32, WorldSpace>) -> bool {
        self.player_position != *pos && !self.pieces.contains_key(pos)
    }

    pub fn place_piece(&mut self, piece_type: PieceType, pos: &Point2D<i32, WorldSpace>) -> Result<(), ()> {
        if !self.square_is_empty(pos) || !self.square_is_on_board(pos) {
            return Err(());
        }
        self.pieces.insert(*pos, piece_type);
        Ok(())
    }

    pub fn piece_count(&self, piece_type: PieceType) -> u32 {
        self.pieces.values().filter(|&&found_type| found_type == piece_type).count() as u32
    }
}

