use std::cmp::{max, min};
use euclid::*;
use crate::{ColorName, Glyph, IPoint};

enum Pieces {
    Player,
    Pawn,
}

pub struct Game {
    grid: Vec<Vec<i32>>, // (x,y), left to right, top to bottom
    //step_foes: Vec<StepFoe>,
    running: bool,              // set false to quit
    player_position: IPoint,
}

impl Game {
    pub fn new(width: u16, height: u16) -> Game {
        Game {
            grid: vec![vec![0; height as usize]; width as usize],
            //step_foes: Vec::<StepFoe>::new(),
            running: true,
            player_position: p(5,5),
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

    fn square_is_in_world(&self, pos: Point2D<i32>) -> bool {
        pos.x() >= 0
            && pos.x() < self.terminal_size.0 as i32
            && pos.y() >= 0
            && pos.y() < self.terminal_size.1 as i32
    }
    fn square_is_empty(&mut self, square: IPoint) -> bool {
        self.square_is_in_world(square) && matches!(self.get_block(square), Block::Air)
    }

}

