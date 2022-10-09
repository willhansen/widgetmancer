use std::cmp::{max, min};
use std::io::Write;
use euclid::*;
use crate::{ColorName, Glyph, IPoint, IVector};
use crate::graphics::Graphics;

enum Pieces {
    Player,
    Pawn,
}

pub struct Game {
    grid: Vec<Vec<i32>>,
    // (x,y), left to right, top to bottom
    //step_foes: Vec<StepFoe>,
    pub(crate) running: bool,
    // set false to quit
    player_position: IPoint,
    graphics: Graphics,
}

impl Game {
    pub fn new(width: u16, height: u16) -> Game {
        Game {
            grid: vec![vec![0; height as usize]; width as usize],
            //step_foes: Vec::<StepFoe>::new(),
            running: true,
            player_position: point2((width / 2) as i32, (height / 2) as i32),
            graphics: Graphics::new(width, height),
        }
    }

    fn width(&self) -> usize {
        self.grid.len()
    }
    fn height(&self) -> usize {
        self.grid[0].len()
    }

    fn mid_square(&self) -> IPoint {
        point2(self.width() as i32 / 2, self.height() as i32 / 2)
    }
    fn x_max(&self) -> i32 {
        self.width() as i32 - 1
    }
    fn y_max(&self) -> i32 {
        self.height() as i32 - 1
    }

    fn square_is_in_world(&self, pos: &IPoint) -> bool {
        pos.x >= 0
            && pos.x < self.width() as i32
            && pos.y >= 0
            && pos.y < self.height() as i32
    }

    pub fn quit(&mut self) {
        self.running = false;
    }

    pub fn move_player(&mut self, movement: IVector) -> Result<(), ()>{
        let new_pos = self.player_position + movement;
        self.set_player_position(&new_pos)
    }
    pub fn get_player_position(&self) -> IPoint {
        return self.player_position.clone();
    }
    pub fn set_player_position(&mut self, pos: &IPoint) -> Result<(), ()> {
        if self.square_is_in_world(pos) {
            self.player_position = pos.clone();
            Ok(())
        } else {
            Err(())
        }
    }

    pub fn borrow_graphics_mut(&mut self) -> &mut Graphics {
        return &mut self.graphics;
    }

    pub fn draw_headless(&mut self) {
        self.draw(&mut None)
    }

    pub fn draw(&mut self, mut writer: &mut Option<Box<dyn Write>>) {
        self.graphics.fill_output_buffer_with_black();
        self.graphics.draw_player(self.player_position);
        self.graphics.display(&mut writer);

    }
}

