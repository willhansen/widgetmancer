use std::cmp::{max, min};
use std::io::Write;
use geo::Point;
use termion::input::MouseTerminal;
use termion::raw::RawTerminal;
use crate::{ColorName, Glyph, IPoint};

pub struct Game {
    grid: Vec<Vec<i32>>, // (x,y), left to right, top to bottom
    //step_foes: Vec<StepFoe>,
    prev_mouse_pos: (i32, i32), // where mouse was last frame (if pressed)
    running: bool,              // set false to quit
    player_position: IPoint,
}

impl Game {
    fn new(width: u16, height: u16) -> Game {
        Game {
            grid: vec![vec![0; height as usize]; width as usize],
            //step_foes: Vec::<StepFoe>::new(),
            prev_mouse_pos: (1, 1),
            running: true,
            player_position: p(5,5),
        }
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

    pub(crate) fn place_line_of_blocks(&mut self, pos0: (i32, i32), pos1: (i32, i32), block: Block) {
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

    fn try_get_player_square_adjacency(&self) -> Option<LocalBlockOccupancy> {
        self.get_square_adjacency(self.player_square())
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

