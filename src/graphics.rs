use std::cmp::min;
use std::io::Write;
use euclid::default::Point2D;
use euclid::point2 as p;
use termion::input::MouseTerminal;
use termion::raw::RawTerminal;
use termion::terminal_size;
use crate::{ColorName, Game, Glyph, IPoint};


pub struct Graphics {
    output_buffer: Vec<Vec<Glyph>>, // (x,y), left to right, top to bottom
    output_on_screen: Vec<Vec<Glyph>>, // (x,y), left to right, top to bottom
    terminal_size: (u16, u16),  // (width, height)
}

impl Graphics {

    fn new(width: u16, height: u16) -> Graphics {
        Graphics {
            output_buffer: vec![vec![Glyph::from_char(' '); height as usize]; width as usize],
            output_on_screen: vec![vec![Glyph::from_char('x'); height as usize]; width as usize],
            terminal_size: (width, height),
        }
    }

    fn width(&self) -> i32 {
        self.terminal_size.0 as i32
    }

    fn height(&self) -> i32 {
        self.terminal_size.1 as i32
    }

    fn square_is_on_screen(&self, square: IPoint) -> bool {
        return square.x >= 0 && square.x < self.terminal_size.0 as i32 && square.y >= 0 && square.y < self.terminal_size.1 as i32
    }

    fn world_to_screen(&self, world_position: &(i32, i32)) -> (u16, u16) {
        // terminal indexes from 1, and the y axis goes top to bottom
        // world indexes from 0, origin at bottom left
        (
            world_position.0 as u16 + 1,
            (self.terminal_size.1 as i32 - world_position.1) as u16,
        )
    }

    fn braille_bresenham_line_points(
        start_pos: Point2D<f32>,
        end_pos: Point2D<f32>,
    ) -> Vec<Point2D<f32>> {
        let braille_pos0 = Glyph::world_pos_to_braille_pos(start_pos).round().to_i32();
        let braille_pos1 = Glyph::world_pos_to_braille_pos(end_pos).round().to_i32();

        line_drawing::Bresenham::new(
            braille_pos0.to_tuple(),
            braille_pos1.to_tuple(),
        )
            .map(|(x, y)| Glyph::braille_pos_to_world_pos(p(x as f32, y as f32)))
            .collect()
    }

    fn count_braille_dots_in_square(&self, square: Point2D<i32>) -> i32 {
        return if self.square_is_on_screen(square) {
            Glyph::count_braille_dots(self.get_buffered_glyph(square).character)
        } else {
            0
        };
    }

    fn draw_visual_braille_point(&mut self, pos: Point2D<f32>, color: ColorName) {
        self.draw_visual_braille_line(pos, pos, color);
    }

    fn draw_visual_braille_line(
        &mut self,
        start_pos: Point2D<f32>,
        end_pos: Point2D<f32>,
        color: ColorName,
    ) {
        let squares_to_place =
            Glyph::get_glyphs_for_colored_braille_line(start_pos, end_pos, color);

        let start_grid_square: IPoint = start_pos.round().to_i32();
        let end_grid_square: IPoint = end_pos.round().to_i32();
        let bottom_square_y = start_grid_square.y.min(end_grid_square.y);
        let left_square_x = start_grid_square.x.min( end_grid_square.x);

        for i in 0..squares_to_place.len() {
            for j in 0..squares_to_place[0].len() {
                if let Some(new_glyph) = squares_to_place[i][j] {
                    let grid_square = p(left_square_x + i as i32, bottom_square_y + j as i32);
                    if !self.square_is_on_screen(grid_square) {
                        continue;
                    }
                    let grid_glyph =
                        &mut self.output_buffer[grid_square.x as usize][grid_square.y as usize];
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

    fn fill_output_buffer_with_black(&mut self) {
        for x in 0..self.terminal_size.0 as usize {
            for y in 0..self.terminal_size.1 as usize {
                self.output_buffer[x][y] = Glyph::from_char(' ');
            }
        }
    }


    fn get_buffered_glyph(&self, pos: Point2D<i32>) -> &Glyph {
        return &self.output_buffer[pos.x as usize][pos.y as usize];
    }
    fn set_buffered_glyph(&mut self, pos: Point2D<i32>, new_glyph: Glyph) {
        self.output_buffer[pos.x as usize][pos.y as usize] = new_glyph;
    }
    #[allow(dead_code)]
    fn get_glyph_on_screen(&self, pos: Point2D<i32>) -> &Glyph {
        return &self.output_on_screen[pos.x as usize][pos.y as usize];
    }

    fn print_output_buffer(&self) {
        for y in 0..self.height() as usize {
            let reverse_y: usize = self.height() as usize - 1 - y;
            let mut row_string = String::new();
            for x in 0..self.width() as usize {
                row_string += &self.output_buffer[x][reverse_y].to_string();
            }
            row_string += &Glyph::reset_colors();
            if reverse_y % 5 == 0 {
                row_string += &format!("-- {}", reverse_y);
            }
            println!("{}", row_string);
        }
    }

    fn update_output_buffer(&mut self) {
        self.fill_output_buffer_with_black();

        //if self.player.alive {
            //self.draw_player();
        //}
    }

    fn update_screen(&mut self, stdout: &mut MouseTerminal<RawTerminal<std::io::Stdout>>) {
        // Now update the graphics where applicable
        for x in 0..self.width() as usize {
            for y in 0..self.height() as usize{
                if self.output_buffer[x][y] != self.output_on_screen[x][y] {
                    let (term_x, term_y) = self.world_to_screen(&(x as i32, y as i32));
                    write!(stdout, "{}", termion::cursor::Goto(term_x, term_y)).unwrap();
                    write!(stdout, "{}", self.output_buffer[x][y].to_string()).unwrap();
                }
            }
        }
        //write!(
            //stdout,
            //"{}{}",
            //termion::cursor::Goto(1, 1),
            //self.fps() as i32
        //)
            //.unwrap();
        stdout.flush().unwrap();
        self.output_on_screen = self.output_buffer.clone();
    }
}