use std::borrow::Borrow;
use std::cmp::min;
use std::collections::HashMap;
use std::io::Write;
use std::mem::swap;
use std::time::Duration;

use euclid::*;
use line_drawing::Point;
use termion::input::MouseTerminal;
use termion::raw::RawTerminal;
use termion::terminal_size;

use crate::piece::Piece;
use crate::{
    get_by_point, BrailleGridInWorldFrame, CharacterGridInBufferFrame, CharacterGridInScreenFrame,
    CharacterGridInWorldFrame, ColorName, Game, Glyph, IPoint, PieceType, Square,
    SquareGridInWorldFrame, RIGHT_I,
};

pub struct Graphics {
    output_buffer: Vec<Vec<Glyph>>,
    // (x,y), left to right, top to bottom
    output_on_screen: Vec<Vec<Glyph>>,
    // (x,y), left to right, top to bottom
    terminal_width: u16,
    terminal_height: u16,
}

impl Graphics {
    pub(crate) fn new(terminal_width: u16, terminal_height: u16) -> Graphics {
        Graphics {
            output_buffer: vec![
                vec![Glyph::from_char(' '); terminal_height as usize];
                terminal_width as usize
            ],
            output_on_screen: vec![
                vec![Glyph::from_char('x'); terminal_height as usize];
                terminal_width as usize
            ],
            terminal_width,
            terminal_height,
        }
    }

    fn terminal_width(&self) -> i32 {
        self.terminal_width as i32
    }

    fn terminal_height(&self) -> i32 {
        self.terminal_height as i32
    }

    fn world_character_is_on_screen(
        &self,
        character_square: Point2D<i32, CharacterGridInWorldFrame>,
    ) -> bool {
        self.square_is_on_screen(
            Glyph::character_world_pos_to_exact_world_pos(character_square.to_f32())
                .round()
                .to_i32(),
        )
    }
    fn square_is_on_screen(&self, square: Square) -> bool {
        self.buffer_character_is_on_screen(self.world_square_to_buffer_character_square(square))
    }

    fn buffer_character_is_on_screen(
        &self,
        buffer_char_pos: Point2D<i32, CharacterGridInBufferFrame>,
    ) -> bool {
        return buffer_char_pos.x >= 0
            && buffer_char_pos.x < self.terminal_width as i32
            && buffer_char_pos.y >= 0
            && buffer_char_pos.y < self.terminal_height as i32;
    }

    pub fn world_square_to_left_screen_square(
        &self,
        world_position: Square,
    ) -> Point2D<i32, CharacterGridInScreenFrame> {
        // terminal indexes from 1, and the y axis goes top to bottom
        // world indexes from 0, origin at bottom left
        point2(
            world_position.x * 2 + 1,
            self.terminal_height as i32 - world_position.y,
        )
    }

    pub fn world_pos_to_exact_screen_pos(
        &self,
        world_position: Point2D<f32, SquareGridInWorldFrame>,
    ) -> Point2D<f32, CharacterGridInScreenFrame> {
        // terminal indexes from 1, and the y axis goes top to bottom
        // world indexes from 0, origin at bottom left
        point2(
            world_position.x * 2.0 + 1.5,
            self.terminal_height as f32 - world_position.y,
        )
    }

    pub fn screen_square_to_buffer_square(
        &self,
        screen_pos: Point2D<i32, CharacterGridInScreenFrame>,
    ) -> Point2D<i32, CharacterGridInBufferFrame> {
        // Terminal indexes from 1, and the y axis goes top to bottom
        // The screen buffer indexes from 0, but the y axis also goes top to bottom
        // End result is just an off-by-one offset on both axes
        point2(screen_pos.x - 1, screen_pos.y - 1)
    }
    pub fn buffer_square_to_screen_square(
        &self,
        buffer_pos: Point2D<i32, CharacterGridInBufferFrame>,
    ) -> Point2D<i32, CharacterGridInScreenFrame> {
        // Terminal indexes from 1, and the y axis goes top to bottom
        // The screen buffer indexes from 0, but the y axis also goes top to bottom
        // End result is just an off-by-one offset on both axes
        point2(buffer_pos.x + 1, buffer_pos.y + 1)
    }
    pub fn world_square_to_buffer_character_square(
        &self,
        world_position: Square,
    ) -> Point2D<i32, CharacterGridInBufferFrame> {
        self.screen_square_to_buffer_square(self.world_square_to_left_screen_square(world_position))
    }

    pub fn world_character_square_to_buffer_character_square(
        &self,
        world_character_square: Point2D<i32, CharacterGridInWorldFrame>,
    ) -> Point2D<i32, CharacterGridInBufferFrame> {
        let point_in_screen_space = self.world_pos_to_exact_screen_pos(
            Glyph::character_world_pos_to_exact_world_pos(world_character_square.to_f32()),
        );
        let screen_square = point_in_screen_space.round().to_i32();
        self.screen_square_to_buffer_square(screen_square)
    }

    fn braille_bresenham_line_points(
        start_pos: Point2D<f32, SquareGridInWorldFrame>,
        end_pos: Point2D<f32, SquareGridInWorldFrame>,
    ) -> Vec<Point2D<f32, BrailleGridInWorldFrame>> {
        let braille_pos0 = Glyph::world_pos_to_braille_pos(start_pos).round().to_i32();
        let braille_pos1 = Glyph::world_pos_to_braille_pos(end_pos).round().to_i32();

        line_drawing::Bresenham::new(braille_pos0.to_tuple(), braille_pos1.to_tuple())
            .map(|(x, y)| Point2D::<f32, BrailleGridInWorldFrame>::new(x as f32, y as f32))
            .collect()
    }

    fn count_braille_dots_in_square(&self, square: Square) -> i32 {
        return if self.square_is_on_screen(square) {
            Glyph::count_braille_dots(
                self.get_buffered_glyph(self.world_square_to_buffer_character_square(square))
                    .character,
            )
        } else {
            0
        };
    }

    fn draw_visual_braille_point(
        &mut self,
        pos: Point2D<f32, SquareGridInWorldFrame>,
        color: ColorName,
    ) {
        self.draw_visual_braille_line(pos, pos, color);
    }

    fn draw_visual_braille_line(
        &mut self,
        start_pos: Point2D<f32, SquareGridInWorldFrame>,
        end_pos: Point2D<f32, SquareGridInWorldFrame>,
        color: ColorName,
    ) {
        let start_char = Glyph::world_pos_to_character_world_pos(start_pos);
        let end_char = Glyph::world_pos_to_character_world_pos(end_pos);
        let map_of_line_glyphs =
            Glyph::get_glyphs_for_colored_braille_line(start_char, end_char, color);

        for (world_character_square, new_glyph) in map_of_line_glyphs {
            let buffer_character_square =
                self.world_character_square_to_buffer_character_square(world_character_square);
            if !self.world_character_is_on_screen(world_character_square) {
                continue;
            }
            let grid_glyph = &mut self.output_buffer[buffer_character_square.x as usize]
                [buffer_character_square.y as usize];
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

    pub fn fill_output_buffer_with_black(&mut self) {
        for x in 0..self.terminal_width as usize {
            for y in 0..self.terminal_height as usize {
                self.output_buffer[x][y] = Glyph::from_char(' ');
            }
        }
    }
    pub fn fill_output_buffer_with_checker(&mut self) {
        for x in 0..self.terminal_width as usize {
            for y in 0..self.terminal_height as usize {
                let mut glyph = Glyph::from_char(' ');
                if (x / 2 + y) % 2 == 0 {
                    swap(&mut glyph.bg_color, &mut glyph.fg_color);
                }
                self.output_buffer[x][y] = glyph;
            }
        }
    }

    pub fn get_buffered_glyphs_for_square(&self, world_pos: Square) -> (Glyph, Glyph) {
        let buffer_pos = self.world_square_to_buffer_character_square(world_pos);
        (
            self.get_buffered_glyph(buffer_pos).clone(),
            self.get_buffered_glyph(buffer_pos + RIGHT_I.cast_unit())
                .clone(),
        )
    }

    fn get_buffered_glyph(&self, pos: Point2D<i32, CharacterGridInBufferFrame>) -> &Glyph {
        return &self.output_buffer[pos.x as usize][pos.y as usize];
    }
    fn set_buffered_glyph(
        &mut self,
        pos: Point2D<i32, CharacterGridInBufferFrame>,
        new_glyph: Glyph,
    ) {
        self.output_buffer[pos.x as usize][pos.y as usize] = new_glyph;
    }
    #[allow(dead_code)]
    fn get_glyph_on_screen(&self, screen_pos: Point2D<i32, CharacterGridInScreenFrame>) -> &Glyph {
        let buffer_pos = self.screen_square_to_buffer_square(screen_pos);
        return &self.output_on_screen[buffer_pos.x as usize][buffer_pos.y as usize];
    }

    pub fn print_output_buffer(&self) {
        for y in 0..self.terminal_height() as usize {
            let reverse_y: usize = self.terminal_height() as usize - 1 - y;
            let mut row_string = String::new();
            for x in 0..self.terminal_width() as usize {
                row_string += &self.output_buffer[x][reverse_y].to_string();
            }
            row_string += &Glyph::reset_colors();
            if reverse_y % 5 == 0 || y == 0 {
                row_string += &format!("――{}", reverse_y);
            }
            println!("{}", row_string);
        }

        let mut x_markings = " ".repeat(self.terminal_width() as usize);
        let mut x_numbers = " ".repeat(self.terminal_width() as usize);

        for x in 0..self.terminal_width() as usize {
            if x % 5 == 0 || x == self.terminal_width() as usize - 1 {
                x_markings.insert_str(x, "|");
                x_numbers.insert_str(x, &(x.to_string()));
            }
        }
        println!("{}", x_markings);
        println!("{}", x_numbers);
    }
    pub fn display_headless(&mut self) {
        self.display(&mut None);
    }

    pub fn display(&mut self, optional_writer: &mut Option<Box<dyn Write>>) {
        if optional_writer.is_some() {
            self.update_screen(optional_writer.as_mut().unwrap());
        }
        self.output_on_screen = self.output_buffer.clone();
    }

    pub fn draw_string_to_screen(
        &mut self,
        screen_pos: Point2D<i32, CharacterGridInScreenFrame>,
        the_string: &str,
    ) {
        for i in 0..the_string.len() {
            let character: char = the_string.chars().nth(i).unwrap();
            let buffer_pos = self.screen_square_to_buffer_square(screen_pos);
            self.output_buffer[buffer_pos.x as usize + i][buffer_pos.y as usize] =
                Glyph::from_char(character);
        }
    }

    pub fn get_char_at_screen_pos(
        &self,
        screen_pos: Point2D<i32, CharacterGridInScreenFrame>,
    ) -> char {
        let buffer_pos = self.screen_square_to_buffer_square(screen_pos);
        get_by_point(&self.output_on_screen, buffer_pos).character
    }

    pub fn draw_player(&mut self, world_pos: Square) {
        self.draw_string_to_screen(self.world_square_to_left_screen_square(world_pos), "@@");
    }
    pub fn draw_piece(&mut self, piece: Piece, pos: Square) {
        self.draw_string_to_screen(self.world_square_to_left_screen_square(pos), "Pa");
    }
    pub fn draw_laser(&mut self, start: Square, end: Square) {
        self.draw_visual_braille_line(start.to_f32(), end.to_f32(), ColorName::Red);
    }

    pub fn update_screen(&mut self, writer: &mut Box<dyn Write>) {
        // Now update the graphics where applicable
        for buffer_x in 0..self.terminal_width() {
            for buffer_y in 0..self.terminal_height() {
                let buffer_pos: Point2D<i32, CharacterGridInBufferFrame> =
                    point2(buffer_x, buffer_y);
                if self.output_buffer[buffer_pos.x as usize][buffer_pos.y as usize]
                    != self.output_on_screen[buffer_pos.x as usize][buffer_pos.y as usize]
                {
                    let screen_pos: Point2D<i32, CharacterGridInScreenFrame> =
                        self.buffer_square_to_screen_square(buffer_pos);
                    write!(
                        writer,
                        "{}",
                        termion::cursor::Goto(screen_pos.x as u16, screen_pos.y as u16)
                    )
                    .unwrap();
                    write!(
                        writer,
                        "{}",
                        self.output_buffer[buffer_pos.x as usize][buffer_pos.y as usize]
                            .to_string()
                    )
                    .unwrap();
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
        writer.flush().unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::RIGHT_I;

    #[test]
    fn test_world_to_screen() {
        let g = Graphics::new(20, 20);

        let world_pos = point2(0, 0);
        let screen_pos = point2(1, 20);
        assert_eq!(screen_pos, g.world_square_to_left_screen_square(world_pos));

        let world_pos = point2(0, 19);
        let screen_pos = point2(1, 1);
        assert_eq!(screen_pos, g.world_square_to_left_screen_square(world_pos));
    }

    #[test]
    fn test_one_world_square_is_two_characters() {
        let g = Graphics::new(20, 20);

        let world_pos = point2(5, 5); // arbitrary
        let screen_pos1 = g.world_square_to_left_screen_square(world_pos + RIGHT_I.cast_unit());
        let screen_pos2 = g.world_square_to_left_screen_square(world_pos) + RIGHT_I.cast_unit() * 2;
        assert_eq!(screen_pos1, screen_pos2);
    }

    #[test]
    fn test_draw_diagonal_braille_line() {
        let mut g = Graphics::new(40, 20);
        let line_start = Square::new(2, 2);
        let line_end = Square::new(7, 7);

        g.draw_laser(line_start, line_end);

        let test_square = Square::new(4, 4);

        let (glyph_left, glyph_right) = g.get_buffered_glyphs_for_square(test_square);
        g.print_output_buffer();

        assert_eq!(glyph_left.fg_color, ColorName::Red);
    }
}
