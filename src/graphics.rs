use std::any::Any;
use std::borrow::Borrow;
use std::cmp::min;
use std::collections::HashMap;
use std::f32::consts::{PI, TAU};
use std::io::Write;
use std::mem::swap;
use std::ptr::hash;
use std::time::{Duration, Instant};

use crate::animations::*;
use crate::num::ToPrimitive;
use euclid::*;
use line_drawing::Point;
use rand::{Rng, SeedableRng};
use rgb::RGB8;
use termion::input::MouseTerminal;
use termion::raw::RawTerminal;
use termion::terminal_size;

use crate::piece::Piece;
use crate::{
    get_by_point, point_to_string, BrailleGridInWorldFrame, BufferCharacterPoint,
    BufferCharacterSquare, CharacterGridInBufferFrame, CharacterGridInScreenFrame,
    CharacterGridInWorldFrame, Game, Glyph, IPoint, PieceType, ScreenCharacterPoint,
    SquareGridInWorldFrame, WorldBraillePoint, WorldCharacterPoint, WorldGlyphMap, WorldMove,
    WorldPoint, WorldSquare, WorldStep, BLACK, BOARD_BLACK, BOARD_WHITE, EXPLOSION_COLOR, RED,
    RIGHT_I, WHITE,
};

pub struct Graphics {
    output_buffer: Vec<Vec<Glyph>>,
    // (x,y), left to right, top to bottom
    output_on_screen: Vec<Vec<Glyph>>,
    // (x,y), left to right, top to bottom
    terminal_width: u16,
    terminal_height: u16,
    active_animations: Vec<Box<dyn Animation>>,
    selectors: Vec<Selector>,
    start_time: Instant,
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
            active_animations: vec![],
            selectors: vec![],
            start_time: Instant::now(),
        }
    }

    fn time_from_start(&self) -> Duration {
        Instant::now().duration_since(self.start_time)
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
            Glyph::world_character_point_to_world_point(character_square.to_f32())
                .round()
                .to_i32(),
        )
    }
    fn square_is_on_screen(&self, square: WorldSquare) -> bool {
        self.buffer_character_is_on_screen(self.world_square_to_buffer_square(square))
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

    ////////////////////////////////////////////////////////////////////////////////
    // CONVERSIONS START
    ////////////////////////////////////////////////////////////////////////////////

    // world point -> world character point -> buffer point -> screen point
    //       ↓                 ↓                    ↓               ↓
    // world square   world_character square   buffer square   screen square

    pub fn world_character_point_to_buffer_point(
        &self,
        world_character_point: WorldCharacterPoint,
    ) -> BufferCharacterPoint {
        // buffer indexes from 0, and the y axis goes top to bottom
        // world indexes from 0, origin at bottom left
        point2(
            world_character_point.x,
            self.terminal_height as f32 - (world_character_point.y + 1.0),
        )
    }

    pub fn world_character_square_to_buffer_square(
        &self,
        world_character_square: Point2D<i32, CharacterGridInWorldFrame>,
    ) -> Point2D<i32, CharacterGridInBufferFrame> {
        self.world_character_point_to_buffer_point(world_character_square.to_f32())
            .round()
            .to_i32()
    }

    pub fn world_square_to_left_buffer_square(
        &self,
        world_position: WorldSquare,
    ) -> Point2D<i32, CharacterGridInBufferFrame> {
        self.screen_square_to_buffer_square(self.world_square_to_left_screen_square(world_position))
    }

    pub fn world_point_to_buffer_point(&self, world_point: WorldPoint) -> BufferCharacterPoint {
        self.world_character_point_to_buffer_point(Glyph::world_point_to_world_character_point(
            world_point,
        ))
    }

    pub fn world_square_to_buffer_square(
        &self,
        world_position: WorldSquare,
    ) -> Point2D<i32, CharacterGridInBufferFrame> {
        self.screen_square_to_buffer_square(self.world_square_to_left_screen_square(world_position))
    }

    pub fn buffer_point_to_screen_point(
        buffer_point: BufferCharacterPoint,
    ) -> ScreenCharacterPoint {
        // Buffer indexes from 0
        // Screen indexes from 1
        point2(buffer_point.x + 1.0, buffer_point.y + 1.0)
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

    pub fn screen_square_to_buffer_square(
        &self,
        screen_pos: Point2D<i32, CharacterGridInScreenFrame>,
    ) -> Point2D<i32, CharacterGridInBufferFrame> {
        // Terminal indexes from 1, and the y axis goes top to bottom
        // The screen buffer indexes from 0, but the y axis also goes top to bottom
        // End result is just an off-by-one offset on both axes
        point2(screen_pos.x - 1, screen_pos.y - 1)
    }

    pub fn world_point_to_screen_point(
        &self,
        world_position: Point2D<f32, SquareGridInWorldFrame>,
    ) -> Point2D<f32, CharacterGridInScreenFrame> {
        // terminal indexes from 1, and the y axis goes top to bottom
        // world indexes from 0, origin at bottom left
        Graphics::buffer_point_to_screen_point(self.world_character_point_to_buffer_point(
            Glyph::world_point_to_world_character_point(world_position),
        ))
    }

    pub fn world_square_to_left_screen_square(
        &self,
        world_position: WorldSquare,
    ) -> Point2D<i32, CharacterGridInScreenFrame> {
        // terminal indexes from 1, and the y axis goes top to bottom
        // world indexes from 0, origin at bottom left
        point2(
            world_position.x * 2 + 1,
            self.terminal_height as i32 - world_position.y,
        )
    }

    ////////////////////////////////////////////////////////////////////////////////
    // CONVERSIONS END
    ////////////////////////////////////////////////////////////////////////////////

    fn count_braille_dots_in_square(&self, square: WorldSquare) -> i32 {
        return if self.square_is_on_screen(square) {
            Glyph::count_braille_dots(
                self.get_buffered_glyph(self.world_square_to_buffer_square(square))
                    .character,
            )
        } else {
            0
        };
    }

    fn draw_braille_point(&mut self, pos: Point2D<f32, SquareGridInWorldFrame>, color: RGB8) {
        self.draw_braille_line(pos, pos, color);
    }

    fn draw_glyphs(&mut self, glyph_map: WorldGlyphMap) {
        for (world_character_square, new_glyph) in glyph_map {
            let buffer_character_square =
                self.world_character_square_to_buffer_square(world_character_square);
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
            grid_glyph.bg_color = Graphics::board_color_at_square(
                Glyph::world_character_square_to_world_square(world_character_square),
            );
        }
    }

    fn draw_braille_line(
        &mut self,
        start_pos: Point2D<f32, SquareGridInWorldFrame>,
        end_pos: Point2D<f32, SquareGridInWorldFrame>,
        color: RGB8,
    ) {
        let line_glyphs = Glyph::get_glyphs_for_colored_braille_line(start_pos, end_pos, color);
        self.draw_glyphs(line_glyphs);
    }

    pub fn fill_output_buffer_with_black(&mut self) {
        for x in 0..self.terminal_width as usize {
            for y in 0..self.terminal_height as usize {
                self.output_buffer[x][y] = Glyph::from_char(' ');
            }
        }
    }
    pub fn draw_empty_board(&mut self, width: usize, height: usize) {
        for x in 0..width {
            for y in 0..height {
                let mut glyph = Glyph::from_char(' ');
                let world_square: WorldSquare = WorldSquare::new(x as i32, y as i32);
                glyph.bg_color = Graphics::board_color_at_square(world_square);
                self.draw_glyphs_at_square(world_square, [glyph, glyph]);
            }
        }
    }

    pub fn board_color_at_square(square: WorldSquare) -> RGB8 {
        if (square.x + square.y) % 2 == 0 {
            BOARD_WHITE
        } else {
            BOARD_BLACK
        }
    }

    pub fn get_buffered_glyphs_for_square(&self, world_pos: WorldSquare) -> [Glyph; 2] {
        let buffer_pos = self.world_square_to_buffer_square(world_pos);
        [
            self.get_buffered_glyph(buffer_pos).clone(),
            self.get_buffered_glyph(buffer_pos + RIGHT_I.cast_unit())
                .clone(),
        ]
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
            let mut row_string = String::new();
            for x in 0..self.terminal_width() as usize {
                row_string += &self.output_buffer[x][y].to_string();
            }
            row_string += &Glyph::reset_colors();
            if y % 5 == 0 || y == self.terminal_height() as usize - 1 {
                row_string += &format!("――{}", y);
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

    pub fn draw_string(
        &mut self,
        screen_pos: Point2D<i32, CharacterGridInScreenFrame>,
        the_string: &str,
    ) {
        for i in 0..the_string.chars().count() {
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

    pub fn draw_player(&mut self, world_pos: WorldSquare, faced_direction: WorldStep) {
        let mut player_glyphs = Glyph::get_glyphs_for_player(faced_direction);
        let square_color = Graphics::board_color_at_square(world_pos);
        player_glyphs[0].bg_color = square_color;
        player_glyphs[1].bg_color = square_color;
        self.draw_glyphs_at_square(world_pos, player_glyphs);
    }

    pub fn draw_glyphs_at_square(&mut self, world_pos: WorldSquare, glyphs: [Glyph; 2]) {
        if !self.square_is_on_screen(world_pos) {
            panic!(
                "Tried to draw square off screen: {}",
                point_to_string(world_pos)
            );
        }
        let buffer_square = self.world_square_to_left_buffer_square(world_pos);
        self.draw_glyph(buffer_square, *glyphs.get(0).unwrap());
        self.draw_glyph(buffer_square + RIGHT_I.cast_unit(), *glyphs.get(1).unwrap());
    }

    pub fn draw_glyph(
        &mut self,
        buffer_pos: Point2D<i32, CharacterGridInBufferFrame>,
        glyph: Glyph,
    ) {
        // TODO: account for alpha colors and braille combinations
        self.output_buffer[buffer_pos.x as usize][buffer_pos.y as usize] = glyph;
    }

    pub fn draw_piece(&mut self, piece: Piece, pos: WorldSquare) {
        let bg_color = Graphics::board_color_at_square(pos);
        let mut glyphs = piece.glyphs().map(|glyph| glyph.with_bg(bg_color));
        self.draw_glyphs_at_square(pos, glyphs);
    }

    pub fn add_laser(&mut self, start: WorldPoint, end: WorldPoint) {
        self.active_animations
            .push(Box::new(Laser::new(start, end)));
    }
    pub fn add_sniper_shot(&mut self, start: WorldPoint, end: WorldPoint) {
        self.active_animations
            .push(Box::new(SniperShot::new(start, end)));
    }

    pub fn add_explosion(&mut self, position: WorldPoint) {
        self.active_animations
            .push(Box::new(Explosion::new(position)));
    }
    pub fn add_selector(&mut self, square: WorldSquare) {
        self.active_animations.push(Box::new(Selector::new(square)));
    }

    pub fn play_animations(&mut self, delta: Duration) {
        self.draw_animations();
        self.advance_animations(delta);
    }

    fn draw_animations(&mut self) {
        let mut glyphs_to_draw = vec![];
        for animation in &self.active_animations {
            glyphs_to_draw.push(animation.glyphs());
        }
        for selector in &self.selectors {
            glyphs_to_draw.push(selector.glyphs())
        }

        for glyph_map in glyphs_to_draw {
            self.draw_glyphs(glyph_map);
        }
    }

    fn advance_animations(&mut self, delta: Duration) {
        for mut animation in &mut self.active_animations {
            animation.advance(delta);
        }
        for mut selector in &mut self.selectors {
            selector.advance(delta);
        }

        self.cull_dead_animations();
    }

    fn cull_dead_animations(&mut self) {
        self.active_animations.drain_filter(|x| x.finished());
        self.selectors.drain_filter(|x| x.finished());
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

    pub fn count_buffered_braille_dots_in_rect(
        &self,
        rect: Box2D<i32, SquareGridInWorldFrame>,
    ) -> i32 {
        let mut count = 0;
        for x in rect.min.x..=rect.max.x {
            for y in rect.min.y..=rect.max.y {
                let square = WorldSquare::new(x, y);
                let glyphs = self.get_buffered_glyphs_for_square(square);
                for glyph in glyphs {
                    let character = glyph.character;
                    count += Glyph::count_braille_dots(character);
                }
            }
        }
        count
    }

    pub fn select_squares(&mut self, squares: Vec<WorldSquare>) {
        self.selectors = squares
            .into_iter()
            .map(|square| Selector::new(square))
            .collect();
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::RIGHT_I;

    use super::*;

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
        let line_start = WorldSquare::new(2, 2);
        let line_end = WorldSquare::new(7, 7);

        g.draw_braille_line(line_start.to_f32(), line_end.to_f32(), RED);

        let test_square = WorldSquare::new(4, 4);

        let [glyph_left, glyph_right] = g.get_buffered_glyphs_for_square(test_square);
        //g.print_output_buffer();

        assert_eq!(glyph_left.fg_color, RED);
    }
}
