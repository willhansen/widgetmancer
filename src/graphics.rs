use std::any::Any;
use std::borrow::Borrow;
use std::cmp::min;
use std::collections::HashMap;
use std::f32::consts::{PI, TAU};
use std::io::Write;
use std::mem::swap;
use std::ptr::hash;
use std::time::{Duration, Instant};

use euclid::*;
use line_drawing::Point;
use rand::{Rng, SeedableRng};
use rgb::RGB8;
use termion::input::MouseTerminal;
use termion::raw::RawTerminal;
use termion::terminal_size;

use crate::animations::*;
use crate::num::ToPrimitive;
use crate::piece::Piece;
use crate::{
    get_by_point, point_to_string, BoardSize, BrailleGridInWorldFrame, BufferCharacterPoint,
    BufferCharacterSquare, CharacterGridInBufferFrame, CharacterGridInScreenFrame,
    CharacterGridInWorldFrame, Game, Glyph, IPoint, PieceType, ScreenCharacterPoint,
    ScreenCharacterSquare, SquareGridInWorldFrame, SquareList, WorldBraillePoint,
    WorldCharacterGlyphMap, WorldCharacterPoint, WorldCharacterSquare, WorldMove, WorldPoint,
    WorldSquare, WorldSquareRect, WorldStep, BLACK, BOARD_BLACK, BOARD_WHITE, EXPLOSION_COLOR, RED,
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
    board_animation: Option<Box<dyn BoardAnimation>>,
    start_time: Instant,
}

impl Graphics {
    pub(crate) fn new(terminal_width: u16, terminal_height: u16, start_time: Instant) -> Graphics {
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
            board_animation: None,
            start_time,
        }
    }

    pub fn start_time(&self) -> Instant {
        self.start_time
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

    fn count_braille_dots_in_square(&self, square: WorldSquare) -> u32 {
        return if self.square_is_on_screen(square) {
            Glyph::count_braille_dots(
                self.get_buffered_glyph(self.world_square_to_buffer_square(square))
                    .character,
            )
        } else {
            0
        };
    }

    fn draw_braille_point(&mut self, pos: WorldPoint, color: RGB8) {
        self.draw_braille_line(pos, pos, color);
    }

    fn draw_glyphs(&mut self, glyph_map: WorldCharacterGlyphMap) {
        for (world_character_square, new_glyph) in glyph_map {
            let buffer_character_square =
                self.world_character_square_to_buffer_square(world_character_square);
            if !self.world_character_is_on_screen(world_character_square) {
                continue;
            }

            self.draw_glyph(buffer_character_square, new_glyph);
        }
    }

    pub fn draw_string_below_board(&mut self, string: String) {
        let left_of_screen_under_board =
            self.world_square_to_left_screen_square(point2(0, 0)) + vec2(0, -1);
        self.draw_string(left_of_screen_under_board, &string);
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
        self.fill_output_buffer_with_solid_color(BLACK);
    }
    pub fn fill_output_buffer_with_solid_color(&mut self, color: RGB8) {
        for x in 0..self.terminal_width as usize {
            for y in 0..self.terminal_height as usize {
                self.output_buffer[x][y] = Glyph::new(' ', WHITE, color);
            }
        }
    }

    pub fn set_empty_board_animation(&mut self, board_size: BoardSize) {
        self.board_animation = Some(Box::new(StaticBoard::new(board_size)));
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

    pub fn draw_glyph(&mut self, buffer_pos: BufferCharacterSquare, mut new_glyph: Glyph) {
        let old_glyph = self.output_buffer[buffer_pos.x as usize][buffer_pos.y as usize];

        if new_glyph.bg_alpha == 0 {
            if old_glyph.is_solid() {
                new_glyph.bg_color = old_glyph.get_solid_color().unwrap();
            }
            if old_glyph.is_braille() {
                new_glyph.bg_color = old_glyph.bg_color;
                new_glyph.character =
                    Glyph::combine_braille_characters(new_glyph.character, old_glyph.character);
            }
        }
        self.output_buffer[buffer_pos.x as usize][buffer_pos.y as usize] = new_glyph;
    }

    pub fn draw_piece(&mut self, piece: Piece, pos: WorldSquare) {
        let bg_color = Graphics::board_color_at_square(pos);
        let mut glyphs = piece.glyphs().map(|glyph| glyph.with_bg(bg_color));
        self.draw_glyphs_at_square(pos, glyphs);
    }

    pub fn draw_danger_squares(&mut self, danger_squares: SquareList) {
        danger_squares
            .into_iter()
            .for_each(|square| self.draw_glyphs_at_square(square, Glyph::danger_square_glyphs()));
    }
    pub fn draw_block(&mut self, block_square: WorldSquare) {
        self.draw_glyphs_at_square(block_square, Glyph::block_glyphs());
    }

    pub fn add_simple_laser(&mut self, start: WorldPoint, end: WorldPoint) {
        self.active_animations
            .push(Box::new(SimpleLaser::new(start, end)));
    }
    pub fn add_floaty_laser(&mut self, start: WorldPoint, end: WorldPoint) {
        self.active_animations
            .push(Box::new(FloatyLaser::new(start, end)));
    }

    pub fn add_explosion(&mut self, position: WorldPoint) {
        self.active_animations
            .push(Box::new(Explosion::new(position)));
    }
    pub fn add_selector(&mut self, square: WorldSquare) {
        self.active_animations.push(Box::new(Selector::new(square)));
    }

    pub fn start_recoil_animation(&mut self, board_size: BoardSize, shot_direction: WorldStep) {
        self.board_animation = Some(Box::new(RecoilingBoard::new(board_size, shot_direction)));
    }

    pub fn draw_board_animation(&mut self, time: Instant) {
        if let Some(board_animation) = &self.board_animation {
            self.draw_animation(board_animation.clone(), time);
        }
    }

    fn draw_animation(&mut self, animation: AnimationObject, time: Instant) {
        self.draw_animations(vec![animation], time)
    }

    fn draw_animations(&mut self, animations: AnimationList, time: Instant) {
        animations
            .into_iter()
            .map(|animation| animation.glyphs_at_time(time))
            .for_each(|glyph_map| self.draw_glyphs(glyph_map));
    }

    pub fn draw_non_board_animations(&mut self, time: Instant) {
        let mut glyphs_to_draw = vec![];
        for animation in &self.active_animations {
            glyphs_to_draw.push(animation.glyphs_at_time(time));
        }
        for selector in &self.selectors {
            glyphs_to_draw.push(selector.glyphs_at_time(time))
        }

        for glyph_map in glyphs_to_draw {
            self.draw_glyphs(glyph_map);
        }
    }

    pub fn remove_finished_animations(&mut self, time: Instant) {
        if let Some(board_animation) = &mut self.board_animation {
            if board_animation.finished_at_time(time) {
                self.board_animation = Some(board_animation.next_animation())
            }
        }
        self.active_animations
            .drain_filter(|x| x.finished_at_time(time));
        self.selectors.drain_filter(|x| x.finished_at_time(time));
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

    pub fn count_buffered_braille_dots_in_rect(&self, rect: WorldSquareRect) -> u32 {
        let mut count: u32 = 0;
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

    pub fn glyph_map_to_string(glyph_map: WorldCharacterGlyphMap) -> String {
        let top_row = glyph_map.keys().map(|square| square.y).max().unwrap();
        let bottom_row = glyph_map.keys().map(|square| square.y).min().unwrap();
        let left_column = glyph_map.keys().map(|square| square.x).min().unwrap();
        let right_column = glyph_map.keys().map(|square| square.x).max().unwrap();
        let mut string = String::new();
        for bottom_to_top_y in bottom_row..=top_row {
            let y = top_row + bottom_row - bottom_to_top_y;
            for x in left_column..=right_column {
                let square = WorldCharacterSquare::new(x, y);
                let new_part = if let Some(glyph) = glyph_map.get(&square) {
                    glyph.to_string()
                } else {
                    " ".to_string()
                };

                string += &new_part;
            }
            string += "\n";
        }
        string
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::{BLUE, RIGHT_I};

    use super::*;

    fn set_up_graphics() -> Graphics {
        Graphics::new(40, 20, Instant::now())
    }

    #[test]
    fn test_world_to_screen() {
        let g = set_up_graphics();

        let world_square: WorldSquare = point2(0, 0);
        let screen_character_square: ScreenCharacterSquare = point2(1, g.terminal_height());
        assert_eq!(
            screen_character_square,
            g.world_square_to_left_screen_square(world_square)
        );

        let world_square: WorldSquare = point2(0, g.terminal_height() - 1);
        let screen_character_square: ScreenCharacterSquare = point2(1, 1);
        assert_eq!(
            screen_character_square,
            g.world_square_to_left_screen_square(world_square)
        );
    }

    #[test]
    fn test_one_world_square_is_two_characters() {
        let g = set_up_graphics();

        let world_pos = point2(5, 5); // arbitrary
        let screen_pos1 = g.world_square_to_left_screen_square(world_pos + RIGHT_I.cast_unit());
        let screen_pos2 = g.world_square_to_left_screen_square(world_pos) + RIGHT_I.cast_unit() * 2;
        assert_eq!(screen_pos1, screen_pos2);
    }

    #[test]
    fn test_draw_diagonal_braille_line() {
        let mut g = set_up_graphics();
        let line_start = WorldSquare::new(2, 2);
        let line_end = WorldSquare::new(7, 7);

        g.draw_braille_line(line_start.to_f32(), line_end.to_f32(), RED);

        let test_square = WorldSquare::new(4, 4);

        let [glyph_left, glyph_right] = g.get_buffered_glyphs_for_square(test_square);
        //g.print_output_buffer();

        assert_eq!(glyph_left.fg_color, RED);
    }

    #[test]
    fn test_single_braille_point() {
        let mut g = set_up_graphics();
        let test_square = point2(5, 5);
        let diag = vec2(1, 1);
        let test_rectangle = WorldSquareRect::new(test_square - diag, test_square + diag);
        let test_point = test_square.to_f32();

        g.draw_braille_point(test_point, RED);
        assert_eq!(g.count_buffered_braille_dots_in_rect(test_rectangle), 1)
    }

    #[test]
    fn test_laser_has_transparent_background() {
        let mut g = set_up_graphics();
        g.fill_output_buffer_with_solid_color(BLUE);
        assert_eq!(
            g.get_buffered_glyphs_for_square(point2(5, 0))[0].bg_color,
            BLUE
        );
        g.add_simple_laser(point2(0.0, 0.0), point2(10.0, 0.0));
        g.draw_non_board_animations(Instant::now());
        //g.print_output_buffer();
        assert_eq!(
            g.get_buffered_glyphs_for_square(point2(5, 0))[0].bg_color,
            BLUE
        );
        //g.print_output_buffer();
    }
    #[test]
    fn test_draw_on_far_right_square_in_odd_width_terminal() {
        let mut g = Graphics::new(41, 20, Instant::now());
        g.add_simple_laser(point2(0.0, 0.0), point2(50.0, 0.0));
        g.draw_non_board_animations(Instant::now());
    }
}
