use std::collections::HashMap;
use std::io::Write;

use rgb::RGB8;

use crate::glyph::glyph_constants::WHITE;
use crate::glyph::{DoubleGlyph, Glyph};
use crate::graphics::drawable::Drawable;
use crate::utility::units::{
    world_character_square_to_world_square, world_point_to_world_character_point,
    world_square_to_left_world_character_square, SquareSet, WorldCharacterSquare,
    WorldCharacterStep, WorldPoint, WorldSquare, WorldStep,
};
use crate::utility::{get_by_point, QuarterTurnsCcw, RIGHT_I, STEP_RIGHT, STEP_UP};
use crate::{point2, vec2, Coordinate, Point2D, SignedCoordinate, Vector2D};

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct CharacterGridInScreenBufferFrame;

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct SquareGridInScreenBufferFrame;

pub type ScreenBufferCharacterSquare = Point2D<i32, CharacterGridInScreenBufferFrame>;
pub type ScreenBufferCharacterStep = Vector2D<i32, CharacterGridInScreenBufferFrame>;

pub type ScreenBufferSquare = Point2D<i32, SquareGridInScreenBufferFrame>;
pub type ScreenBufferStep = Vector2D<i32, SquareGridInScreenBufferFrame>;

pub type ScreenBufferGlyphMap = HashMap<ScreenBufferCharacterSquare, Glyph>;

// Flipped y axis from the world step constants
pub const SCREEN_STEP_ZERO: ScreenBufferStep = vec2(0, 0);
pub const SCREEN_STEP_UP: ScreenBufferStep = vec2(0, -1);
pub const SCREEN_STEP_DOWN: ScreenBufferStep = vec2(0, 1);
pub const SCREEN_STEP_RIGHT: ScreenBufferStep = vec2(1, 0);
pub const SCREEN_STEP_LEFT: ScreenBufferStep = vec2(-1, 0);

pub const SCREEN_STEP_UP_RIGHT: ScreenBufferStep = vec2(1, -1);
pub const SCREEN_STEP_UP_LEFT: ScreenBufferStep = vec2(-1, -1);
pub const SCREEN_STEP_DOWN_LEFT: ScreenBufferStep = vec2(-1, 1);
pub const SCREEN_STEP_DOWN_RIGHT: ScreenBufferStep = vec2(1, 1);

pub struct Screen {
    screen_origin: WorldSquare,
    rotation: QuarterTurnsCcw,
    pub screen_buffer: Vec<Vec<Glyph>>,
    // (x,y), left to right, top to bottom
    pub current_screen_state: Vec<Vec<Glyph>>,
    // (x,y), left to right, top to bottom
    pub terminal_width: u16,
    pub terminal_height: u16,
}

impl Screen {
    pub fn new(terminal_width: u16, terminal_height: u16) -> Self {
        Screen {
            screen_origin: point2(0, terminal_height as i32 - 1),
            rotation: QuarterTurnsCcw::default(),
            screen_buffer: vec![
                vec![Glyph::from_char(' '); terminal_height as usize];
                terminal_width as usize
            ],
            current_screen_state: vec![
                vec![Glyph::from_char('x'); terminal_height as usize];
                terminal_width as usize
            ],
            terminal_width,
            terminal_height,
        }
    }
    pub fn new_by_square_dimensions(width: u16, height: u16) -> Self {
        Self::new(width * 2, height)
    }
    pub(crate) fn terminal_width(&self) -> i32 {
        self.terminal_width as i32
    }

    pub(crate) fn terminal_height(&self) -> i32 {
        self.terminal_height as i32
    }

    fn set_screen_origin_by_world_square(&mut self, world_square: WorldSquare) {
        self.screen_origin = world_square;
    }

    pub fn set_screen_center_by_world_square(&mut self, world_square: WorldSquare) {
        self.screen_origin = world_square - self.world_step_from_origin_to_center();
    }

    fn screen_max_as_world_square(&self) -> WorldSquare {
        self.screen_origin_as_world_square() + self.world_step_from_origin_to_max()
    }

    fn world_step_from_origin_to_max(&self) -> WorldStep {
        let unrotated = self
            .screen_step_from_origin_to_max()
            .reflect(STEP_UP.cast_unit())
            .cast_unit();
        self.rotation.rotate_vector(unrotated)
    }

    fn world_step_from_origin_to_center(&self) -> WorldStep {
        self.world_step_from_origin_to_max() / 2
    }
    fn screen_buffer_character_step_from_origin_to_max(&self) -> ScreenBufferCharacterStep {
        vec2(self.terminal_width() - 1, self.terminal_height() - 1)
    }
    fn screen_step_from_origin_to_max(&self) -> ScreenBufferStep {
        vec2(self.terminal_width() / 2 - 1, self.terminal_height() - 1)
    }

    fn screen_buffer_character_step_from_origin_to_center(&self) -> ScreenBufferCharacterStep {
        self.screen_buffer_character_step_from_origin_to_max() / 2
    }
    fn screen_buffer_step_from_origin_to_center(&self) -> ScreenBufferStep {
        self.screen_step_from_origin_to_max() / 2
    }

    pub fn screen_center_as_world_square(&self) -> WorldSquare {
        self.screen_origin_as_world_square() + self.world_step_from_origin_to_center()
    }
    pub fn screen_origin_as_world_square(&self) -> WorldSquare {
        self.screen_origin
    }

    pub fn screen_center_as_screen_buffer_character_square(&self) -> ScreenBufferCharacterSquare {
        self.screen_buffer_character_step_from_origin_to_center()
    }

    fn screen_center_as_screen_buffer_square(&self) -> ScreenBufferSquare {
        self.screen_buffer_step_from_origin_to_center()
    }

    pub fn world_square_is_at_least_partially_on_screen(&self, square: WorldSquare) -> bool {
        self.world_square_to_both_screen_buffer_character_squares(square)
            .into_iter()
            .any(|buffer_square| self.buffer_character_square_is_on_screen(buffer_square))
    }

    pub fn update_screen(&mut self, writer: &mut Box<dyn Write>) {
        // Now update the graphics where applicable
        for buffer_x in 0..self.terminal_width() {
            for buffer_y in 0..self.terminal_height() {
                let buffer_pos: Point2D<i32, CharacterGridInScreenBufferFrame> =
                    point2(buffer_x, buffer_y);
                if self.screen_buffer[buffer_pos.x as usize][buffer_pos.y as usize]
                    != self.current_screen_state[buffer_pos.x as usize][buffer_pos.y as usize]
                {
                    let screen_pos_starting_at_1 = buffer_pos + vec2(1, 1);
                    write!(
                        writer,
                        "{}",
                        termion::cursor::Goto(
                            screen_pos_starting_at_1.x as u16,
                            screen_pos_starting_at_1.y as u16,
                        )
                    )
                    .unwrap();
                    write!(
                        writer,
                        "{}",
                        self.screen_buffer[buffer_pos.x as usize][buffer_pos.y as usize]
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

    fn buffer_character_square_is_on_screen(
        &self,
        buffer_char_pos: Point2D<i32, CharacterGridInScreenBufferFrame>,
    ) -> bool {
        buffer_char_pos.x >= 0
            && buffer_char_pos.x < self.terminal_width as i32
            && buffer_char_pos.y >= 0
            && buffer_char_pos.y < self.terminal_height as i32
    }

    pub fn rotate(&mut self, rotation: QuarterTurnsCcw) {
        let old_center = self.screen_center_as_world_square();
        self.rotation += rotation;
        self.set_screen_center_by_world_square(old_center);
    }
    pub fn set_rotation(&mut self, rotation: QuarterTurnsCcw) {
        let old_center = self.screen_center_as_world_square();
        self.rotation = rotation;
        self.set_screen_center_by_world_square(old_center);
    }
    pub fn rotation(&self) -> QuarterTurnsCcw {
        self.rotation
    }

    ////////////////////////////////////////////////////////////////////////////////
    // CONVERSIONS START
    ////////////////////////////////////////////////////////////////////////////////

    // world point -> world character point -> buffer point -> screen point
    //       ↓                 ↓                    ↓               ↓
    // world square   world_character square   buffer square   screen square

    pub fn world_square_to_left_screen_buffer_character_square(
        &self,
        world_square: WorldSquare,
    ) -> ScreenBufferCharacterSquare {
        let screen_square = self.world_square_to_screen_buffer_square(world_square);
        self.screen_buffer_square_to_left_screen_buffer_character_square(screen_square)
    }

    pub fn world_square_to_both_screen_buffer_character_squares(
        &self,
        world_position: WorldSquare,
    ) -> [ScreenBufferCharacterSquare; 2] {
        let screen_square = self.world_square_to_screen_buffer_square(world_position);
        self.screen_buffer_square_to_both_screen_buffer_character_squares(screen_square)
    }

    pub fn screen_buffer_character_square_to_world_square(
        &self,
        buffer_square: ScreenBufferCharacterSquare,
    ) -> WorldSquare {
        self.screen_buffer_square_to_world_square(
            self.screen_buffer_character_square_to_screen_buffer_square(buffer_square),
        )
    }

    pub fn screen_buffer_character_square_is_left_glyph_of_world_square(
        &self,
        buffer_square: ScreenBufferCharacterSquare,
    ) -> bool {
        self.world_square_to_left_screen_buffer_character_square(
            self.screen_buffer_character_square_to_world_square(buffer_square),
        ) == buffer_square
    }

    pub fn screen_buffer_character_square_is_left_glyph_of_screen_square(
        &self,
        buffer_character_square: ScreenBufferCharacterSquare,
    ) -> bool {
        self.screen_buffer_character_square_is_left_glyph_of_world_square(buffer_character_square)
    }

    pub fn screen_buffer_character_square_to_screen_buffer_square(
        &self,
        screen_character_square: ScreenBufferCharacterSquare,
    ) -> ScreenBufferSquare {
        point2(screen_character_square.x / 2, screen_character_square.y)
    }

    pub fn screen_buffer_square_to_left_screen_buffer_character_square(
        &self,
        screen_buffer_square: ScreenBufferSquare,
    ) -> ScreenBufferCharacterSquare {
        point2(screen_buffer_square.x * 2, screen_buffer_square.y)
    }

    pub fn screen_buffer_square_to_both_screen_buffer_character_squares(
        &self,
        screen_buffer_square: ScreenBufferSquare,
    ) -> [ScreenBufferCharacterSquare; 2] {
        let left_screen_buffer_character_square =
            self.screen_buffer_square_to_left_screen_buffer_character_square(screen_buffer_square);
        [
            left_screen_buffer_character_square,
            left_screen_buffer_character_square + STEP_RIGHT.cast_unit(),
        ]
    }

    pub fn world_step_to_screen_step(&self, world_step: WorldStep) -> ScreenBufferStep {
        let rotated: WorldStep = (-self.rotation()).rotate_vector(world_step);
        let flipped: WorldStep = rotated.flip_y();
        flipped.cast_unit()
    }
    pub fn screen_step_to_world_step(&self, screen_step: ScreenBufferStep) -> WorldStep {
        let flipped: ScreenBufferStep = screen_step.flip_y();
        let rotated: ScreenBufferStep = self.rotation().rotate_vector(flipped);
        rotated.cast_unit()
    }

    pub fn world_square_to_screen_buffer_square(
        &self,
        world_square: WorldSquare,
    ) -> ScreenBufferSquare {
        let world_step_from_screen_center = world_square - self.screen_center_as_world_square();

        // compensate for screen rotation
        let screen_step_from_screen_center =
            self.world_step_to_screen_step(world_step_from_screen_center);

        self.screen_center_as_screen_buffer_square() + screen_step_from_screen_center
    }

    pub fn screen_buffer_square_to_world_square(
        &self,
        screen_square: ScreenBufferSquare,
    ) -> WorldSquare {
        let screen_step_from_center = screen_square - self.screen_center_as_screen_buffer_square();

        let world_step_from_screen_center = self.screen_step_to_world_step(screen_step_from_center);

        self.screen_center_as_world_square() + world_step_from_screen_center
    }

    ////////////////////////////////////////////////////////////////////////////////
    // CONVERSIONS END
    ////////////////////////////////////////////////////////////////////////////////

    pub fn get_screen_glyphs_at_world_square(&self, world_pos: WorldSquare) -> DoubleGlyph {
        let buffer_pos = self.world_square_to_left_screen_buffer_character_square(world_pos);
        [
            self.get_screen_buffered_glyph(buffer_pos).clone(),
            self.get_screen_buffered_glyph(buffer_pos + RIGHT_I.cast_unit())
                .clone(),
        ]
    }
    pub fn get_screen_glyphs_at_visual_offset_from_center(
        &self,
        visual_offset: ScreenBufferStep,
    ) -> DoubleGlyph {
        self.get_glyphs_at_screen_square(
            self.screen_center_as_screen_buffer_square() + visual_offset,
        )
    }

    pub fn all_screen_squares(&self) -> Vec<ScreenBufferSquare> {
        let mut the_squares = vec![];
        for buffer_x in 0..self.terminal_width() {
            for buffer_y in 0..self.terminal_height() {
                let screen_buffer_character_square: ScreenBufferCharacterSquare =
                    point2(buffer_x, buffer_y);
                if !self.screen_buffer_character_square_is_left_glyph_of_screen_square(
                    screen_buffer_character_square,
                ) {
                    continue;
                }
                let screen_square = self.screen_buffer_character_square_to_screen_buffer_square(
                    screen_buffer_character_square,
                );
                the_squares.push(screen_square);
            }
        }
        the_squares
    }

    pub fn get_screen_buffered_glyph(&self, pos: ScreenBufferCharacterSquare) -> &Glyph {
        &self.screen_buffer[pos.x as usize][pos.y as usize]
    }
    pub fn get_glyphs_at_screen_square(&self, square: ScreenBufferSquare) -> DoubleGlyph {
        self.screen_buffer_square_to_both_screen_buffer_character_squares(square)
            .map(|s| *self.get_screen_buffered_glyph(s))
    }

    fn set_buffered_glyph(
        &mut self,
        pos: Point2D<i32, CharacterGridInScreenBufferFrame>,
        new_glyph: Glyph,
    ) {
        self.screen_buffer[pos.x as usize][pos.y as usize] = new_glyph;
    }
    pub fn print_screen_buffer(&self) {
        Self::print_buffer_of_glyphs(&self.screen_buffer);
    }

    pub fn print_buffer_of_glyphs(buffer: &Vec<Vec<Glyph>>) {
        let buffer_width = buffer.len();
        let buffer_height = buffer[0].len();
        for y in 0..buffer_height as usize {
            let mut row_string = String::new();
            for x in 0..buffer_width as usize {
                row_string += &buffer[x][y].to_string();
            }
            row_string += &Glyph::reset_colors();
            if y % 5 == 0 || y == buffer_height as usize - 1 {
                row_string += &format!("――{}", y);
            }
            println!("{}", row_string);
        }

        let mut x_markings = " ".repeat(buffer_width as usize);
        let mut x_numbers = " ".repeat(buffer_width as usize);

        for x in 0..buffer_width as usize {
            if x % 5 == 0 || x == buffer_width as usize - 1 {
                x_markings.insert_str(x, "|");
                x_numbers.insert_str(x, &(x.to_string()));
            }
        }
        println!("{}", x_markings);
        println!("{}", x_numbers);
    }

    pub fn draw_string_to_screen(&mut self, screen_square: ScreenBufferSquare, the_string: &str) {
        for i in 0..the_string.chars().count() {
            let character: char = the_string.chars().nth(i).unwrap();
            let buffer_pos =
                self.screen_buffer_square_to_left_screen_buffer_character_square(screen_square);
            self.screen_buffer[buffer_pos.x as usize + i][buffer_pos.y as usize] =
                Glyph::from_char(character);
        }
    }
    fn draw_glyph_straight_to_screen_buffer(
        &mut self,
        new_glyph: Glyph,
        buffer_square: ScreenBufferCharacterSquare,
    ) {
        if !self.buffer_character_square_is_on_screen(buffer_square) {
            panic!(
                "Tried to draw character off screen: {}",
                buffer_square.to_string()
            );
        }

        self.screen_buffer[buffer_square.x as usize][buffer_square.y as usize] = new_glyph;
    }

    fn draw_glyphs_straight_to_screen_square(
        &mut self,
        glyphs: DoubleGlyph,
        screen_square: ScreenBufferSquare,
    ) {
        let buffer_squares =
            self.screen_buffer_square_to_both_screen_buffer_character_squares(screen_square);
        (0..2)
            .for_each(|i| self.draw_glyph_straight_to_screen_buffer(glyphs[i], buffer_squares[i]));
    }

    pub fn draw_drawable<T: Drawable>(&mut self, drawable: &T, screen_square: ScreenBufferSquare) {
        self.draw_glyphs_straight_to_screen_square(drawable.to_glyphs(), screen_square);
    }

    pub fn fill_screen_buffer(&mut self, color: RGB8) {
        for x in 0..self.terminal_width as usize {
            for y in 0..self.terminal_height as usize {
                self.screen_buffer[x][y] = Glyph::new(' ', WHITE, color);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use ntest::{assert_false, timeout};
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::utility::{STEP_DOWN, STEP_DOWN_LEFT, STEP_LEFT, STEP_RIGHT, STEP_UP, STEP_UP_LEFT};

    use super::*;

    fn set_up_10x10_character_screen() -> Screen {
        let terminal_width = 10;
        let terminal_height = 10;
        Screen::new(terminal_width, terminal_height)
    }

    fn set_up_nxn_character_screen(n: u16) -> Screen {
        Screen::new(n, n)
    }

    fn set_up_nxn_square_screen(n: u16) -> Screen {
        Screen::new(n * 2, n)
    }

    #[test]

    fn test_world_to_screen_buffer() {
        let g = set_up_10x10_character_screen();

        assert_eq!(g.screen_origin_as_world_square(), point2(0, 9));

        assert_eq!(
            g.world_square_to_left_screen_buffer_character_square(point2(0, 0)),
            point2(0, 9)
        );

        assert_eq!(
            g.world_square_to_left_screen_buffer_character_square(point2(
                1,
                g.terminal_height() - 2,
            )),
            point2(2, 1),
        );
    }

    #[test]

    fn test_world_to_screen__should_bias_up_left() {
        let s0 = set_up_nxn_square_screen(5);
        assert_eq!(s0.screen_origin_as_world_square(), point2(0, 4));
        assert_eq!(s0.screen_center_as_world_square(), point2(2, 2));
        assert_eq!(
            s0.world_square_to_screen_buffer_square(point2(0, 0)),
            point2(0, 4)
        );

        let s1 = set_up_nxn_square_screen(19);
        assert_eq!(s1.screen_center_as_world_square(), point2(9, 9));
        assert_eq!(
            s1.world_square_to_screen_buffer_square(point2(0, 0)),
            point2(0, 18)
        );

        let s2 = set_up_nxn_square_screen(20);
        assert_eq!(s2.screen_origin_as_world_square(), point2(0, 19));
        assert_eq!(s2.screen_center_as_world_square(), point2(9, 10));
        assert_eq!(
            s2.world_square_to_screen_buffer_square(point2(0, 0)),
            point2(0, 19)
        );

        let s3 = set_up_nxn_square_screen(21);
        assert_eq!(s3.screen_center_as_world_square(), point2(10, 10));
        assert_eq!(
            s3.world_square_to_screen_buffer_square(point2(0, 0)),
            point2(0, 20)
        );
    }

    #[test]

    fn test_screen_origin_set_get_consistency() {
        let mut s = set_up_nxn_square_screen(23);
        let new_center = point2(24, 89);
        s.set_screen_center_by_world_square(new_center);
        assert_eq!(s.screen_center_as_world_square(), new_center);
    }

    #[test]

    fn test_screen_max_position() {
        let mut s = set_up_nxn_square_screen(5);

        s.set_screen_origin_by_world_square(point2(7, 95));

        assert_eq!(s.screen_max_as_world_square(), point2(11, 91));
    }

    #[test]

    fn test_screen_max_position__with_rotation() {
        let mut s = set_up_nxn_square_screen(5);
        s.set_rotation(QuarterTurnsCcw::new(3));
        s.set_screen_origin_by_world_square(point2(3, 79));

        assert_eq!(s.screen_max_as_world_square(), point2(-1, 75));
    }

    #[test]

    fn test_world_to_screen__with_screen_motion() {
        let mut g = set_up_10x10_character_screen();

        let world_square: WorldSquare = point2(0, 0);
        let screen_pos_1 = g.world_square_to_left_screen_buffer_character_square(world_square);

        g.set_screen_origin_by_world_square(g.screen_origin_as_world_square() + STEP_RIGHT);

        let screen_pos_2 = g.world_square_to_left_screen_buffer_character_square(world_square);

        assert_ne!(screen_pos_1, screen_pos_2);
        assert_eq!(screen_pos_2, screen_pos_1 + STEP_LEFT.cast_unit() * 2);
    }

    #[test]

    fn test_world_square_is_on_screen() {
        let mut g = Screen::new(41, 20);
        let xmax = 20;
        let ymax = 19;

        assert!(
            g.world_square_is_at_least_partially_on_screen(point2(0, 0)),
            "bottom_left"
        );
        assert!(
            !g.world_square_is_at_least_partially_on_screen(point2(-1, 0)),
            "one step left of bottom left"
        );
        assert!(
            !g.world_square_is_at_least_partially_on_screen(point2(0, -1)),
            "one step down of bottom left"
        );

        assert!(
            g.world_square_is_at_least_partially_on_screen(point2(xmax, ymax)),
            "top right of visible board squares.  Partially visible"
        );
        assert!(
            !g.world_square_is_at_least_partially_on_screen(point2(xmax, ymax + 1)),
            "one step up of top right of visible board"
        );
        assert!(
            !g.world_square_is_at_least_partially_on_screen(point2(xmax, -1)),
            "one down of bottom right"
        );
        assert!(
            !g.world_square_is_at_least_partially_on_screen(point2(xmax + 1, ymax)),
            "one right of top right"
        );
    }

    #[test]

    fn test_screen_rotation__square_to_square() {
        let mut s = Screen::new(20, 20);
        let center = point2(3, 5);
        s.set_screen_center_by_world_square(center);
        assert_eq!(s.screen_center_as_world_square(), center);

        assert_eq!(s.rotation(), QuarterTurnsCcw::new(0));
        assert_eq!(
            s.world_square_to_screen_buffer_square(center + STEP_UP * 2),
            point2(4, 7)
        );
        assert_eq!(
            s.screen_buffer_square_to_world_square(point2(0, -2)),
            point2(-1, 16)
        );

        s.rotate(QuarterTurnsCcw::new(1));

        assert_eq!(s.rotation(), QuarterTurnsCcw::new(1));
        assert_eq!(s.screen_center_as_world_square(), center);
        assert_eq!(
            s.world_square_to_screen_buffer_square(center + STEP_UP * 2),
            s.screen_center_as_screen_buffer_square() + SCREEN_STEP_RIGHT * 2
        );
        assert_eq!(
            s.screen_buffer_square_to_world_square(
                s.screen_center_as_screen_buffer_square() + SCREEN_STEP_RIGHT * 2
            ),
            center + STEP_UP * 2
        );
        s.rotate(QuarterTurnsCcw::new(6));

        assert_eq!(s.rotation(), QuarterTurnsCcw::new(3));
        assert_eq!(s.screen_center_as_world_square(), center);
        assert_eq!(
            s.world_square_to_screen_buffer_square(center + STEP_LEFT),
            s.screen_center_as_screen_buffer_square() + SCREEN_STEP_DOWN
        );
        assert_eq!(
            s.screen_buffer_square_to_world_square(
                s.screen_center_as_screen_buffer_square() + SCREEN_STEP_DOWN
            ),
            point2(2, 5)
        );
    }

    #[test]

    fn test_world_square_is_on_screen__with_screen_rotation() {
        let mut s = Screen::new_by_square_dimensions(100, 5);
        s.set_screen_center_by_world_square(point2(300, 20));
        assert_eq!(s.screen_center_as_world_square(), point2(300, 20));

        assert!(s.world_square_is_at_least_partially_on_screen(point2(340, 22)));
        assert_false!(s.world_square_is_at_least_partially_on_screen(point2(370, 20)));
        assert_false!(s.world_square_is_at_least_partially_on_screen(point2(300, 26)));

        s.set_rotation(QuarterTurnsCcw::new(1));
        assert_eq!(s.screen_center_as_world_square(), point2(300, 20));

        assert_false!(s.world_square_is_at_least_partially_on_screen(point2(340, 22)));
        assert!(s.world_square_is_at_least_partially_on_screen(point2(302, -20)));
        assert!(s.world_square_is_at_least_partially_on_screen(point2(300, 26)));
    }

    #[test]

    fn test_screen_rotation_pivots_around_center_square() {
        let mut s = Screen::new(20, 20);
        let center = point2(3, 5);
        s.set_screen_center_by_world_square(center);
        let start_origin = s.screen_origin_as_world_square();

        assert_eq!(s.rotation(), QuarterTurnsCcw::new(0));
        assert_eq!(s.screen_center_as_world_square(), center);
        assert_eq!(s.screen_origin_as_world_square(), start_origin);
        (0..3).for_each(|i| {
            s.rotate(QuarterTurnsCcw::new(1));

            assert_eq!(s.rotation(), QuarterTurnsCcw::new(i + 1));
            assert_eq!(s.screen_center_as_world_square(), center);
            assert_ne!(s.screen_origin_as_world_square(), start_origin);
        });
    }
}
