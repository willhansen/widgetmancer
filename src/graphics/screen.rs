use std::collections::HashMap;

use euclid::{point2, vec2, Point2D, Vector2D};

use crate::glyph::{DoubleGlyph, Glyph};
use crate::utility::coordinate_frame_conversions::{
    world_character_square_to_world_square, world_point_to_world_character_point,
    world_square_to_left_world_character_square, CharacterGridInWorldFrame, SquareGridInWorldFrame,
    SquareSet, WorldCharacterPoint, WorldCharacterSquare, WorldCharacterStep, WorldPoint,
    WorldSquare, WorldStep,
};
use crate::utility::{
    get_by_point, point_to_string, QuarterTurnsAnticlockwise, RIGHT_I, STEP_RIGHT, STEP_UP,
};

#[deprecated(note = "Screen square origin should be screen character square origin")]
#[derive(Clone, PartialEq, Debug, Copy)]
pub struct SquareGridInCenteredScreenBufferFrame;

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct CharacterGridInScreenBufferFrame;

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct SquareGridInScreenBufferFrame;

#[deprecated(note = "Avoid using the raw screen frame, use the screen buffer frame instead")]
#[derive(Clone, PartialEq, Debug, Copy)]
pub struct CharacterGridInScreenFrame;

#[deprecated(note = "Screen square origin should be screen character square origin")]
pub type CenteredScreenBufferSquare = Point2D<i32, SquareGridInCenteredScreenBufferFrame>;
#[deprecated(note = "Screen square origin should be screen character square origin")]
pub type CenteredScreenBufferStep = Vector2D<i32, SquareGridInCenteredScreenBufferFrame>;

pub type ScreenBufferCharacterSquare = Point2D<i32, CharacterGridInScreenBufferFrame>;
pub type ScreenBufferCharacterStep = Vector2D<i32, CharacterGridInScreenBufferFrame>;

pub type ScreenBufferSquare = Point2D<i32, SquareGridInScreenBufferFrame>;
pub type ScreenBufferStep = Vector2D<i32, SquareGridInScreenBufferFrame>;

#[deprecated(note = "Screen should not care about points")]
pub type ScreenBufferCharacterPoint = Point2D<f32, CharacterGridInScreenBufferFrame>;

#[deprecated(note = "Avoid using the raw screen frame, use the screen buffer frame instead")]
pub type ScreenCharacterSquare = Point2D<i32, CharacterGridInScreenFrame>;
#[deprecated(note = "Avoid using the raw screen frame, use the screen buffer frame instead")]
pub type ScreenCharacterPoint = Point2D<f32, CharacterGridInScreenFrame>;

pub type ScreenBufferGlyphMap = HashMap<ScreenBufferCharacterSquare, Glyph>;

pub struct Screen {
    screen_origin: WorldSquare,
    rotation: QuarterTurnsAnticlockwise,
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
            rotation: QuarterTurnsAnticlockwise::default(),
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
    pub(crate) fn terminal_width(&self) -> i32 {
        self.terminal_width as i32
    }

    pub(crate) fn terminal_height(&self) -> i32 {
        self.terminal_height as i32
    }

    #[deprecated(note = "Invalidated by screen rotation")]
    pub fn screen_origin_as_world_character_square(&self) -> WorldCharacterSquare {
        self.screen_center_as_world_character_square()
            - self.world_character_step_from_origin_to_center()
    }

    #[deprecated(note = "Invalidated by screen rotation")]
    fn set_screen_center_by_world_character_square(
        &mut self,
        world_character_square: WorldCharacterSquare,
    ) {
        self.set_screen_center_by_world_square(world_character_square_to_world_square(
            world_character_square,
        ))
    }

    #[deprecated(note = "Invalidated by screen rotation")]
    fn set_screen_origin_by_world_character_square(
        &mut self,
        world_character_square: WorldCharacterSquare,
    ) {
        let new_center = world_character_square - self.world_character_step_from_origin_to_center();
        self.set_screen_center_by_world_character_square(new_center)
    }

    fn set_screen_origin_by_world_square(&mut self, world_square: WorldSquare) {
        let world_character_square = world_square_to_left_world_character_square(world_square);
        self.set_screen_origin_by_world_character_square(world_character_square);
    }

    pub fn set_screen_center_by_world_square(&mut self, world_square: WorldSquare) {
        self.screen_origin = world_square - self.world_step_from_origin_to_center();
    }

    fn screen_max_as_world_character_square(&self) -> WorldCharacterSquare {
        self.screen_origin_as_world_character_square()
            + self.world_character_step_from_origin_to_max()
    }
    fn world_character_step_from_origin_to_max(&self) -> WorldCharacterStep {
        let unrotated = self
            .screen_buffer_character_step_from_origin_to_max()
            .reflect(STEP_UP.cast_unit())
            .cast_unit();
        self.rotation.rotate_vector(unrotated)
    }
    fn world_step_from_origin_to_max(&self) -> WorldStep {
        let unrotated = self
            .screen_step_from_origin_to_max()
            .reflect(STEP_UP.cast_unit())
            .cast_unit();
        self.rotation.rotate_vector(unrotated)
    }

    fn world_character_step_from_origin_to_center(&self) -> WorldCharacterStep {
        self.world_character_step_from_origin_to_max() / 2
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

    fn screen_center_as_world_character_square(&self) -> WorldCharacterSquare {
        world_square_to_left_world_character_square(self.screen_center_as_world_square())
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
            .to_point()
    }

    fn screen_center_as_screen_buffer_square(&self) -> ScreenBufferSquare {
        self.screen_buffer_step_from_origin_to_center().to_point()
    }

    fn world_character_square_is_on_screen(&self, character_square: WorldCharacterSquare) -> bool {
        dbg!("asdfasdf", character_square);
        self.world_square_is_on_screen(world_character_square_to_world_square(character_square))
    }

    pub fn world_square_is_on_screen(&self, square: WorldSquare) -> bool {
        dbg!(
            "asdfasdf",
            square,
            self.world_square_to_both_screen_buffer_character_squares(square)
        );
        self.world_square_to_both_screen_buffer_character_squares(square)
            .into_iter()
            .all(|buffer_square| self.buffer_character_square_is_on_screen(buffer_square))
    }

    fn buffer_character_square_is_on_screen(
        &self,
        buffer_char_pos: Point2D<i32, CharacterGridInScreenBufferFrame>,
    ) -> bool {
        return buffer_char_pos.x >= 0
            && buffer_char_pos.x < self.terminal_width as i32
            && buffer_char_pos.y >= 0
            && buffer_char_pos.y < self.terminal_height as i32;
    }

    pub fn rotate(&mut self, rotation: QuarterTurnsAnticlockwise) {
        self.rotation += rotation;
    }
    pub fn set_rotation(&mut self, rotation: QuarterTurnsAnticlockwise) {
        self.rotation = rotation;
    }
    pub fn rotation(&self) -> QuarterTurnsAnticlockwise {
        self.rotation
    }

    ////////////////////////////////////////////////////////////////////////////////
    // CONVERSIONS START
    ////////////////////////////////////////////////////////////////////////////////

    // world point -> world character point -> buffer point -> screen point
    //       ↓                 ↓                    ↓               ↓
    // world square   world_character square   buffer square   screen square

    #[deprecated(note = "Invalidated by screen rotation")]
    pub fn world_character_square_to_screen_buffer_character_square(
        &self,
        world_character_square: WorldCharacterSquare,
    ) -> ScreenBufferCharacterSquare {
        self.world_square_to_left_screen_buffer_character_square(
            world_character_square_to_world_square(world_character_square),
        )
    }
    #[deprecated(note = "Invalidated by screen rotation")]
    pub fn screen_buffer_character_square_to_world_character_square(
        &self,
        screen_buffer_character_square: ScreenBufferCharacterSquare,
    ) -> WorldCharacterSquare {
        world_square_to_left_world_character_square(
            self.screen_buffer_character_square_to_world_square(screen_buffer_character_square),
        )
    }

    pub fn world_square_to_left_screen_buffer_character_square(
        &self,
        world_square: WorldSquare,
    ) -> ScreenBufferCharacterSquare {
        let screen_square = self.world_square_to_screen_buffer_square(world_square);
        dbg!(
            "asdfasdf B",
            world_square,
            screen_square,
            self.screen_center_as_world_square(),
            self.screen_origin_as_world_character_square()
        );
        self.screen_buffer_square_to_left_screen_buffer_character_square(screen_square)
    }

    pub fn world_square_to_both_screen_buffer_character_squares(
        &self,
        world_position: WorldSquare,
    ) -> [ScreenBufferCharacterSquare; 2] {
        let screen_square = self.world_square_to_screen_buffer_square(world_position);
        dbg!(
            "asdfasdf",
            world_position,
            screen_square,
            self.screen_center_as_world_square(),
            self.screen_buffer_square_to_both_screen_buffer_character_squares(screen_square)
        );
        self.screen_buffer_square_to_both_screen_buffer_character_squares(screen_square)
    }

    pub fn screen_buffer_character_square_to_world_square(
        &self,
        buffer_square: ScreenBufferCharacterSquare,
    ) -> WorldSquare {
        self.centered_screen_buffer_square_to_world_square(
            self.screen_buffer_character_square_to_centered_screen_buffer_square(buffer_square),
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

    #[deprecated(note = "Avoid using the raw screen frame, use the screen buffer frame instead")]
    pub fn screen_buffer_character_point_to_screen_character_point(
        buffer_point: ScreenBufferCharacterPoint,
    ) -> ScreenCharacterPoint {
        // Buffer indexes from 0
        // Screen indexes from 1
        point2(buffer_point.x + 1.0, buffer_point.y + 1.0)
    }

    #[deprecated(note = "Avoid using the raw screen frame, use the screen buffer frame instead")]
    pub fn screen_buffer_character_square_to_screen_character_square(
        &self,
        buffer_pos: Point2D<i32, CharacterGridInScreenBufferFrame>,
    ) -> Point2D<i32, CharacterGridInScreenFrame> {
        // Terminal indexes from 1, and the y axis goes top to bottom
        // The screen buffer indexes from 0, but the y axis also goes top to bottom
        // End result is just an off-by-one offset on both axes
        point2(buffer_pos.x + 1, buffer_pos.y + 1)
    }

    #[deprecated(note = "Avoid using the raw screen frame, use the screen buffer frame instead")]
    pub fn screen_character_square_to_buffer_character_square(
        &self,
        screen_pos: Point2D<i32, CharacterGridInScreenFrame>,
    ) -> Point2D<i32, CharacterGridInScreenBufferFrame> {
        // Terminal indexes from 1, and the y axis goes top to bottom
        // The screen buffer indexes from 0, but the y axis also goes top to bottom
        // End result is just an off-by-one offset on both axes
        point2(screen_pos.x - 1, screen_pos.y - 1)
    }

    #[deprecated(note = "Avoid using the raw screen frame, use the screen buffer frame instead")]
    pub fn world_square_to_left_screen_character_square(
        &self,
        world_position: WorldSquare,
    ) -> Point2D<i32, CharacterGridInScreenFrame> {
        // terminal indexes from 1, and the y axis goes top to bottom
        // world indexes from 0, origin at bottom left
        let world_char_square = world_square_to_left_world_character_square(world_position);
        let buffer_square =
            self.world_character_square_to_screen_buffer_character_square(world_char_square);
        self.screen_buffer_character_square_to_screen_character_square(buffer_square)
    }

    pub fn screen_buffer_character_square_to_centered_screen_buffer_square(
        &self,
        screen_character_square: ScreenBufferCharacterSquare,
    ) -> CenteredScreenBufferSquare {
        let character_step_from_screen_center =
            screen_character_square - self.screen_center_as_screen_buffer_character_square();
        point2(
            character_step_from_screen_center.x / 2,
            character_step_from_screen_center.y,
        )
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

    pub fn world_square_to_screen_buffer_square(
        &self,
        world_square: WorldSquare,
    ) -> ScreenBufferSquare {
        let step_in_world_from_screen_center = world_square - self.screen_center_as_world_square();

        let screen_rotation = self.rotation();
        dbg!(
            "asdfasdf A",
            screen_rotation,
            step_in_world_from_screen_center
        );

        // compensate for screen rotation
        let step_in_screen_from_screen_center = (-screen_rotation)
            .rotate_vector(step_in_world_from_screen_center)
            .cast_unit();

        let screen_square = point2(0, 0)
            + self.screen_buffer_step_from_origin_to_center()
            + step_in_screen_from_screen_center;
        screen_square
    }

    pub fn screen_buffer_square_to_world_square(
        &self,
        screen_square: ScreenBufferSquare,
    ) -> WorldSquare {
        let screen_step_from_center = screen_square - self.screen_center_as_screen_buffer_square();
        self.screen_center_as_world_square()
            + self
                .rotation()
                .rotate_vector(screen_step_from_center)
                .cast_unit()
    }

    ////////////////////////////////////////////////////////////////////////////////
    // CONVERSIONS END
    ////////////////////////////////////////////////////////////////////////////////

    pub fn get_glyphs_for_square_from_screen_buffer(&self, world_pos: WorldSquare) -> DoubleGlyph {
        let buffer_pos = self.world_square_to_left_screen_buffer_character_square(world_pos);
        [
            self.get_screen_buffered_glyph(buffer_pos).clone(),
            self.get_screen_buffered_glyph(buffer_pos + RIGHT_I.cast_unit())
                .clone(),
        ]
    }

    pub fn get_screen_buffered_glyph(&self, pos: ScreenBufferCharacterSquare) -> &Glyph {
        return &self.screen_buffer[pos.x as usize][pos.y as usize];
    }

    fn set_buffered_glyph(
        &mut self,
        pos: Point2D<i32, CharacterGridInScreenBufferFrame>,
        new_glyph: Glyph,
    ) {
        self.screen_buffer[pos.x as usize][pos.y as usize] = new_glyph;
    }
    pub fn print_screen_buffer(&self) {
        for y in 0..self.terminal_height() as usize {
            let mut row_string = String::new();
            for x in 0..self.terminal_width() as usize {
                row_string += &self.screen_buffer[x][y].to_string();
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
    pub fn draw_string_to_screen(
        &mut self,
        screen_pos: Point2D<i32, CharacterGridInScreenFrame>,
        the_string: &str,
    ) {
        for i in 0..the_string.chars().count() {
            let character: char = the_string.chars().nth(i).unwrap();
            let buffer_pos = self.screen_character_square_to_buffer_character_square(screen_pos);
            self.screen_buffer[buffer_pos.x as usize + i][buffer_pos.y as usize] =
                Glyph::from_char(character);
        }
    }
    pub fn draw_glyph_straight_to_screen_buffer(
        &mut self,
        new_glyph: Glyph,
        buffer_square: ScreenBufferCharacterSquare,
    ) {
        if !self.buffer_character_square_is_on_screen(buffer_square) {
            panic!(
                "Tried to draw character off screen: {}",
                point_to_string(buffer_square)
            );
        }

        self.screen_buffer[buffer_square.x as usize][buffer_square.y as usize] = new_glyph;
    }

    pub fn draw_glyphs_straight_to_screen_square(
        &mut self,
        glyphs: DoubleGlyph,
        screen_square: CenteredScreenBufferSquare,
    ) {
        let buffer_squares =
            self.centered_screen_buffer_square_to_screen_buffer_character_squares(screen_square);
        (0..2)
            .for_each(|i| self.draw_glyph_straight_to_screen_buffer(glyphs[i], buffer_squares[i]));
    }

    #[deprecated(note = "Avoid using the raw screen frame, use the screen buffer frame instead")]
    pub fn get_char_at_screen_pos(
        &self,
        screen_pos: Point2D<i32, CharacterGridInScreenFrame>,
    ) -> char {
        let buffer_pos = self.screen_character_square_to_buffer_character_square(screen_pos);
        get_by_point(&self.current_screen_state, buffer_pos).character
    }
    pub fn all_squares_on_screen(&self) -> SquareSet {
        let mut all_squares = SquareSet::new();
        for buffer_x in 0..self.terminal_width() {
            for buffer_y in 0..self.terminal_height() {
                let buffer_square: Point2D<i32, CharacterGridInScreenBufferFrame> =
                    point2(buffer_x, buffer_y);
                let world_square =
                    self.screen_buffer_character_square_to_world_square(buffer_square);
                if self.world_square_is_on_screen(world_square) {
                    all_squares.insert(world_square);
                }
            }
        }
        all_squares
    }
}

#[cfg(test)]
mod tests {
    use ntest::assert_false;
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::utility::{STEP_DOWN_LEFT, STEP_LEFT, STEP_RIGHT, STEP_UP, STEP_UP_LEFT};

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

        assert_eq!(
            g.world_square_to_left_screen_buffer_character_square(point2(0, 0)),
            point2(0, 9)
        );

        assert_eq!(
            g.world_square_to_left_screen_buffer_character_square(point2(
                1,
                g.terminal_height() - 2
            )),
            point2(1, 1),
        );
    }

    #[test]
    fn test_world_to_screen__should_bias_up_left() {
        let s0 = set_up_nxn_square_screen(5);
        assert_eq!(s0.screen_origin_as_world_character_square(), point2(0, 4));
        assert_eq!(s0.screen_center_as_world_character_square(), point2(4, 2));
        assert_eq!(s0.screen_center_as_world_square(), point2(2, 2));
        assert_eq!(
            s0.world_square_to_centered_screen_buffer_square(point2(0, 0)),
            point2(-2, -2)
        );

        let s1 = set_up_nxn_square_screen(19);
        assert_eq!(s1.screen_center_as_world_character_square(), point2(18, 9));
        assert_eq!(s1.screen_center_as_world_square(), point2(9, 9));
        assert_eq!(
            s1.world_square_to_centered_screen_buffer_square(point2(0, 0)),
            point2(-9, -9)
        );

        let s2 = set_up_nxn_square_screen(20);
        assert_eq!(s2.screen_origin_as_world_character_square(), point2(0, 19));
        assert_eq!(s2.screen_center_as_world_character_square(), point2(19, 10));
        assert_eq!(s2.screen_center_as_world_square(), point2(9, 10));
        assert_eq!(
            s2.world_square_to_centered_screen_buffer_square(point2(0, 0)),
            point2(-9, -10)
        );

        let s3 = set_up_nxn_square_screen(21);
        assert_eq!(s3.screen_center_as_world_square(), point2(10, 10));
        assert_eq!(
            s3.world_square_to_centered_screen_buffer_square(point2(0, 0)),
            point2(-10, -10)
        );
    }

    #[test]
    fn test_screen_origin_set_get_consistency() {
        let mut s = set_up_nxn_square_screen(23);
        let new_center = point2(24, 89);
        s.set_screen_center_by_world_character_square(new_center);
        assert_eq!(s.screen_center_as_world_character_square(), new_center);
    }

    #[test]
    fn test_screen_max_position() {
        let mut s = set_up_nxn_square_screen(5);
        s.set_screen_origin_by_world_character_square(point2(3, 79));

        assert_eq!(s.screen_max_as_world_character_square(), point2(12, 75));
    }

    #[test]
    fn test_screen_max_position__with_rotation() {
        let mut s = set_up_nxn_square_screen(5);
        s.set_rotation(QuarterTurnsAnticlockwise::new(3));
        s.set_screen_origin_by_world_character_square(point2(3, 79));

        assert_eq!(s.screen_max_as_world_character_square(), point2(-1, 70));
    }

    #[test]
    fn test_world_to_screen__with_screen_motion() {
        let mut g = set_up_10x10_character_screen();

        let world_square: WorldSquare = point2(0, 0);
        let screen_character_square: ScreenCharacterSquare = point2(1, 5);
        let screen_pos_1 = g.world_square_to_left_screen_buffer_character_square(world_square);

        g.set_screen_origin_by_world_character_square(
            g.screen_origin_as_world_character_square() + STEP_RIGHT.cast_unit(),
        );

        let screen_pos_2 = g.world_square_to_left_screen_buffer_character_square(world_square);

        assert_ne!(screen_pos_1, screen_pos_2);
        assert_eq!(screen_pos_2, screen_pos_1 + STEP_LEFT.cast_unit());
    }

    #[test]
    fn test_world_character_is_on_screen() {
        let mut g = Screen::new(41, 20);

        assert!(
            g.world_character_square_is_on_screen(point2(0, 0)),
            "bottom_left"
        );
        assert!(
            !g.world_character_square_is_on_screen(point2(-1, 0)),
            "one step left of bottom left"
        );
        dbg!("asdfasdf", g.screen_origin_as_world_character_square());
        assert!(
            !g.world_character_square_is_on_screen(point2(0, -1)),
            "one step down of bottom left"
        );

        assert!(
            g.world_character_square_is_on_screen(point2(39, 19)),
            "top right of visible board squares"
        );
        assert!(
            !g.world_character_square_is_on_screen(point2(40, 19)),
            "top right of terminal, but on cut-off square"
        );
        assert!(
            !g.world_character_square_is_on_screen(point2(39, 20)),
            "one step up of top right of visible board"
        );
        assert!(
            !g.world_character_square_is_on_screen(point2(40, 20)),
            "one step up of top right of screen"
        );
        assert!(
            !g.world_character_square_is_on_screen(point2(41, 19)),
            "one step right of top right of screen"
        );
    }

    #[test]
    fn test_screen_rotation__square_to_square() {
        let mut s = Screen::new(20, 20);
        s.set_screen_center_by_world_square(point2(3, 5));

        assert_eq!(s.rotation(), QuarterTurnsAnticlockwise::new(0));
        assert_eq!(
            s.world_square_to_centered_screen_buffer_square(point2(3, 7)),
            point2(0, 2)
        );
        assert_eq!(
            s.centered_screen_buffer_square_to_world_square(point2(0, 2)),
            point2(3, 7)
        );

        s.rotate(QuarterTurnsAnticlockwise::new(1));

        assert_eq!(s.rotation(), QuarterTurnsAnticlockwise::new(1));
        assert_eq!(
            s.world_square_to_centered_screen_buffer_square(point2(3, 7)),
            point2(2, 0)
        );
        assert_eq!(
            s.centered_screen_buffer_square_to_world_square(point2(2, 0)),
            point2(3, 7)
        );
        s.rotate(QuarterTurnsAnticlockwise::new(6));

        assert_eq!(s.rotation(), QuarterTurnsAnticlockwise::new(3));
        assert_eq!(
            s.world_square_to_centered_screen_buffer_square(point2(2, 5)),
            point2(0, -1)
        );
        assert_eq!(
            s.centered_screen_buffer_square_to_world_square(point2(0, -1)),
            point2(2, 5)
        );
    }

    #[test]
    fn test_world_character_is_on_screen__with_screen_rotation() {
        let mut s = Screen::new(100, 5);
        s.set_screen_center_by_world_square(point2(300, 20));
        assert!(s.world_square_is_on_screen(point2(320, 22)));
        assert_false!(s.world_square_is_on_screen(point2(330, 20)));
        assert_false!(s.world_square_is_on_screen(point2(300, 26)));

        s.set_rotation(QuarterTurnsAnticlockwise::new(1));
        assert_false!(s.world_square_is_on_screen(point2(320, 22)));
        assert!(s.world_square_is_on_screen(point2(330, 20)));
        assert!(s.world_square_is_on_screen(point2(300, 26)));
    }

    #[test]
    fn test_screen_rotation_pivots_around_center_square() {
        let mut s = Screen::new(20, 20);
        let center = point2(3, 5);
        s.set_screen_center_by_world_square(center);
        let start_origin = s.screen_origin_as_world_character_square();

        assert_eq!(s.rotation(), QuarterTurnsAnticlockwise::new(0));
        assert_eq!(s.screen_center_as_world_square(), center);
        assert_eq!(s.screen_origin_as_world_character_square(), start_origin);
        (0..3).for_each(|i| {
            s.rotate(QuarterTurnsAnticlockwise::new(1));

            assert_eq!(s.rotation(), QuarterTurnsAnticlockwise::new(i + 1));
            assert_eq!(s.screen_center_as_world_square(), center);
            assert_ne!(s.screen_origin_as_world_character_square(), start_origin);
        });
    }
}
