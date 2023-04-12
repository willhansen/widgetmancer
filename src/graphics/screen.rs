use crate::glyph::{DoubleGlyph, Glyph};
use crate::utility::coordinate_frame_conversions::{
    world_character_square_to_world_square, world_point_to_world_character_point,
    world_square_to_left_world_character_square, CharacterGridInWorldFrame, SquareGridInWorldFrame,
    SquareSet, WorldCharacterPoint, WorldCharacterSquare, WorldCharacterStep, WorldPoint,
    WorldSquare,
};
use crate::utility::{get_by_point, point_to_string, QuarterTurnsAnticlockwise, RIGHT_I};
use euclid::{point2, Point2D, Vector2D};
use std::collections::HashMap;

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct SquareGridInCenteredScreenFrame;

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct CharacterGridInScreenBufferFrame;
#[derive(Clone, PartialEq, Debug, Copy)]
pub struct CharacterGridInScreenFrame;

pub type ScreenSquare = Point2D<i32, SquareGridInCenteredScreenFrame>;

pub type ScreenBufferCharacterSquare = Point2D<i32, CharacterGridInScreenBufferFrame>;
pub type BufferCharacterPoint = Point2D<f32, CharacterGridInScreenBufferFrame>;
pub type BufferCharacterStep = Vector2D<i32, CharacterGridInScreenBufferFrame>;

pub type ScreenCharacterSquare = Point2D<i32, CharacterGridInScreenFrame>;
pub type ScreenCharacterPoint = Point2D<f32, CharacterGridInScreenFrame>;

pub type BufferGlyphMap = HashMap<ScreenBufferCharacterSquare, Glyph>;

pub struct Screen {
    pub screen_origin: WorldCharacterSquare,
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

    fn set_screen_center(&mut self, world_character_square: WorldCharacterSquare) {
        self.screen_origin = point2(
            world_character_square.x - self.terminal_width() / 2,
            world_character_square.y + self.terminal_height() / 2,
        );
    }
    fn set_screen_origin(&mut self, world_character_square: WorldCharacterSquare) {
        self.screen_origin = world_character_square;
    }

    pub fn set_screen_center_by_world_square(&mut self, world_square: WorldSquare) {
        self.set_screen_center(world_square_to_left_world_character_square(world_square))
    }

    fn screen_center(&self) -> WorldCharacterSquare {
        point2(
            self.screen_origin.x + self.terminal_width() / 2,
            self.screen_origin.y - self.terminal_height() / 2,
        )
    }

    pub fn screen_center_world_square(&self) -> WorldSquare {
        world_character_square_to_world_square(self.screen_center())
    }

    pub fn screen_center_in_screen_space(&self) -> ScreenBufferCharacterSquare {
        point2(self.terminal_width() / 2, self.terminal_height() / 2)
    }

    fn world_character_square_is_on_screen(&self, character_square: WorldCharacterSquare) -> bool {
        self.square_is_on_screen(world_character_square_to_world_square(character_square))
    }

    pub fn square_is_on_screen(&self, square: WorldSquare) -> bool {
        self.world_square_to_multiple_buffer_squares(square)
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

    pub fn world_character_point_to_buffer_point(
        &self,
        world_character_point: WorldCharacterPoint,
    ) -> BufferCharacterPoint {
        // buffer indexes from 0, and the y axis goes top to bottom
        // world indexes from 0, origin at bottom left
        // origin of buffer is a world character point
        let buffer_origin_point = self.screen_origin.to_f32();

        let new_x = world_character_point.x - buffer_origin_point.x;
        let new_unflipped_y = world_character_point.y - buffer_origin_point.y;
        let new_y = -new_unflipped_y;

        let new_point = point2(new_x, new_y);

        new_point
    }
    pub fn buffer_point_to_world_character_point(
        &self,
        buffer_point: BufferCharacterPoint,
    ) -> WorldCharacterPoint {
        // buffer indexes from 0, and the y axis goes top to bottom
        // world indexes from 0, origin at bottom left
        // origin of buffer is a world character point

        let buffer_origin_point = self.screen_origin.to_f32();

        point2(
            buffer_point.x + buffer_origin_point.x,
            -buffer_point.y + buffer_origin_point.y,
        )
    }

    pub fn world_character_square_to_screen_buffer_character_square(
        &self,
        world_character_square: Point2D<i32, CharacterGridInWorldFrame>,
    ) -> Point2D<i32, CharacterGridInScreenBufferFrame> {
        self.world_character_point_to_buffer_point(world_character_square.to_f32())
            .round()
            .to_i32()
    }
    pub fn buffer_square_to_world_character_square(
        &self,
        buffer_square: ScreenBufferCharacterSquare,
    ) -> WorldCharacterSquare {
        self.buffer_point_to_world_character_point(buffer_square.to_f32())
            .round()
            .to_i32()
    }
    pub fn screen_buffer_step_to_world_character_step(
        &self,
        buffer_step: BufferCharacterStep,
    ) -> WorldCharacterStep {
        let buffer_square_relative_to_zero = buffer_step.to_point();
        let world_character_square_relative_to_zero =
            self.buffer_square_to_world_character_square(buffer_square_relative_to_zero);
        let world_character_square_of_buffer_zero =
            self.buffer_square_to_world_character_square(point2(0, 0));
        world_character_square_relative_to_zero - world_character_square_of_buffer_zero
    }

    pub fn world_square_to_left_screen_buffer_square(
        &self,
        world_position: WorldSquare,
    ) -> ScreenBufferCharacterSquare {
        self.screen_square_to_buffer_square(
            self.world_square_to_left_screen_character_square(world_position),
        )
    }

    pub fn world_square_to_multiple_buffer_squares(
        &self,
        world_position: WorldSquare,
    ) -> [ScreenBufferCharacterSquare; 2] {
        let left_square = self.screen_square_to_buffer_square(
            self.world_square_to_left_screen_character_square(world_position),
        );
        let right_square = left_square + RIGHT_I.cast_unit();
        [left_square, right_square]
    }

    pub fn buffer_square_to_world_square(
        &self,
        buffer_square: ScreenBufferCharacterSquare,
    ) -> WorldSquare {
        world_character_square_to_world_square(
            self.buffer_square_to_world_character_square(buffer_square),
        )
    }

    pub fn is_buffer_character_square_left_glyph_of_world_square(
        &self,
        buffer_square: ScreenBufferCharacterSquare,
    ) -> bool {
        self.world_square_to_left_screen_buffer_square(
            self.buffer_square_to_world_square(buffer_square),
        ) == buffer_square
    }

    pub fn world_point_to_buffer_point(&self, world_point: WorldPoint) -> BufferCharacterPoint {
        self.world_character_point_to_buffer_point(world_point_to_world_character_point(
            world_point,
        ))
    }

    pub fn buffer_point_to_screen_point(
        buffer_point: BufferCharacterPoint,
    ) -> ScreenCharacterPoint {
        // Buffer indexes from 0
        // Screen indexes from 1
        point2(buffer_point.x + 1.0, buffer_point.y + 1.0)
    }

    pub fn screen_buffer_square_to_screen_square(
        &self,
        buffer_pos: Point2D<i32, CharacterGridInScreenBufferFrame>,
    ) -> Point2D<i32, CharacterGridInScreenFrame> {
        // Terminal indexes from 1, and the y axis goes top to bottom
        // The screen buffer indexes from 0, but the y axis also goes top to bottom
        // End result is just an off-by-one offset on both axes
        point2(buffer_pos.x + 1, buffer_pos.y + 1)
    }

    pub fn screen_square_to_buffer_square(
        &self,
        screen_pos: Point2D<i32, CharacterGridInScreenFrame>,
    ) -> Point2D<i32, CharacterGridInScreenBufferFrame> {
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
        Self::buffer_point_to_screen_point(self.world_character_point_to_buffer_point(
            world_point_to_world_character_point(world_position),
        ))
    }

    pub fn world_square_to_left_screen_character_square(
        &self,
        world_position: WorldSquare,
    ) -> Point2D<i32, CharacterGridInScreenFrame> {
        // terminal indexes from 1, and the y axis goes top to bottom
        // world indexes from 0, origin at bottom left
        let world_char_square = world_square_to_left_world_character_square(world_position);
        let buffer_square =
            self.world_character_square_to_screen_buffer_character_square(world_char_square);
        self.screen_buffer_square_to_screen_square(buffer_square)
    }

    pub fn screen_character_square_to_screen_square(
        &self,
        screen_character_square: ScreenBufferCharacterSquare,
    ) -> ScreenSquare {
        let character_step_from_screen_center =
            screen_character_square - self.screen_center_in_screen_space();
        point2(
            character_step_from_screen_center.x / 2,
            character_step_from_screen_center.y,
        )
    }

    pub fn world_square_to_screen_square(&self, world_square: WorldSquare) -> ScreenSquare {
        let screen_character_sqare = self.world_square_to_left_screen_buffer_square(world_square);
        self.screen_character_square_to_screen_square(screen_character_sqare)
    }

    ////////////////////////////////////////////////////////////////////////////////
    // CONVERSIONS END
    ////////////////////////////////////////////////////////////////////////////////

    pub fn get_glyphs_for_square_from_screen_buffer(&self, world_pos: WorldSquare) -> DoubleGlyph {
        let buffer_pos = self.world_square_to_left_screen_buffer_square(world_pos);
        [
            self.get_screen_buffered_glyph(buffer_pos).clone(),
            self.get_screen_buffered_glyph(buffer_pos + RIGHT_I.cast_unit())
                .clone(),
        ]
    }

    pub fn get_screen_buffered_glyph(
        &self,
        pos: Point2D<i32, CharacterGridInScreenBufferFrame>,
    ) -> &Glyph {
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
            let buffer_pos = self.screen_square_to_buffer_square(screen_pos);
            self.screen_buffer[buffer_pos.x as usize + i][buffer_pos.y as usize] =
                Glyph::from_char(character);
        }
    }
    pub fn draw_glyph_straight_to_screen_buffer(
        &mut self,
        buffer_square: ScreenBufferCharacterSquare,
        new_glyph: Glyph,
    ) {
        if !self.buffer_character_square_is_on_screen(buffer_square) {
            panic!(
                "Tried to draw character off screen: {}",
                point_to_string(buffer_square)
            );
        }

        self.screen_buffer[buffer_square.x as usize][buffer_square.y as usize] = new_glyph;
    }
    pub fn get_char_at_screen_pos(
        &self,
        screen_pos: Point2D<i32, CharacterGridInScreenFrame>,
    ) -> char {
        let buffer_pos = self.screen_square_to_buffer_square(screen_pos);
        get_by_point(&self.current_screen_state, buffer_pos).character
    }
    pub fn all_squares_on_screen(&self) -> SquareSet {
        let mut all_squares = SquareSet::new();
        for buffer_x in 0..self.terminal_width() {
            for buffer_y in 0..self.terminal_height() {
                let buffer_square: Point2D<i32, CharacterGridInScreenBufferFrame> =
                    point2(buffer_x, buffer_y);
                let world_square = self.buffer_square_to_world_square(buffer_square);
                if self.square_is_on_screen(world_square) {
                    all_squares.insert(world_square);
                }
            }
        }
        all_squares
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::utility::{STEP_DOWN_LEFT, STEP_LEFT, STEP_RIGHT, STEP_UP, STEP_UP_LEFT};
    use pretty_assertions::{assert_eq, assert_ne};

    fn set_up_10x10_screen() -> Screen {
        let terminal_width = 10;
        let terminal_height = 10;
        Screen::new(terminal_width, terminal_height)
    }
    fn set_up_nxn_screen(n: u16) -> Screen {
        Screen::new(n, n)
    }

    #[test]
    fn test_world_to_screen() {
        let g = set_up_10x10_screen();

        let world_square: WorldSquare = point2(0, 0);
        let screen_character_square: ScreenCharacterSquare = point2(1, g.terminal_height());
        assert_eq!(
            screen_character_square,
            g.world_square_to_left_screen_character_square(world_square)
        );

        let world_square: WorldSquare = point2(0, g.terminal_height() - 1);
        let screen_character_square: ScreenCharacterSquare = point2(1, 1);
        assert_eq!(
            screen_character_square,
            g.world_square_to_left_screen_character_square(world_square)
        );
    }
    #[test]
    fn test_world_to_screen__with_screen_motion() {
        let mut g = set_up_10x10_screen();

        let world_square: WorldSquare = point2(0, 0);
        let screen_character_square: ScreenCharacterSquare = point2(1, 5);
        let screen_pos_1 = g.world_square_to_left_screen_buffer_square(world_square);

        g.set_screen_origin(g.screen_origin + STEP_RIGHT.cast_unit());

        let screen_pos_2 = g.world_square_to_left_screen_buffer_square(world_square);

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
    fn test_world_character_grid_to_screen_buffer_grid_conversions() {
        let mut g = Screen::new(6, 3);
        g.set_screen_origin(WorldCharacterSquare::new(5, 2));
        let world_character_square = WorldCharacterSquare::new(6, 0);
        let screen_buffer_square = ScreenBufferCharacterSquare::new(1, 2);
        assert_eq!(
            g.world_character_square_to_screen_buffer_character_square(world_character_square),
            screen_buffer_square
        );
        assert_eq!(
            g.buffer_square_to_world_character_square(screen_buffer_square),
            world_character_square
        );
    }

    #[test]
    fn test_screen_buffer_step_to_world_character_step_conversion() {
        let g = Screen::new(10, 5);
        assert_eq!(
            g.screen_buffer_step_to_world_character_step(STEP_UP_LEFT.cast_unit()),
            STEP_DOWN_LEFT.cast_unit()
        );
    }

    #[test]
    fn test_rotation() {
        let mut s = Screen::new(20, 20);
        s.set_screen_center_by_world_square(point2(3, 5));

        assert_eq!(s.rotation(), QuarterTurnsAnticlockwise::new(0));
        assert_eq!(s.world_square_to_screen_square(point2(3, 7)), point2(0, 2));

        s.rotate(QuarterTurnsAnticlockwise::new(1));

        assert_eq!(s.rotation(), QuarterTurnsAnticlockwise::new(1));
        assert_eq!(s.world_square_to_screen_square(point2(3, 7)), point2(-2, 0));
        s.rotate(QuarterTurnsAnticlockwise::new(6));

        assert_eq!(s.rotation(), QuarterTurnsAnticlockwise::new(3));
        assert_eq!(s.world_square_to_screen_square(point2(2, 5)), point2(0, 1));
    }
}
