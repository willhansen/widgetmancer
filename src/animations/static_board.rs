use crate::animations::{Animation, BoardAnimation};
use crate::glyph::glyph_constants::BLACK;
use crate::glyph::Glyph;
use crate::graphics::Graphics;
use crate::utility::coordinate_frame_conversions::{
    world_square_to_left_world_character_square, BoardSize, WorldCharacterSquareGlyphMap,
    WorldSquare,
};
use euclid::vec2;
use std::time;
use std::time::{Duration, Instant};

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct StaticBoard {
    board_size: BoardSize,
}

impl StaticBoard {
    pub fn new(board_size: BoardSize) -> StaticBoard {
        StaticBoard { board_size }
    }
}

impl Animation for StaticBoard {
    fn start_time(&self) -> Instant {
        // TODO: is this even applicable?
        Instant::now()
    }
    fn duration(&self) -> Duration {
        Duration::from_secs_f32(0.0)
    }

    fn glyphs_at_time(&self, _time: Instant) -> WorldCharacterSquareGlyphMap {
        let mut glyphs = WorldCharacterSquareGlyphMap::new();
        for x in 0..self.board_size.width {
            for y in 0..self.board_size.height {
                let world_square = WorldSquare::new(x as i32, y as i32);
                let glyph = Glyph::new(' ', BLACK, Graphics::board_color_at_square(world_square));
                let left_character_square =
                    world_square_to_left_world_character_square(world_square);
                let right_character_square = left_character_square + vec2(1, 0);
                glyphs.insert(left_character_square, glyph);
                glyphs.insert(right_character_square, glyph);
            }
        }
        glyphs
    }

    fn finished_at_time(&self, _time: Instant) -> bool {
        false
    }
}

impl BoardAnimation for StaticBoard {
    fn next_animation(&self) -> Box<dyn BoardAnimation> {
        Box::new(*self)
    }
}
