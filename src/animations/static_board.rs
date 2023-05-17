use crate::animations::Animation;
use crate::glyph::glyph_constants::BLACK;
use crate::glyph::Glyph;
use crate::graphics::{FloorColorEnum, Graphics};
use crate::utility::coordinate_frame_conversions::{
    world_square_to_left_world_character_square, BoardSize, WorldCharacterSquareGlyphMap,
    WorldSquare,
};
use euclid::vec2;
use rgb::RGB8;
use std::time;
use std::time::{Duration, Instant};

#[derive(Clone)]
pub struct StaticBoard {
    board_size: BoardSize,
    floor_color_enum: FloorColorEnum,
}

impl StaticBoard {
    pub fn new(board_size: BoardSize, floor_color_enum: FloorColorEnum) -> StaticBoard {
        StaticBoard {
            board_size,
            floor_color_enum,
        }
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
                let glyph = Glyph::new(' ', BLACK, self.floor_color_enum.color_at(world_square));
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
