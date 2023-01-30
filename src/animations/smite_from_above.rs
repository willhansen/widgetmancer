use crate::animations::Animation;
use crate::glyph::braille::world_points_for_braille_line;
use crate::glyph::glyph_constants::RED;
use crate::glyph::{floating_square, DoubleGlyph, Glyph};
use crate::utility::coordinate_frame_conversions::{
    world_square_glyph_map_to_world_character_glyph_map, WorldCharacterSquareGlyphMap, WorldMove,
    WorldPoint, WorldSquare,
};
use crate::utility::STEP_UP;
use euclid::Angle;
use num::ToPrimitive;
use rand::{Rng, SeedableRng};
use std::f32::consts::{E, PI, TAU};
use std::time::{Duration, Instant};

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct SmiteFromAbove {
    target: WorldSquare,
    start_time: Instant,
}

impl SmiteFromAbove {
    pub fn new(square: WorldSquare) -> SmiteFromAbove {
        SmiteFromAbove {
            target: square,
            start_time: Instant::now(),
        }
    }
}

impl Animation for SmiteFromAbove {
    fn start_time(&self) -> Instant {
        self.start_time
    }
    fn duration(&self) -> Duration {
        Duration::from_millis(500)
    }

    fn glyphs_at_time(&self, time: Instant) -> WorldCharacterSquareGlyphMap {
        let beam_width_fraction =
            time.duration_since(self.start_time()).as_secs_f32() / self.duration().as_secs_f32();

        let one_horizontal_slice: DoubleGlyph = [1.0, -1.0].map(|i| {
            Glyph::fg_only(
                floating_square::character_for_square_with_1d_offset(
                    false,
                    i * beam_width_fraction,
                ),
                RED,
            )
        });
        // pretty arbitrary
        //let hash = ((self.target.x * 5 + self.target.y) * 1000) .to_u64() .unwrap();

        world_square_glyph_map_to_world_character_glyph_map(
            (0..30)
                .map(|i| (self.target + STEP_UP * i, one_horizontal_slice))
                .collect(),
        )
    }
}
