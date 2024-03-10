use crate::animations::Animation;
use crate::glyph::glyph_constants::EXPLOSION_COLOR;
use crate::glyph::Glyph;
use crate::utility::coordinate_frame_conversions::{
    WorldCharacterSquareGlyphMap, WorldPoint, WorldSquare,
};
use crate::vec2;
use num::ToPrimitive;
use rand::{Rng, SeedableRng};
use std::f32::consts::PI;
use std::time::{Duration, Instant};

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct PieceDeathAnimation {
    square: WorldSquare,
    start_time: Instant,
}

impl PieceDeathAnimation {
    pub fn new(square: WorldSquare) -> PieceDeathAnimation {
        PieceDeathAnimation {
            square,
            start_time: Instant::now(),
        }
    }
}

impl Animation for PieceDeathAnimation {
    fn start_time(&self) -> Instant {
        self.start_time
    }
    fn duration(&self) -> Duration {
        Duration::from_secs_f32(5.0)
    }

    fn glyphs_at_time(&self, time: Instant) -> WorldCharacterSquareGlyphMap {
        assert!(!self.finished_at_time(time));

        // rather arbitrary
        let hash = ((self.square.x as f32 * PI + self.square.y as f32) * 1000.0)
            .abs()
            .floor()
            .to_u64()
            .unwrap();
        let mut rng = rand::rngs::StdRng::seed_from_u64(hash);
        let mut points_to_draw: Vec<WorldPoint> = vec![];
        let num_particles = 20;
        let age = time.duration_since(self.start_time);
        let remaining_seconds = self.duration().as_secs_f32() - age.as_secs_f32();
        let lifetime_fraction_remaining = remaining_seconds / self.duration().as_secs_f32();

        let range = -0.5..0.5;
        let points_to_draw = (0..num_particles)
            .map(|_| {
                let x_pos = rng.gen_range(range.clone()) * lifetime_fraction_remaining;
                let y_pos =
                    (rng.gen_range(range.clone()) + 0.5) * lifetime_fraction_remaining - 0.5;
                self.square.to_f32() + vec2(x_pos, y_pos)
            })
            .collect();
        Glyph::points_to_braille_glyphs(points_to_draw, EXPLOSION_COLOR)
    }
}
