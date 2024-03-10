use crate::animations::Animation;
use crate::coordinates::Coordinate;
use crate::glyph::glyph_constants::EXPLOSION_COLOR;
use crate::glyph::Glyph;
use crate::utility::coordinate_frame_conversions::{
    WorldCharacterSquareGlyphMap, WorldMove, WorldPoint,
};
use crate::utility::coordinates::SignedCoordinate;
use euclid::Angle;
use num::ToPrimitive;
use rand::{Rng, SeedableRng};
use std::f32::consts::{PI, TAU};
use std::time::{Duration, Instant};

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct BurstExplosionAnimation {
    position: WorldPoint,
    start_time: Instant,
}

impl BurstExplosionAnimation {
    pub fn new(position: WorldPoint) -> BurstExplosionAnimation {
        BurstExplosionAnimation {
            position,
            start_time: Instant::now(),
        }
    }
}

impl Animation for BurstExplosionAnimation {
    fn start_time(&self) -> Instant {
        self.start_time
    }
    fn duration(&self) -> Duration {
        Duration::from_millis(500)
    }

    fn glyphs_at_time(&self, time: Instant) -> WorldCharacterSquareGlyphMap {
        // rather arbitrary
        let hash = ((self.position.x() * PI + self.position.y()) * 1000.0)
            .abs()
            .floor()
            .to_u64()
            .unwrap();
        let mut rng = rand::rngs::StdRng::seed_from_u64(hash);
        let mut points_to_draw: Vec<WorldPoint> = vec![];
        let num_particles = 50;
        let age = time.duration_since(self.start_time);
        for _ in 0..num_particles {
            let radius: f32 = 10.0;
            let speed_in_squares_per_second = rng.gen_range(0.0..=(radius.powi(2))).sqrt();

            let distance_in_squares = speed_in_squares_per_second * age.as_secs_f32();
            let angle = Angle::radians(rng.gen_range(0.0..TAU));
            let relative_position = WorldMove::from_angle_and_length(angle, distance_in_squares);
            let particle_pos = self.position + relative_position;
            points_to_draw.push(particle_pos);
        }
        Glyph::points_to_braille_glyphs(points_to_draw, EXPLOSION_COLOR)
    }
}
