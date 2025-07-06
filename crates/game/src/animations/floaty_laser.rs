use crate::animations::Animation;
use crate::glyph::braille::world_points_for_braille_line;
use crate::glyph::glyph_constants::RED;
use crate::glyph::Glyph;
use crate::utility::coordinate_frame_conversions::{
    WorldCharacterSquareGlyphMap, WorldMove, WorldPoint,
};
use euclid::Angle;
use num::ToPrimitive;
use rand::{Rng, SeedableRng};
use std::f32::consts::{E, PI, TAU};
use std::time::{Duration, Instant};

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct FloatyLaserAnimation {
    start: WorldPoint,
    end: WorldPoint,
    start_time: Instant,
}

impl FloatyLaserAnimation {
    pub fn new(start: WorldPoint, end: WorldPoint) -> FloatyLaserAnimation {
        FloatyLaserAnimation {
            start,
            end,
            start_time: Instant::now(),
        }
    }
}

impl Animation for FloatyLaserAnimation {
    fn start_time(&self) -> Instant {
        self.start_time
    }
    fn duration(&self) -> Duration {
        Duration::from_millis(500)
    }

    fn glyphs_at_time(&self, time: Instant) -> WorldCharacterSquareGlyphMap {
        let mut line_points: Vec<WorldPoint> = world_points_for_braille_line(self.start, self.end);
        // pretty arbitrary
        let hash = ((self.start.x * PI + self.start.y)
            * 1000.0
            * (self.end.x * E + self.end.y * TAU * 5.0))
            .abs()
            .floor()
            .to_u64()
            .unwrap();
        let mut rng = rand::rngs::StdRng::seed_from_u64(hash);
        let vertical_drift_speed_blocks_per_s = 3.0;
        let random_drift_speed_blocks_per_s = 1.0;
        let age = time.duration_since(self.start_time);
        for mut point in &mut line_points {
            let vertical_displacement: WorldMove =
                WorldMove::new(0.0, 1.0) * vertical_drift_speed_blocks_per_s * age.as_secs_f32();
            let random_angle = Angle::radians(rng.gen_range(0.0..TAU));
            let random_displacement = WorldMove::from_angle_and_length(
                random_angle,
                random_drift_speed_blocks_per_s * age.as_secs_f32(),
            );
            *point += vertical_displacement + random_displacement;
        }
        Glyph::points_to_braille_glyphs(line_points, RED)
        //Glyph::get_glyphs_for_colored_braille_line(self.start, self.end, RED)
    }
}
