use crate::animations::Animation;
use crate::glyph::glyph_constants::CYAN;
use crate::glyph::Glyph;
use crate::utility::application_specific_units_and_conversions::{
    WorldCharacterSquareGlyphMap, WorldMove, WorldPoint,
};
use euclid::Angle;
use std::time::{Duration, Instant};

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct CircleAttackAnimation {
    position: WorldPoint,
    radius: f32,
    start_time: Instant,
}

impl CircleAttackAnimation {
    pub fn new(position: WorldPoint, radius: f32) -> CircleAttackAnimation {
        CircleAttackAnimation {
            position,
            radius,
            start_time: Instant::now(),
        }
    }
}

impl Animation for CircleAttackAnimation {
    fn start_time(&self) -> Instant {
        self.start_time
    }
    fn duration(&self) -> Duration {
        Duration::from_millis(500)
    }

    fn glyphs_at_time(&self, time: Instant) -> WorldCharacterSquareGlyphMap {
        // rather arbitrary
        let mut points_to_draw: Vec<WorldPoint> = vec![];
        let num_particles = 50;
        for i in 0..num_particles {
            let angle = Angle::degrees(i as f32 * 360.0 / num_particles as f32);

            let distance_in_squares = self.radius * self.fraction_remaining_at_time(time);
            let relative_position = WorldMove::from_angle_and_length(angle, distance_in_squares);
            let particle_pos = self.position + relative_position;
            points_to_draw.push(particle_pos);
        }
        Glyph::points_to_braille_glyphs(points_to_draw, CYAN)
    }
}
