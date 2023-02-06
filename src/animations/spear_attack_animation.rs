use crate::animations::Animation;
use crate::glyph::glyph_constants::{CYAN, EXPLOSION_COLOR, GREY, GREY_RED, RED};
use crate::glyph::Glyph;
use crate::utility::coordinate_frame_conversions::{
    WorldCharacterSquareGlyphMap, WorldMove, WorldPoint, WorldSquare, WorldStep,
};
use crate::utility::{is_orthodiagonal, KING_STEPS};
use euclid::Angle;
use num::ToPrimitive;
use rand::{Rng, SeedableRng};
use std::f32::consts::{PI, TAU};
use std::time::{Duration, Instant};

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct SpearAttackAnimation {
    start_square: WorldSquare,
    direction: WorldStep,
    range: u32,
    start_time: Instant,
}

impl SpearAttackAnimation {
    pub fn new(
        start_square: WorldSquare,
        direction: WorldStep,
        range: u32,
    ) -> SpearAttackAnimation {
        assert!(KING_STEPS.contains(&direction));
        SpearAttackAnimation {
            start_square,
            direction,
            range,
            start_time: Instant::now(),
        }
    }
}

impl Animation for SpearAttackAnimation {
    fn start_time(&self) -> Instant {
        self.start_time
    }
    fn duration(&self) -> Duration {
        Duration::from_millis(500)
    }

    fn glyphs_at_time(&self, time: Instant) -> WorldCharacterSquareGlyphMap {
        let mut points_to_draw: Vec<WorldPoint> = vec![];
        let num_particles = 50;
        let sweep_degrees = 10.0;
        let angle_delta =
            Angle::degrees(-sweep_degrees / 2.0 + sweep_degrees * self.fraction_done_at_time(time));
        let angle = self.direction.to_f32().angle_from_x_axis() + angle_delta;
        let spear_length = self.range as f32; //* self.fraction_remaining_at_time(time);
        for i in 0..num_particles {
            let relative_position = WorldMove::from_angle_and_length(
                angle,
                spear_length * i as f32 / num_particles as f32,
            );
            let particle_pos = self.start_square.to_f32() + relative_position;
            points_to_draw.push(particle_pos);
        }
        Glyph::points_to_braille_glyphs(points_to_draw, GREY_RED)
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::utility::STEP_RIGHT;
    use euclid::point2;

    #[test]
    fn test_spear_attack_does_any_drawing() {
        let square = point2(3, 3);
        let dir = STEP_RIGHT;
        let animation = SpearAttackAnimation::new(square, dir, 3);
        let double_glyphs = animation.double_glyphs_at_duration(Duration::from_secs_f32(0.1));
        assert!(double_glyphs.contains_key(&(square + STEP_RIGHT)));
        dbg!(double_glyphs);
    }
}
