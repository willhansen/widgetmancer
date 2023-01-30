use crate::animations::{Animation, DOTS_IN_SELECTOR};
use crate::glyph::glyph_constants::SELECTOR_COLOR;
use crate::glyph::Glyph;
use crate::utility::coordinate_frame_conversions::{
    WorldCharacterSquareGlyphMap, WorldMove, WorldSquare,
};
use euclid::Angle;
use num::Float;
use std::f32::consts::TAU;
use std::time::{Duration, Instant};

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct SelectorAnimation {
    square: WorldSquare,
    start_time: Instant,
}

impl SelectorAnimation {
    pub fn new(square: WorldSquare) -> SelectorAnimation {
        SelectorAnimation {
            square,
            start_time: Instant::now(),
        }
    }
}

impl Animation for SelectorAnimation {
    fn start_time(&self) -> Instant {
        self.start_time
    }
    fn duration(&self) -> Duration {
        Duration::from_secs_f32(1.0)
    }

    fn glyphs_at_time(&self, time: Instant) -> WorldCharacterSquareGlyphMap {
        let num_dots = DOTS_IN_SELECTOR;
        let radius_in_squares = 1.0; //f32::sqrt(2.0) / 2.0;

        let rotation_rate_rad_per_s = 3.0;
        let age = time.duration_since(self.start_time);

        let base_angle = Angle::radians(rotation_rate_rad_per_s * age.as_secs_f32());
        let mut points = vec![];
        for i in 0..num_dots {
            let radians: f32 = (base_angle).radians + i as f32 / num_dots as f32 * TAU;
            let relative_point = WorldMove::new(
                radius_in_squares * radians.cos(),
                radius_in_squares * radians.sin(),
            );
            points.push(self.square.to_f32() + relative_point);
        }
        Glyph::points_to_braille_glyphs(points, SELECTOR_COLOR)
    }
    fn finished_at_time(&self, _time: Instant) -> bool {
        false
    }
}
