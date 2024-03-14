use crate::animations::Animation;
use crate::glyph::glyph_constants::RED;
use crate::glyph::Glyph;
use crate::utility::application_specific_units_and_conversions::{
    WorldCharacterSquareGlyphMap, WorldPoint,
};
use std::time::{Duration, Instant};

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct SimpleLaserAnimation {
    start: WorldPoint,
    end: WorldPoint,
    start_time: Instant,
}

impl SimpleLaserAnimation {
    pub fn new(start: WorldPoint, end: WorldPoint) -> SimpleLaserAnimation {
        SimpleLaserAnimation {
            start,
            end,
            start_time: Instant::now(),
        }
    }
}

impl Animation for SimpleLaserAnimation {
    fn start_time(&self) -> Instant {
        self.start_time
    }
    fn duration(&self) -> Duration {
        Duration::from_millis(500)
    }

    fn glyphs_at_time(&self, _time: Instant) -> WorldCharacterSquareGlyphMap {
        Glyph::get_glyphs_for_colored_braille_line(self.start, self.end, RED)
    }
}
