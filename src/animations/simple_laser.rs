use crate::animations::Animation;
use crate::glyph::glyph_constants::RED;
use crate::glyph::Glyph;
use crate::utility::coordinate_frame_conversions::{WorldCharacterSquareGlyphMap, WorldPoint};
use std::time::{Duration, Instant};

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct SimpleLaser {
    start: WorldPoint,
    end: WorldPoint,
    start_time: Instant,
}

impl SimpleLaser {
    pub fn new(start: WorldPoint, end: WorldPoint) -> SimpleLaser {
        SimpleLaser {
            start,
            end,
            start_time: Instant::now(),
        }
    }
}

impl Animation for SimpleLaser {
    fn start_time(&self) -> Instant {
        self.start_time
    }

    fn glyphs_at_time(&self, _time: Instant) -> WorldCharacterSquareGlyphMap {
        Glyph::get_glyphs_for_colored_braille_line(self.start, self.end, RED)
    }

    fn finished_at_time(&self, time: Instant) -> bool {
        time.duration_since(self.start_time) > Duration::from_millis(500)
    }
}
