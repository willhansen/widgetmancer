use crate::graphics::*;
use std::collections::HashMap;
use std::time::Duration;
use crate::LogicalTime;

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct SimpleLaserAnimation {
    start: WorldPoint,
    end: WorldPoint,
    start_time: LogicalTime,
}

impl SimpleLaserAnimation {
    pub fn new(start: WorldPoint, end: WorldPoint) -> SimpleLaserAnimation {
        SimpleLaserAnimation {
            start,
            end,
            start_time: LogicalTime::ZERO,
        }
    }
}

impl Animation for SimpleLaserAnimation {
    fn start_time(&self) -> LogicalTime {
        self.start_time
    }
    fn set_start_time(&mut self, time: LogicalTime) {
        self.start_time = time;
    }
    fn duration(&self) -> Duration {
        Duration::from_millis(500)
    }

    fn double_glyphs_at_time(&self, _time: LogicalTime) -> HashMap<WorldSquare, DoubleGlyph> {
        Glyph::double_glyphs_for_colored_braille_line(self.start, self.end, RED)
    }
}
