use crate::graphics::*;
use std::collections::HashMap;
use std::time::{Duration, Instant};

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct SmiteAnimation {
    target: WorldSquare,
    start_time: Instant,
}

impl SmiteAnimation {
    pub fn new(square: WorldSquare) -> SmiteAnimation {
        SmiteAnimation {
            target: square,
            start_time: Instant::now(),
        }
    }
}

impl Animation for SmiteAnimation {
    fn start_time(&self) -> Instant {
        self.start_time
    }
    fn duration(&self) -> Duration {
        Duration::from_millis(500)
    }

    fn double_glyphs_at_time(&self, time: Instant) -> HashMap<WorldSquare, DoubleGlyph> {
        let start_width_fraction = 0.4;
        let beam_width_fraction = 1.0
            - (self.fraction_done_at_time(time) * (1.0 + start_width_fraction)
                - start_width_fraction)
                .abs();

        let one_horizontal_slice: DoubleGlyph = [1.0, -1.0].map(|i| {
            Glyph::fg_only(
                floating_square::character_for_half_square_with_1d_offset(
                    false,
                    i * (1.0 - beam_width_fraction),
                ),
                RED,
            )
        });
        // pretty arbitrary
        //let hash = ((self.target.x * 5 + self.target.y) * 1000) .to_u64() .unwrap();

        (0..30)
            .map(|i| (self.target + STEP_UP * i, one_horizontal_slice))
            .collect()
    }
}
