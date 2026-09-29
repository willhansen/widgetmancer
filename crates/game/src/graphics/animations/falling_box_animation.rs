use crate::graphics::*;
use euclid::vec2;
use std::collections::HashMap;
use std::time::Duration;
use crate::LogicalTime;

/// A numbered box that was shoved off the board edge. Draws a solid square
/// centered on the edge square it fell from, shrinking toward the center so it
/// reads as dropping away into the distance.
#[derive(Clone, PartialEq, Debug, Copy)]
pub struct FallingBoxAnimation {
    square: WorldSquare,
    start_time: LogicalTime,
}

impl FallingBoxAnimation {
    pub fn new(square: WorldSquare) -> FallingBoxAnimation {
        FallingBoxAnimation {
            square,
            start_time: LogicalTime::ZERO,
        }
    }

    /// A filled lattice of points inside the square centered on `self.square`,
    /// with `half_extent` world units on each side. The braille renderer snaps
    /// these to its dot grid, so the fill reads as a solid square that shrinks
    /// toward a single dot.
    fn filled_square_points(&self, half_extent: f32) -> Vec<WorldPoint> {
        let center = self.square.to_f32();
        let steps = 5;
        let mut points = Vec::with_capacity((steps + 1) * (steps + 1));
        for i in 0..=steps {
            for j in 0..=steps {
                let x = -half_extent + 2.0 * half_extent * i as f32 / steps as f32;
                let y = -half_extent + 2.0 * half_extent * j as f32 / steps as f32;
                points.push(center + vec2(x, y));
            }
        }
        points
    }
}

impl Animation for FallingBoxAnimation {
    fn start_time(&self) -> LogicalTime {
        self.start_time
    }
    fn set_start_time(&mut self, time: LogicalTime) {
        self.start_time = time;
    }
    fn duration(&self) -> Duration {
        Duration::from_millis(700)
    }

    fn double_glyphs_at_time(&self, time: LogicalTime) -> HashMap<WorldSquare, DoubleGlyph> {
        // Start just inside the cell: a half-extent of exactly 0.5 puts the
        // square's edge on the cell boundary, where rounding scatters the dots
        // into the neighboring squares.
        let half_extent = 0.49 * self.fraction_remaining_at_time(time);
        Glyph::points_to_braille_double_glyphs(
            self.filled_square_points(half_extent),
            FALLING_BOX_COLOR,
        )
    }
}
