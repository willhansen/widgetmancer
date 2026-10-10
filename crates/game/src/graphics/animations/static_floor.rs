use crate::graphics::*;
use std::collections::HashMap;
use std::time::Duration;
use crate::LogicalTime;

#[derive(Clone)]
pub struct StaticFloor {
    extent: GridExtent,
    floor_color_enum: FloorColorEnum,
    start_time: LogicalTime,
}

impl StaticFloor {
    pub fn new(extent: GridExtent, floor_color_enum: FloorColorEnum) -> StaticFloor {
        StaticFloor {
            extent,
            floor_color_enum,
            start_time: LogicalTime::ZERO,
        }
    }
}

impl Animation for StaticFloor {
    fn start_time(&self) -> LogicalTime {
        // Stable per instance. The board render ignores time (duration is zero),
        // so this only needs to stop reporting a fresh wall-clock value per call.
        self.start_time
    }
    fn set_start_time(&mut self, time: LogicalTime) {
        self.start_time = time;
    }
    fn duration(&self) -> Duration {
        Duration::from_secs_f32(0.0)
    }

    fn double_glyphs_at_time(&self, _time: LogicalTime) -> HashMap<WorldSquare, DoubleGlyph> {
        let mut glyphs = HashMap::new();
        for x in 0..self.extent.width {
            for y in 0..self.extent.height {
                let world_square = WorldSquare::new(x as i32, y as i32);
                let glyph = Glyph::new(' ', BLACK, self.floor_color_enum.color_at(world_square));
                glyphs.insert(world_square, [glyph, glyph]);
            }
        }
        glyphs
    }

    fn finished_at_time(&self, _time: LogicalTime) -> bool {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn static_floor_emits_double_glyphs_by_world_square() {
        let floor_color = RGB8::new(1, 2, 3);
        let animation = StaticFloor::new(GridExtent::new(3, 2), FloorColorEnum::Solid(floor_color));
        let glyphs = animation.double_glyphs_at_time(LogicalTime::ZERO);
        let expected_glyph = Glyph::new(' ', BLACK, floor_color);

        assert_eq!(glyphs.len(), 6);
        for x in 0..3 {
            for y in 0..2 {
                assert_eq!(
                    glyphs.get(&WorldSquare::new(x, y)),
                    Some(&[expected_glyph, expected_glyph])
                );
            }
        }
    }
}
