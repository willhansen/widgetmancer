use crate::graphics::*;
use std::collections::HashMap;
use std::time::{Duration, Instant};

#[derive(Clone)]
pub struct StaticBoard {
    board_size: BoardSize,
    floor_color_enum: FloorColorEnum,
}

impl StaticBoard {
    pub fn new(board_size: BoardSize, floor_color_enum: FloorColorEnum) -> StaticBoard {
        StaticBoard {
            board_size,
            floor_color_enum,
        }
    }
}

impl Animation for StaticBoard {
    fn start_time(&self) -> Instant {
        // TODO: is this even applicable?
        Instant::now()
    }
    fn duration(&self) -> Duration {
        Duration::from_secs_f32(0.0)
    }

    fn double_glyphs_at_time(&self, _time: Instant) -> HashMap<WorldSquare, DoubleGlyph> {
        let mut glyphs = HashMap::new();
        for x in 0..self.board_size.width {
            for y in 0..self.board_size.height {
                let world_square = WorldSquare::new(x as i32, y as i32);
                let glyph = Glyph::new(' ', BLACK, self.floor_color_enum.color_at(world_square));
                glyphs.insert(world_square, [glyph, glyph]);
            }
        }
        glyphs
    }

    fn finished_at_time(&self, _time: Instant) -> bool {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn static_board_emits_double_glyphs_by_world_square() {
        let floor_color = RGB8::new(1, 2, 3);
        let animation = StaticBoard::new(BoardSize::new(3, 2), FloorColorEnum::Solid(floor_color));
        let glyphs = animation.double_glyphs_at_time(Instant::now());
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
