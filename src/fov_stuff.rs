use crate::glyph::glyph_constants::SPACE;
use crate::utility::{SquareSet, WorldPoint, WorldSquare, WorldSquareGlyphMap};
use std::collections::HashSet;

pub struct Line {
    p1: WorldPoint,
    p2: WorldPoint,
}

pub struct PartiallyVisibleSquare {
    square: WorldSquare,
    chosen_dividing_line_on_left_char: Option<Line>,
    chosen_dividing_line_on_right_char: Option<Line>,
}

pub struct FovResult {
    fully_visible_squares: SquareSet,
    partially_visible_squares: HashSet<PartiallyVisibleSquare>,
}

impl FovResult {
    pub fn as_glyph_mask(&self) -> WorldSquareGlyphMap {
        todo!()
    }
}

pub fn field_of_view_from_square(
    start_square: WorldSquare,
    sight_blockers: SquareSet,
) -> FovResult {
    todo!()
}

// trying out the newtype pattern
#[derive(shrinkwraprs::Shrinkwrap)]
pub struct FOVMask(WorldSquareGlyphMap);
impl FOVMask {}
