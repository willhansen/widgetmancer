use crate::glyph::glyph_constants::SPACE;
use crate::utility::{
    octant_to_outward_and_across_directions, SquareSet, WorldPoint, WorldSquare,
    WorldSquareGlyphMap,
};
use std::collections::{HashMap, HashSet};

pub struct Line {
    p1: WorldPoint,
    p2: WorldPoint,
}

pub struct PartialVisibilityOfASquare {
    chosen_dividing_line_on_left_char: Option<Line>,
    chosen_dividing_line_on_right_char: Option<Line>,
}

pub struct FovResult {
    fully_visible_squares: SquareSet,
    partially_visible_squares: HashMap<WorldSquare, PartialVisibilityOfASquare>,
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
    let sight_radius = 8;
    for octant_number in 0..8 {
        let (outward_dir, across_dir) = octant_to_outward_and_across_directions(octant_number);
        // skip the central square
        for outward_steps in 1..sight_radius {
            for across_steps in 0..=outward_steps {
                let square = start_square + outward_dir * outward_steps + across_dir * across_steps;
            }
        }
    }
    todo!()
}
