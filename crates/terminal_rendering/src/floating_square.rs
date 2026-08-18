use crate::glyph_constants::*;
use crate::hextant_blocks::hextant_block_by_offset;
use crate::DoubleChar;
use utility::coordinate_frame_conversions::{WorldMove,};
use utility::*;
use euclid::vec2;
use ordered_float::OrderedFloat;

pub fn quadrant_block_by_offset(half_steps: IVector) -> char {
    match half_steps.to_tuple() {
        (1, -1) => '▗',
        (1, 0) => '▐',
        (1, 1) => '▝',
        (0, -1) => '▄',
        (0, 0) => '█',
        (0, 1) => '▀',
        (-1, -1) => '▖',
        (-1, 0) => '▌',
        (-1, 1) => '▘',
        _ => ' ',
    }
}

pub fn square_with_half_step_offset(offset: FVector) -> char {
    let step: IVector = (offset * 2.0).round().to_i32();
    quadrant_block_by_offset(step)
}

pub fn character_for_half_square_with_vertical_thirds_offset(thirds_up: i32) -> char {
    if thirds_up >= 3 {
        SPACE
    } else if thirds_up == 2 {
        UPPER_ONE_THIRD_BLOCK
    } else if thirds_up == 1 {
        UPPER_TWO_THIRD_BLOCK
    } else if thirds_up == 0 {
        FULL_BLOCK
    } else if thirds_up == -1 {
        LOWER_TWO_THIRD_BLOCK
    } else if thirds_up == -2 {
        LOWER_ONE_THIRD_BLOCK
    } else {
        SPACE
    }
}

pub fn character_for_half_square_with_1d_eighths_offset(vertical: bool, eighths: i32) -> char {
    if eighths.abs() >= 8 {
        return SPACE;
    }
    let positive_case = eighths >= 0;
    let abs_index = 8 - eighths.abs() as usize;
    let array = if vertical {
        if positive_case {
            EIGHTH_BLOCKS_FROM_TOP
        } else {
            EIGHTH_BLOCKS_FROM_BOTTOM
        }
    } else {
        if positive_case {
            EIGHTH_BLOCKS_FROM_RIGHT
        } else {
            EIGHTH_BLOCKS_FROM_LEFT
        }
    };
    array[abs_index]
}

pub fn character_for_half_square_with_1d_offset(
    vertical: bool,
    fraction_of_square_offset: f32,
) -> char {
    let eighths = (fraction_of_square_offset * 8.0).round() as i32;
    if vertical {
        let snapped_to_thirds = snap_to_nths(fraction_of_square_offset, 3);
        let snapped_to_eighths = snap_to_nths(fraction_of_square_offset, 8);
        let error_from_thirds = (fraction_of_square_offset - snapped_to_thirds).abs();
        let error_from_eighths = (fraction_of_square_offset - snapped_to_eighths).abs();
        if error_from_thirds < error_from_eighths {
            let thirds = (snapped_to_thirds * 3.0).round() as i32;
            character_for_half_square_with_vertical_thirds_offset(thirds)
        } else {
            character_for_half_square_with_1d_eighths_offset(vertical, eighths)
        }
    } else {
        character_for_half_square_with_1d_eighths_offset(vertical, eighths)
    }
}

/// Glyph families that can render a square coherently. No glyph combines
/// fine x AND fine y in one cell, so each family commits to a resolution
/// per axis; snapping each half-cell independently let sibling cells pick
/// different families, which tore the silhouette (see
/// tests/floating_square_coherence.rs).
#[derive(Clone, Copy, Debug, PartialEq)]
enum SnapFamily {
    /// x to 1/16 world (eighth of a half-cell), y to the row
    HorizontalEighths,
    /// x to the half-cell, y to 1/8 world
    VerticalEighths,
    /// x to 1/4 world (half-cell), y to thirds
    Hextant,
    /// x to 1/4 world (half-cell), y to halves
    Quadrant,
}

impl SnapFamily {
    const ALL: [SnapFamily; 4] = [
        SnapFamily::HorizontalEighths,
        SnapFamily::VerticalEighths,
        SnapFamily::Hextant,
        SnapFamily::Quadrant,
    ];

    /// Nearest position on this family's grid, in world units. All grids
    /// are integer-aligned, so snapping preserves the cell's integer
    /// offset from the square's center; that is what makes every
    /// neighborhood cell agree on the family.
    fn snapped_offset(&self, o: FVector) -> FVector {
        match self {
            SnapFamily::HorizontalEighths => vec2(snap_to_nths(o.x, 16), snap_to_nths(o.y, 1)),
            SnapFamily::VerticalEighths => vec2(snap_to_nths(o.x, 2), snap_to_nths(o.y, 8)),
            SnapFamily::Hextant => vec2(snap_to_nths(o.x, 4), snap_to_nths(o.y, 3)),
            SnapFamily::Quadrant => vec2(snap_to_nths(o.x, 4), snap_to_nths(o.y, 2)),
        }
    }

    /// Every snap grid is integer-aligned, so all cells of a square agree
    /// on the family no matter which cell's offset is passed. Errors are
    /// compared in world units, which are already visually isotropic
    /// (1 unit = 2 columns horizontally, 1 row ~ 2 column-widths
    /// vertically).
    fn for_offset(o: FVector) -> SnapFamily {
        SnapFamily::ALL
            .into_iter()
            .min_by_key(|f| OrderedFloat(f.snap_error(o)))
            .unwrap()
    }

    fn name(&self) -> &'static str {
        match self {
            SnapFamily::HorizontalEighths => "horizontal eighths (x: 1/16, y: row)",
            SnapFamily::VerticalEighths => "vertical eighths (x: half-cell, y: 1/8)",
            SnapFamily::Hextant => "hextant (x: 1/4, y: 1/3)",
            SnapFamily::Quadrant => "quadrant (x: 1/4, y: 1/2)",
        }
    }

    fn snap_error(&self, o: FVector) -> f32 {
        (self.snapped_offset(o) - o).length()
    }

    /// Glyph for one half-cell whose offset from the square's center is
    /// `r` (half-cell units in x, row units in y). The snapped offset plus
    /// the integer x-compensation guarantees `r` lands on this family's
    /// grid.
    fn character_for_half_square(&self, r: FVector) -> char {
        match self {
            // pure eighths, not character_for_half_square_with_1d_offset:
            // its thirds-vs-eighths mixing would break family purity.
            // The 1d glyph fills the whole row/column, so cells the
            // snapped square doesn't overlap (|r| = 1 on the forced axis)
            // must be empty.
            SnapFamily::HorizontalEighths => {
                if r.y.abs() >= 0.5 {
                    SPACE
                } else {
                    character_for_half_square_with_1d_eighths_offset(
                        false,
                        (r.x * 8.0).round() as i32,
                    )
                }
            }
            SnapFamily::VerticalEighths => {
                if r.x.abs() >= 0.5 {
                    SPACE
                } else {
                    character_for_half_square_with_1d_eighths_offset(
                        true,
                        (r.y * 8.0).round() as i32,
                    )
                }
            }
            SnapFamily::Hextant => {
                hextant_block_by_offset(vec2(r.x * 2.0, r.y * 3.0).round().to_i32())
            }
            SnapFamily::Quadrant => square_with_half_step_offset(r),
        }
    }
}

fn characters_in_family(offset: WorldMove, family: SnapFamily) -> DoubleChar {
    let snapped = family.snapped_offset(vec2(offset.x, offset.y));
    let char_offsets = [-1.0, 1.0].map(|i| {
        let scaled_x_offset = snapped.x * 2.0;
        let shifted_toward_this_side = sign(scaled_x_offset) == i;
        let compensated_x_offset = if shifted_toward_this_side {
            (scaled_x_offset.abs() - 1.0).max(0.0) * sign(scaled_x_offset)
        } else {
            scaled_x_offset
        };
        vec2(compensated_x_offset, snapped.y)
    });
    char_offsets.map(|char_offset| family.character_for_half_square(char_offset))
}

pub fn characters_for_full_square_with_2d_offset(offset: WorldMove) -> DoubleChar {
    characters_in_family(offset, SnapFamily::for_offset(vec2(offset.x, offset.y)))
}

/// Per-position snapshot of the snap decision, for the floating_square_debug
/// tool. Not game-facing API.
#[doc(hidden)]
#[derive(Clone, Copy, Debug)]
pub struct SnapDebugInfo {
    pub family: &'static str,
    pub snapped_offset: FVector,
    /// (family name, snap error) for every candidate, best first.
    pub candidates: [(&'static str, f32); 4],
}

#[doc(hidden)]
pub fn snap_debug_info(offset: WorldMove) -> SnapDebugInfo {
    let o = vec2(offset.x, offset.y);
    let mut candidates = SnapFamily::ALL.map(|f| (f.name(), f.snap_error(o)));
    candidates.sort_by_key(|&(_, e)| OrderedFloat(e));
    let family = SnapFamily::for_offset(o);
    SnapDebugInfo {
        family: family.name(),
        snapped_offset: family.snapped_offset(o),
        candidates,
    }
}

/// Display names of the snap families, in `SnapFamily::ALL` order
/// (h-eighths, v-eighths, hextant, quadrant).
#[doc(hidden)]
pub fn snap_family_names() -> [&'static str; 4] {
    SnapFamily::ALL.map(|f| f.name())
}

/// `characters_for_full_square_with_2d_offset` with the family forced, for
/// side-by-side "what would family X have done" views. `family_index`
/// indexes `SnapFamily::ALL` / `snap_family_names()`.
#[doc(hidden)]
pub fn characters_for_full_square_with_2d_offset_forced(
    offset: WorldMove,
    family_index: usize,
) -> DoubleChar {
    characters_in_family(offset, SnapFamily::ALL[family_index])
}

pub fn characters_for_full_square_with_1d_offset(
    direction: OrthogonalWorldStep,
    fraction_of_full_square_in_direction: f32,
) -> DoubleChar {
    let is_vertical = direction.step().x == 0;
    let is_positive_direction = direction.step().x + direction.step().y > 0;

    let fraction_of_full_square_in_positive_direction =
        fraction_of_full_square_in_direction * if is_positive_direction { 1.0 } else { -1.0 };
    if is_vertical {
        [character_for_half_square_with_1d_offset(
            is_vertical,
            fraction_of_full_square_in_positive_direction,
        ); 2]
    } else {
        let dx = fraction_of_full_square_in_positive_direction;
        let offsets = if dx > 0.0 {
            [dx * 2.0, (dx * 2.0 - 1.0).max(0.0)]
        } else {
            [(dx * 2.0 + 1.0).min(0.0), dx * 2.0]
        };
        offsets.map(|x| character_for_half_square_with_1d_offset(is_vertical, x))
    }
}
pub fn characters_for_full_square_with_looping_1d_offset(
    direction: OrthogonalWorldStep,
    fraction_of_full_square_in_direction: f32,
) -> DoubleChar {
    characters_for_full_square_with_1d_offset(
        direction,
        looping_clamp(-1.0, 1.0, fraction_of_full_square_in_direction),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    
    use euclid::vec2;

    #[test]
    fn test_colored_square_with_half_step_offsets() {
        assert_eq!(
            square_with_half_step_offset(vec2(0.0, 0.0)),
            quadrant_block_by_offset(vec2(0, 0))
        );
        assert_eq!(
            square_with_half_step_offset(vec2(0.1, 0.1)),
            quadrant_block_by_offset(vec2(0, 0))
        );
        assert_eq!(
            square_with_half_step_offset(vec2(0.24, 0.0)),
            quadrant_block_by_offset(vec2(0, 0))
        );
        assert_eq!(
            square_with_half_step_offset(vec2(0.25, 0.0)),
            quadrant_block_by_offset(vec2(1, 0))
        );
        assert_eq!(
            square_with_half_step_offset(vec2(0.26, 0.0)),
            quadrant_block_by_offset(vec2(1, 0))
        );
        assert_eq!(
            square_with_half_step_offset(vec2(-0.25, 0.0)),
            quadrant_block_by_offset(vec2(0, 0))
        );
        assert_eq!(
            square_with_half_step_offset(vec2(-0.26, 0.0)),
            quadrant_block_by_offset(vec2(-1, 0))
        );
        assert_eq!(
            square_with_half_step_offset(vec2(0.49, 0.0)),
            quadrant_block_by_offset(vec2(1, 0))
        );
        assert_eq!(
            square_with_half_step_offset(vec2(0.5, 0.0)),
            quadrant_block_by_offset(vec2(1, 0))
        );
        assert_eq!(
            square_with_half_step_offset(vec2(0.2, 0.4)),
            quadrant_block_by_offset(vec2(0, 1))
        );
        assert_eq!(
            square_with_half_step_offset(vec2(-0.499, 0.4)),
            quadrant_block_by_offset(vec2(-1, 1))
        );
        assert_eq!(
            square_with_half_step_offset(vec2(0.74, 0.0)),
            quadrant_block_by_offset(vec2(1, 0))
        );
        assert_eq!(
            square_with_half_step_offset(vec2(0.76, 0.0)),
            quadrant_block_by_offset(vec2(2, 0))
        );
        assert_eq!(
            square_with_half_step_offset(vec2(0.3, -0.6)),
            quadrant_block_by_offset(vec2(1, -1))
        );
    }

    //                      |<--halfway
    // ' ', '▏', '▎', '▍', '▌', '▋', '▊', '▉', '█'
    #[test]
    fn test_character_square_horizontal_offset_base_case() {
        assert_eq!(
            character_for_half_square_with_1d_offset(false, 0.0),
            FULL_BLOCK
        );
    }

    #[test]
    fn test_character_square_horizontal_offset_round_to_zero() {
        assert_eq!(
            character_for_half_square_with_1d_offset(false, -0.001),
            FULL_BLOCK
        );

        assert_eq!(
            character_for_half_square_with_1d_offset(false, 0.001),
            FULL_BLOCK
        );
    }

    #[test]
    fn test_character_square_horizontal_offset_out_of_range() {
        assert_eq!(character_for_half_square_with_1d_offset(false, -1.5), SPACE);
        assert_eq!(character_for_half_square_with_1d_offset(false, 1.5), SPACE);
    }

    #[test]
    fn test_character_square_horizontal_offset_halfway() {
        assert_eq!(
            character_for_half_square_with_1d_offset(false, -0.5),
            EIGHTH_BLOCKS_FROM_LEFT[4]
        );
        assert_eq!(
            character_for_half_square_with_1d_offset(false, 0.5),
            EIGHTH_BLOCKS_FROM_RIGHT[4]
        );
    }

    #[test]
    fn test_character_square_horizontal_offset_match_opposite_ends() {
        assert_eq!(character_for_half_square_with_1d_offset(false, -1.0), SPACE);
        assert_eq!(character_for_half_square_with_1d_offset(false, 1.0), SPACE);
    }

    #[test]
    fn test_eighths_1d_offset() {
        assert_eq!(
            character_for_half_square_with_1d_eighths_offset(false, 0),
            FULL_BLOCK
        );
        assert_eq!(
            character_for_half_square_with_1d_eighths_offset(false, 4),
            RIGHT_HALF_BLOCK
        );
        assert_eq!(
            character_for_half_square_with_1d_eighths_offset(false, -4),
            LEFT_HALF_BLOCK
        );
        assert_eq!(
            character_for_half_square_with_1d_eighths_offset(true, -4),
            LOWER_HALF_BLOCK
        );
        assert_eq!(
            character_for_half_square_with_1d_eighths_offset(true, 25),
            SPACE
        );
    }

    #[test]
    fn test_snap_family_selection() {
        let f = SnapFamily::for_offset;
        // the reported tearing case: v-eighths (0.5, 1/8) is closer than
        // hextant (1/4, 1/3) once y error is counted honestly
        assert_eq!(f(vec2(0.363, 0.184)), SnapFamily::VerticalEighths);
        // y residual near a third, x near a quarter -> hextant
        assert_eq!(f(vec2(0.25, 0.3)), SnapFamily::Hextant);
        // near-axis offsets pick the fine family of the other axis
        assert_eq!(f(vec2(0.1, 0.01)), SnapFamily::HorizontalEighths);
        assert_eq!(f(vec2(0.0, 0.2)), SnapFamily::VerticalEighths);
        assert_eq!(f(vec2(0.0, 0.0)), SnapFamily::HorizontalEighths);
        // family agreement: every neighborhood cell of one square must
        // pick the same family (grids are integer-aligned). Sweep
        // near-axis and mixed offsets; near-axis positions are where a
        // snap that loses the integer row/col offset would disagree.
        for pos in [
            vec2(0.363, 0.184),
            vec2(0.123, 0.064),
            vec2(0.05, 0.3),
            vec2(0.25, 0.05),
            vec2(-0.4, -0.2),
        ] {
            for dx in -1..=1 {
                for dy in -1..=1 {
                    assert_eq!(
                        f(pos - vec2(dx, dy).to_f32()),
                        f(pos),
                        "neighbor ({dx}, {dy}) of {pos:?} disagreed"
                    );
                }
            }
        }
    }

    #[test]
    fn test_2d_offset_uses_one_family_for_both_chars() {
        // the reported tearing case, pos frac (0.363, 0.184): v-eighths
        // family, center snapped to (0.5, 1/8) - the square sits entirely
        // in the right half-cell
        assert_eq!(
            characters_for_full_square_with_2d_offset(vec2(0.363, 0.184)),
            [' ', '🮆']
        );
        // row above: same family, y snapped to -7/8
        assert_eq!(
            characters_for_full_square_with_2d_offset(vec2(0.363, -0.816)),
            [' ', '▁']
        );
        // hextant family keeps both chars non-empty and third-consistent
        assert_eq!(
            characters_for_full_square_with_2d_offset(vec2(0.25, 0.3)),
            ['🬉', '🬎']
        );
    }

    #[test]
    fn test_offset_full_square() {
        let f = characters_for_full_square_with_1d_offset;
        assert_eq!(f(STEP_UP.into(), 0.5), [UPPER_HALF_BLOCK; 2], "Basic up");
        assert_eq!(
            f(STEP_UP.into(), 1.0 / 3.0),
            [UPPER_TWO_THIRD_BLOCK; 2],
            "1/3 up"
        );
        assert_eq!(
            f(STEP_RIGHT.into(), 0.25),
            [RIGHT_HALF_BLOCK, FULL_BLOCK],
            "right"
        );
        assert_eq!(
            f(STEP_LEFT.into(), 0.25),
            [FULL_BLOCK, LEFT_HALF_BLOCK],
            "left"
        );
        assert_eq!(
            f(STEP_RIGHT.into(), 0.75),
            [SPACE, RIGHT_HALF_BLOCK],
            "right more"
        );
        for i in 0..20 {
            assert_eq!(
                f(STEP_RIGHT.into(), 0.1 * i as f32),
                f(STEP_LEFT.into(), -0.1 * i as f32),
                "negative equivalence horizontally.  i={}",
                i
            );
            assert_eq!(
                f(STEP_DOWN.into(), 0.1 * i as f32),
                f(STEP_UP.into(), -0.1 * i as f32),
                "negative equivalence vertically.  i={}",
                i
            );
        }
        assert_eq!(
            f(STEP_RIGHT.into(), 9.75),
            [SPACE, SPACE],
            "No wraparound right"
        );
        assert_eq!(
            f(STEP_LEFT.into(), 9.75),
            [SPACE, SPACE],
            "No wraparound left"
        );
        assert_eq!(
            f(STEP_RIGHT.into(), -9.75),
            [SPACE, SPACE],
            "No wraparound negative right"
        );
    }
    #[test]
    fn test_one_third_height_single_character() {
        assert_eq!(
            character_for_half_square_with_1d_offset(true, 2.0 / 3.0),
            UPPER_ONE_THIRD_BLOCK
        );
        assert_eq!(
            character_for_half_square_with_1d_offset(true, 1.0 / 3.0),
            UPPER_TWO_THIRD_BLOCK
        );
        assert_eq!(
            character_for_half_square_with_1d_offset(true, -1.0 / 3.0),
            LOWER_TWO_THIRD_BLOCK
        );
        assert_eq!(
            character_for_half_square_with_1d_offset(true, -2.0 / 3.0),
            LOWER_ONE_THIRD_BLOCK
        );
    }
    #[test]
    fn test_offset_full_square_looping() {
        let f = characters_for_full_square_with_looping_1d_offset;
        for i in 0..20 {
            assert_eq!(
                f(STEP_RIGHT.into(), 0.1 * i as f32),
                f(STEP_RIGHT.into(), 0.1 * i as f32 + 2.0),
                "modulo.  i={}",
                i
            );
            assert_eq!(
                f(STEP_DOWN.into(), 0.1 * i as f32),
                f(STEP_DOWN.into(), 0.1 * i as f32 + 22.0),
                "modulo. i={}",
                i
            );
        }
        assert_eq!(f(STEP_RIGHT.into(), 0.3), f(STEP_LEFT.into(), 1.7),);
        assert_eq!(
            f(STEP_UP.into(), 0.3),
            f(STEP_UP.into(), -1.7),
            "negative equivalence"
        );
        assert_eq!(f(STEP_RIGHT.into(), 1.25), [LEFT_HALF_BLOCK, SPACE]);
        assert_eq!(f(STEP_LEFT.into(), 1.25), [SPACE, RIGHT_HALF_BLOCK]);
    }
    #[test]
    fn test_characters_for_full_square_with_2d_offset() {
        let f = characters_for_full_square_with_2d_offset;
        KING_STEPS
            .iter()
            .for_each(|step| assert_eq!(f(step.to_f32()), [SPACE; 2]));
        assert_eq!(f(STEP_ZERO.to_f32()), [FULL_BLOCK; 2]);
        assert_eq!(f(vec2(0.5, 0.0)), [SPACE, FULL_BLOCK]);
        assert_eq!(f(vec2(-0.5, 0.0)), [FULL_BLOCK, SPACE]);
        assert_eq!(
            f(vec2(1.0 / 16.0, 0.0)),
            [RIGHT_SEVEN_EIGHTHS_BLOCK, FULL_BLOCK]
        );
    }
}
