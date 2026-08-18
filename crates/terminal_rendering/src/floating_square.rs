use crate::glyph_constants::*;
use crate::hextant_blocks::hextant_block_by_offset;
use crate::DoubleChar;
use utility::coordinate_frame_conversions::{WorldMove,};
use utility::*;
use euclid::vec2;
use ordered_float::OrderedFloat;

/// Glyph for the part of a 1x1 square, offset by `half_steps` (x and y in
/// half-cell halves), that overlaps one half-cell. Generated like
/// `hextant_block_by_offset`: the overlap is a rectangle of quadrants.
pub fn quadrant_block_by_offset(half_steps: IVector) -> char {
    let cols: [bool; 2] = match half_steps.x {
        -1 => [true, false],
        0 => [true, true],
        1 => [false, true],
        _ => return SPACE,
    };
    let rows: [bool; 2] = match half_steps.y {
        -1 => [false, true],
        0 => [true, true],
        1 => [true, false],
        _ => return SPACE,
    };
    match (cols, rows) {
        ([true, true], [true, true]) => FULL_BLOCK,
        ([true, false], [true, true]) => LEFT_HALF_BLOCK,
        ([false, true], [true, true]) => RIGHT_HALF_BLOCK,
        ([true, true], [true, false]) => UPPER_HALF_BLOCK,
        ([true, true], [false, true]) => LOWER_HALF_BLOCK,
        ([true, false], [true, false]) => '▘',
        ([false, true], [true, false]) => '▝',
        ([true, false], [false, true]) => '▖',
        ([false, true], [false, true]) => '▗',
        _ => SPACE,
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

    /// Snap errors closer than this are treated as tied. The error is
    /// mathematically identical for every cell of a square (translation-
    /// invariant snapping), but float subtraction noise (~1e-7) breaks
    /// exact ties differently per cell: at decision boundaries like
    /// (x=1/8, y=7/24), where hextant and v-eighths are exactly
    /// equidistant, cells picked different families and tore the
    /// silhouette. With the tolerance, all cells find the same tied set
    /// and the fixed `SnapFamily::ALL` priority order resolves it
    /// identically. Positions whose family gap lands within float noise
    /// (~2e-6) of exactly EPSILON could still disagree — a ~1e-6-wide
    /// shell, watched by the dense offset-plane sweep test.
    const SNAP_ERROR_TIE_EPSILON: f32 = 1e-3;

    /// Every snap grid is integer-aligned, so all cells of a square agree
    /// on the family no matter which cell's offset is passed. The decision
    /// comes from the baked map (see family_map.rs): offline-scored by
    /// measured coverage error, folded by absolute value — bit-identical
    /// for every cell of a square by construction.
    fn for_offset(o: FVector) -> SnapFamily {
        SnapFamily::ALL[crate::family_map::family_index_for_offset(o)]
    }

    /// Live selection by snap-error proxy, with epsilon tie-breaking. Not
    /// the render path (that's the baked map); kept for the hysteresis
    /// margin (needs per-family error values at runtime) and for tests
    /// cross-checking the map. Errors are compared in world units, which
    /// are already visually isotropic (1 unit = 2 columns horizontally,
    /// 1 row ~ 2 column-widths vertically).
    fn for_offset_by_snap_error(o: FVector) -> SnapFamily {
        let min = SnapFamily::ALL
            .iter()
            .map(|f| f.snap_error(o))
            .fold(f32::INFINITY, f32::min);
        SnapFamily::ALL
            .into_iter()
            .find(|f| f.snap_error(o) <= min + Self::SNAP_ERROR_TIE_EPSILON)
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

/// Live proxy-based family selection, as an index into
/// `snap_family_names()`. Not the render path (that uses the baked map);
/// exposed for the map cross-check test and the hysteresis margin.
#[doc(hidden)]
pub fn family_index_by_snap_error(o: FVector) -> usize {
    let f = SnapFamily::for_offset_by_snap_error(o);
    SnapFamily::ALL.iter().position(|&x| x == f).unwrap()
}

/// Hysteresis margin, in world units: an incumbent snap family is kept
/// until the baked map's winner beats it by more than this. ~1/50 is
/// visually sub-perceptual; below it family switches (the animation pops)
/// get suppressed. Snap errors are identical for every cell of a square,
/// so biased selection stays coherent by construction.
pub const FAMILY_SWITCH_PENALTY: f32 = 0.02;

/// `characters_for_full_square_with_2d_offset` with hysteresis: returns
/// the rendered chars plus the picked family index (into
/// `snap_family_names()`), keeping `incumbent_family` unless the map
/// winner beats it by more than `FAMILY_SWITCH_PENALTY` in snap-error.
/// Feed the returned index back as the next call's incumbent.
///
/// Note this picks the family for a *single* offset; callers rendering a
/// whole square (the game-side `drawables_for_floating_square_at_point`)
/// must take the returned index and force it for every cell, e.g. via
/// `characters_for_full_square_with_2d_offset_forced`.
pub fn characters_for_full_square_with_2d_offset_biased(
    offset: WorldMove,
    incumbent_family: Option<usize>,
) -> (DoubleChar, usize) {
    let o = vec2(offset.x, offset.y);
    let winner = SnapFamily::for_offset(o);
    let picked = match incumbent_family {
        Some(inc) if SnapFamily::ALL[inc].snap_error(o)
            <= winner.snap_error(o) + FAMILY_SWITCH_PENALTY =>
        {
            SnapFamily::ALL[inc]
        }
        _ => winner,
    };
    let index = SnapFamily::ALL.iter().position(|&f| f == picked).unwrap();
    (characters_in_family(offset, picked), index)
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
        // proxy-based picks; the render path uses the baked map, which
        // agrees with the proxy everywhere except boundary ties (resolved
        // there by priority order)
        let f = SnapFamily::for_offset_by_snap_error;
        // the reported tearing case: v-eighths (0.5, 1/8) is closer than
        // hextant (1/4, 1/3) once y error is counted honestly
        assert_eq!(f(vec2(0.363, 0.184)), SnapFamily::VerticalEighths);
        // y residual near a third, x near a quarter -> hextant
        assert_eq!(f(vec2(0.25, 0.3)), SnapFamily::Hextant);
        // near-axis offsets pick the fine family of the other axis
        assert_eq!(f(vec2(0.1, 0.01)), SnapFamily::HorizontalEighths);
        assert_eq!(f(vec2(0.0, 0.2)), SnapFamily::VerticalEighths);
        assert_eq!(f(vec2(0.0, 0.0)), SnapFamily::HorizontalEighths);
        // neighbor agreement on the render path (baked map): the fold
        // must return the same map cell for every cell of a square
        let fm = SnapFamily::for_offset;
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
                        fm(pos - vec2(dx, dy).to_f32()),
                        fm(pos),
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

#[cfg(test)]
mod geometry_tests {
    use super::*;
    use crate::coverage::glyph_filled;

    /// Quadrant table vs analytic square-overlap rectangle; probes at
    /// quadrant centers. See the hextant equivalent in hextant_blocks.rs.
    #[test]
    fn test_quadrant_table_matches_square_overlap_geometry() {
        for x_steps in -1..=1 {
            for y_steps in -1..=1 {
                let c = quadrant_block_by_offset(vec2(x_steps, y_steps));
                let (fx0, fx1) = match x_steps {
                    -1 => (0.0, 0.5),
                    0 => (0.0, 1.0),
                    _ => (0.5, 1.0),
                };
                let (fy0, fy1) = match y_steps {
                    -1 => (0.0, 0.5),
                    0 => (0.0, 1.0),
                    _ => (0.5, 1.0),
                };
                for &fx in &[0.25, 0.75] {
                    for &fy in &[0.25, 0.75] {
                        let expected =
                            (fx0..fx1).contains(&fx) && (fy0..fy1).contains(&fy);
                        assert_eq!(
                            glyph_filled(c, fx, fy),
                            expected,
                            "steps ({x_steps}, {y_steps}) -> {c:?} at ({fx}, {fy})"
                        );
                    }
                }
            }
        }
    }
}
