use crate::glyph::glyph_constants::*;
use crate::glyph::hextant_blocks::{hextant_array_to_char, hextant_block_by_offset};
use crate::glyph::DoubleChar;
use crate::utility::coordinate_frame_conversions::{
    world_character_point_to_world_character_square, CharacterGridInLocalCharacterFrame,
    WorldCharacterMove, WorldCharacterPoint, WorldCharacterSquare, WorldMove,
};
use crate::utility::*;
use euclid::{point2, vec2, Point2D};
use itertools::Itertools;
use num::clamp;
use ordered_float::OrderedFloat;
use rgb::*;
use std::collections::HashMap;

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

pub fn get_chars_for_floating_square(pos: FPoint) -> Vec<Vec<Option<char>>> {
    let grid_offset = fraction_part(pos);
    let x_offset = grid_offset.x;
    let y_offset = grid_offset.y;
    if y_offset.abs() < x_offset.abs() && y_offset.abs() < 0.25 {
        get_smooth_horizontal_chars_for_floating_square(pos)
    } else if x_offset.abs() < 0.25 {
        get_smooth_vertical_chars_for_floating_square(pos)
    } else {
        get_half_grid_chars_for_floating_square(pos)
    }
}

pub fn get_smooth_horizontal_chars_for_floating_square(pos: FPoint) -> Vec<Vec<Option<char>>> {
    let width = 3;
    let mut output = vec![vec![None; width]; width];

    let c = width / 2 as usize;

    let grid_offset = fraction_part(pos);
    let x_offset = grid_offset.x;
    let offset_dir: IPoint = sign2d(grid_offset).to_i32();

    for i in 0..3 {
        let x = i as i32 - 1;
        if offset_dir.x == x || x == 0 {
            output[i][c] = Some(character_for_half_square_with_1d_offset(
                false,
                x_offset - x as f32,
            ));
        }
    }

    output
}
pub fn get_smooth_vertical_chars_for_floating_square(pos: FPoint) -> Vec<Vec<Option<char>>> {
    let width = 3;
    let mut output = vec![vec![None; width]; width];

    let c = width / 2 as usize;

    let grid_offset = fraction_part(pos);
    let y_offset = grid_offset.y;
    let offset_dir: IPoint = sign2d(grid_offset).to_i32();
    for j in 0..3 {
        let y = j as i32 - 1;
        if offset_dir.y == y || y == 0 {
            output[c][j] = Some(character_for_half_square_with_1d_offset(
                true,
                y_offset - y as f32,
            ));
        }
    }
    output
}

pub fn get_half_grid_chars_for_floating_square(pos: FPoint) -> Vec<Vec<Option<char>>> {
    let width = 3;
    let mut output = vec![vec![None; width]; width];
    let grid_offset = fraction_part(pos);
    let offset_dir = sign2d(grid_offset).to_i32();

    for i in 0..3 {
        for j in 0..3 {
            let x = i as i32 - 1;
            let y = j as i32 - 1;
            let square = point2(x, y);
            if (offset_dir.x == x || x == 0) && (offset_dir.y == y || y == 0) {
                let character = square_with_half_step_offset(grid_offset - square.to_f32());
                if character != ' ' {
                    output[i][j] = Some(character);
                }
            }
        }
    }
    output
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

pub fn character_for_half_square_with_2d_offset(offset: FVector) -> char {
    // start with basic centered square
    let mut snap_points_with_characters: Vec<(FVector, char)> = vec![(vec2(0.0, 0.0), FULL_BLOCK)];

    // the eighth steps along the axes
    let mut horizontal_snap_points_at_eighths: Vec<(FVector, char)> = (-8..=8)
        .map(|i| {
            (
                vec2(i as f32 / 8.0, 0.0),
                character_for_half_square_with_1d_eighths_offset(false, i),
            )
        })
        .collect();
    snap_points_with_characters.append(&mut horizontal_snap_points_at_eighths);

    let mut vertical_snap_points_at_eighths: Vec<(FVector, char)> = (-8..=8)
        .map(|i| {
            (
                vec2(0.0, i as f32 / 8.0),
                character_for_half_square_with_1d_eighths_offset(true, i),
            )
        })
        .collect();
    snap_points_with_characters.append(&mut vertical_snap_points_at_eighths);

    // the one third steps vertically, with horizontal half-square offsets
    let mut hextant_snap_points: Vec<(FVector, char)> = (-2..=2)
        .flat_map(|x| {
            (-3..=3).map(move |y| {
                (
                    vec2(x as f32 / 2.0, y as f32 / 3.0),
                    hextant_block_by_offset(vec2(x, y)),
                )
            })
        })
        .collect();
    snap_points_with_characters.append(&mut hextant_snap_points);

    // the half square grid offsets
    let mut quadrant_snap_points: Vec<(FVector, char)> = (-2..=2)
        .flat_map(|x| {
            (-2..=2).map(move |y| {
                (
                    vec2(x as f32 / 2.0, y as f32 / 2.0),
                    quadrant_block_by_offset(vec2(x, y)),
                )
            })
        })
        .collect();
    snap_points_with_characters.append(&mut quadrant_snap_points);

    // remove duplicate snap points
    // TODO
    //snap_points_with_characters = snap_points_with_characters
    //.into_iter()
    //.unique_by(|(point, char)| (OrderedFloat(point.x), OrderedFloat(point.y), char))
    //.collect();

    *snap_points_with_characters
        .iter()
        .min_by_key(|(snap_point, _character)| OrderedFloat((*snap_point - offset).length()))
        .map(|(_snap_point, character)| character)
        .unwrap()
}

#[deprecated(note = "use drawables_for_floating_square_at_point instead")]
pub fn character_map_for_full_square_at_point(
    point: WorldCharacterPoint,
) -> HashMap<WorldCharacterSquare, char> {
    let mut output_characters = HashMap::<WorldCharacterSquare, char>::new();
    let center_square = world_character_point_to_world_character_square(point);
    (-2..=2).for_each(|dx| {
        (-1..=1).for_each(|dy| {
            let step = vec2(dx, dy);
            let square = center_square + step;
            let center_of_square = square.to_f32();
            let mut offset_from_square_center = point - center_of_square;

            //Tweak offset for effectively double width
            let inward_shifted_x_offset = (offset_from_square_center.x.abs() - 0.5).max(0.0)
                * sign(offset_from_square_center.x);
            offset_from_square_center.x = inward_shifted_x_offset;

            let character_for_square =
                character_for_half_square_with_2d_offset(offset_from_square_center.cast_unit());
            if character_for_square != SPACE {
                output_characters.insert(square, character_for_square);
            }
        })
    });
    output_characters
}

pub fn characters_for_full_square_with_2d_offset(offset: WorldMove) -> DoubleChar {
    let char_offsets = [-1.0, 1.0].map(|i| {
        let scaled_x_offset = offset.x * 2.0;
        let shifted_toward_this_side = sign(scaled_x_offset) == i;
        let compensated_x_offset = if shifted_toward_this_side {
            (scaled_x_offset.abs() - 1.0).max(0.0) * sign(scaled_x_offset)
        } else {
            scaled_x_offset
        };
        vec2(compensated_x_offset, offset.y)
    });
    char_offsets.map(|char_offset| character_for_half_square_with_2d_offset(char_offset))
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
    use crate::glyph::DoubleChar;
    use euclid::vec2;
    use ntest::timeout;

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

    #[test]

    fn test_half_grid_glyph_when_rounding_to_zero_for_both_axes() {
        let test_pos = point2(-0.24, 0.01);
        let chars = get_half_grid_chars_for_floating_square(test_pos);
        assert_eq!(chars[0][0], None);
        assert_eq!(chars[0][1], None);
        assert_eq!(chars[0][2], None);
        assert_eq!(chars[1][0], None);
        assert_eq!(chars[1][1].unwrap(), quadrant_block_by_offset(vec2(0, 0)));
        assert_eq!(chars[1][2], None);
        assert_eq!(chars[2][0], None);
        assert_eq!(chars[2][1], None);
        assert_eq!(chars[2][2], None);
    }

    #[test]

    fn test_half_grid_chars_when_rounding_to_zero_for_x_and_half_step_up_for_y() {
        let test_pos = point2(0.24, 0.26);
        let chars = get_half_grid_chars_for_floating_square(test_pos);
        assert_eq!(chars[0][0], None);
        assert_eq!(chars[0][1], None);
        assert_eq!(chars[0][2], None);
        assert_eq!(chars[1][0], None);
        assert_eq!(chars[1][1].unwrap(), quadrant_block_by_offset(vec2(0, 1)));
        assert_eq!(chars[1][2].unwrap(), quadrant_block_by_offset(vec2(0, -1)));
        assert_eq!(chars[2][0], None);
        assert_eq!(chars[2][1], None);
        assert_eq!(chars[2][2], None);
    }

    #[test]

    fn test_half_grid_chars_when_rounding_to_zero_for_x_and_exactly_half_step_up_for_y() {
        let test_pos = point2(0.24, 0.25);

        let chars = get_half_grid_chars_for_floating_square(test_pos);
        assert_eq!(chars[0][0], None);
        assert_eq!(chars[0][1], None);
        assert_eq!(chars[0][2], None);
        assert_eq!(chars[1][0], None);
        assert_eq!(chars[1][1].unwrap(), quadrant_block_by_offset(vec2(0, 1)));
        assert_eq!(chars[1][2].unwrap(), quadrant_block_by_offset(vec2(0, -1)));
        assert_eq!(chars[2][0], None);
        assert_eq!(chars[2][1], None);
        assert_eq!(chars[2][2], None);
    }

    #[test]

    fn test_half_grid_chars_when_rounding_to_zero_for_x_and_exactly_half_step_down_for_y() {
        let test_pos = point2(-0.2, -0.25);
        let chars = get_half_grid_chars_for_floating_square(test_pos);
        assert_eq!(chars[0][0], None);
        assert_eq!(chars[0][1], None);
        assert_eq!(chars[0][2], None);
        assert_eq!(chars[1][0], None);
        assert_eq!(chars[1][1].unwrap(), quadrant_block_by_offset(vec2(0, 0)));
        assert_eq!(chars[1][2], None);
        assert_eq!(chars[2][0], None);
        assert_eq!(chars[2][1], None);
        assert_eq!(chars[2][2], None);
    }

    #[test]

    fn test_half_grid_chars_when_rounding_to_zero_for_y_and_half_step_right_for_x() {
        let test_pos = point2(0.3, 0.1);
        let chars = get_half_grid_chars_for_floating_square(test_pos);
        assert_eq!(chars[0][0], None);
        assert_eq!(chars[0][1], None);
        assert_eq!(chars[0][2], None);
        assert_eq!(chars[1][0], None);
        assert_eq!(chars[1][1].unwrap(), quadrant_block_by_offset(vec2(1, 0)));
        assert_eq!(chars[1][2], None);
        assert_eq!(chars[2][0], None);
        assert_eq!(chars[2][1].unwrap(), quadrant_block_by_offset(vec2(-1, 0)));
        assert_eq!(chars[2][2], None);
    }

    #[test]

    fn test_half_grid_chars_when_rounding_to_zero_for_y_and_half_step_left_for_x() {
        let test_pos = point2(-0.3, 0.2);
        let chars = get_half_grid_chars_for_floating_square(test_pos);
        assert_eq!(chars[0][0], None);
        assert_eq!(chars[0][1].unwrap(), quadrant_block_by_offset(vec2(1, 0)));
        assert_eq!(chars[0][2], None);
        assert_eq!(chars[1][0], None);
        assert_eq!(chars[1][1].unwrap(), quadrant_block_by_offset(vec2(-1, 0)));
        assert_eq!(chars[1][2], None);
        assert_eq!(chars[2][0], None);
        assert_eq!(chars[2][1], None);
        assert_eq!(chars[2][2], None);
    }

    //                      |<--halfway
    // ' ', '▏', '▎', '▍', '▌', '▋', '▊', '▉', '█'
    #[test]

    fn test_character_square_horizontal_offset__base_case() {
        assert_eq!(
            character_for_half_square_with_1d_offset(false, 0.0),
            FULL_BLOCK
        );
    }

    #[test]

    fn test_character_square_horizontal_offset__round_to_zero() {
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

    fn test_character_square_horizontal_offset__out_of_range() {
        assert_eq!(character_for_half_square_with_1d_offset(false, -1.5), SPACE);
        assert_eq!(character_for_half_square_with_1d_offset(false, 1.5), SPACE);
    }

    #[test]

    fn test_character_square_horizontal_offset__halfway() {
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

    fn test_character_square_horizontal_offset__match_opposite_ends() {
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

    fn test_2d_square_offset() {
        assert_eq!(
            character_for_half_square_with_2d_offset(vec2(0.0, 0.0)),
            FULL_BLOCK
        );
        assert_eq!(
            character_for_half_square_with_2d_offset(vec2(0.001, 0.0)),
            FULL_BLOCK
        );
        assert_eq!(
            character_for_half_square_with_2d_offset(vec2(0.0, -0.01)),
            FULL_BLOCK
        );

        assert_eq!(
            character_for_half_square_with_2d_offset(vec2(0.25, -0.01)),
            EIGHTH_BLOCKS_FROM_RIGHT[6]
        );

        assert_eq!(
            character_for_half_square_with_2d_offset(vec2(1.25, -0.5)),
            SPACE
        );
    }

    #[test]

    fn test_chars_for_floating_square__at_origin() {
        let chars = character_map_for_full_square_at_point(point2(0.0, 0.0));
        assert_eq!(chars.len(), 3);
        assert_eq!(chars.get(&point2(0, 0)), Some(&FULL_BLOCK));
        assert_eq!(chars.get(&point2(-1, 0)), Some(&RIGHT_HALF_BLOCK));
        assert_eq!(chars.get(&point2(1, 0)), Some(&LEFT_HALF_BLOCK));
    }
    #[test]

    fn test_chars_for_floating_square__at_square_center() {
        let chars = character_map_for_full_square_at_point(point2(0.5, 0.0));
        assert_eq!(chars.len(), 2);
        assert_eq!(chars.get(&point2(0, 0)), Some(&FULL_BLOCK));
        assert_eq!(chars.get(&point2(1, 0)), Some(&FULL_BLOCK));
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
