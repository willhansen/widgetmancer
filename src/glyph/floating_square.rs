use crate::glyph::glyph_constants::*;
use crate::utility::*;
use euclid::point2;
use num::clamp;
use rgb::*;

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
            output[i][c] = Some(character_of_square_with_offset(false, x_offset - x as f32));
        }
    }

    return output;
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
            output[c][j] = Some(character_of_square_with_offset(true, y_offset - y as f32));
        }
    }
    return output;
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
                let character = square_with_half_step_offset((grid_offset - square.to_f32()));
                if character != ' ' {
                    output[i][j] = Some(character);
                }
            }
        }
    }
    return output;
}

pub fn character_of_square_with_offset(is_vertical: bool, fraction_of_square_offset: f32) -> char {
    if fraction_of_square_offset < 0.0 {
        partial_block(is_vertical, 1.0 + fraction_of_square_offset)
    } else {
        partial_block(is_vertical, fraction_of_square_offset)
    }
}

pub fn square_with_half_step_offset(offset: FVector) -> char {
    let step: IVector = (offset * 2.0).round().to_i32();
    quadrant_block_by_offset(step)
}

pub fn partial_block(vertical: bool, fraction: f32) -> char {
    let eighths = (fraction * 8.0).round() as usize;
    let clamped_eighths = clamp(eighths, 0, 8);
    if vertical {
        EIGHTH_BLOCKS_FROM_BOTTOM[clamped_eighths]
    } else {
        EIGHTH_BLOCKS_FROM_LEFT[clamped_eighths]
    }
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
        assert_eq!(character_of_square_with_offset(false, 0.0), FULL_BLOCK);
    }

    #[test]
    fn test_character_square_horizontal_offset__round_to_zero() {
        assert_eq!(character_of_square_with_offset(false, -0.001), FULL_BLOCK);

        assert_eq!(character_of_square_with_offset(false, 0.001), FULL_BLOCK);
    }

    #[test]
    fn test_character_square_horizontal_offset__out_of_range() {
        assert_eq!(character_of_square_with_offset(false, -1.5), SPACE);
        assert_eq!(character_of_square_with_offset(false, 1.5), SPACE);
    }

    #[test]
    fn test_character_square_horizontal_offset__halfway() {
        assert_eq!(character_of_square_with_offset(false, -0.5), '▌');
        assert_eq!(
            character_of_square_with_offset(false, 0.5),
            Glyph::new('▌', BLACK, RED)
        );
    }

    #[test]
    fn test_character_square_horizontal_offset__match_opposite_ends() {
        assert!(character_of_square_with_offset(false, -1.0).looks_solid_color(BLACK));
        assert!(character_of_square_with_offset(false, 1.0).looks_solid_color(BLACK));
    }
}
