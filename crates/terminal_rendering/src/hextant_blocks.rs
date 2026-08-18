use super::glyph_constants::*;
use crate::glyph::{DoubleGlyph, Glyph};
use rgb::RGB8;
use utility::coordinate_frame_conversions::{
    WorldSquare,
    WorldPoint,
};
use crate::screen::LocalCharacterPoint;
use utility::IVector;
use euclid::{point2, Point2D};
use std::collections::{HashMap, HashSet};

pub const FIRST_HEXTANT: char = '🬀';
pub const LAST_HEXTANT: char = '🬻';

pub struct HextantGridInWorldFrame;
pub struct HextantGridInLocalFrame;
pub type WorldHextantSquare = Point2D<i32, HextantGridInWorldFrame>;
pub type WorldHextantPoint = Point2D<f32, HextantGridInWorldFrame>;
pub type LocalHextantSquare = Point2D<i32, HextantGridInLocalFrame>;
pub type LocalHextantPoint = Point2D<f32, HextantGridInLocalFrame>;

pub type HextantArray = [[bool; 2]; 3]; // row, column

// all hextant blocks
// U+1FB0x 🬀 🬁 🬂 🬃 🬄 🬅 🬆 🬇 🬈 🬉 🬊 🬋 🬌 🬍 🬎 🬏
// U+1FB1x 🬐 🬑 🬒 🬓 🬔 🬕 🬖 🬗 🬘 🬙 🬚 🬛 🬜 🬝 🬞 🬟
// U+1FB2x 🬠 🬡 🬢 🬣 🬤 🬥 🬦 🬧 🬨 🬩 🬪 🬫 🬬 🬭 🬮 🬯
// U+1FB3x 🬰 🬱 🬲 🬳 🬴 🬵 🬶 🬷 🬸 🬹 🬺 🬻
// 2^6 = 64 = 4* 16
// missing hextant blocks: empty, left half, right half, full

pub fn hextant_block_by_offset(hextant_grid_steps: IVector) -> char {
    match hextant_grid_steps.to_tuple() {
        (1, -2) => '🬞',
        (1, -1) => '🬦',
        (1, 0) => RIGHT_HALF_BLOCK,
        (1, 1) => '🬉',
        (1, 2) => '🬁',
        (0, -2) => LOWER_ONE_THIRD_BLOCK,
        (0, -1) => LOWER_TWO_THIRD_BLOCK,
        (0, 0) => FULL_BLOCK,
        (0, 1) => UPPER_TWO_THIRD_BLOCK,
        (0, 2) => UPPER_ONE_THIRD_BLOCK,
        (-1, -2) => '🬏',
        (-1, -1) => '🬓',
        (-1, 0) => LEFT_HALF_BLOCK,
        (-1, 1) => '🬄',
        (-1, 2) => '🬀',
        _ => SPACE,
    }
}

const HEX_SPACE: u8 = const { hextant_character_to_binary(SPACE) };
const HEX_LEFT: u8 = const { hextant_character_to_binary(LEFT_HALF_BLOCK) };
const HEX_RIGHT: u8 = const { hextant_character_to_binary(RIGHT_HALF_BLOCK) };
const HEX_FULL: u8 = const { hextant_character_to_binary(FULL_BLOCK) };

pub fn hextant_array_to_char(hextant_array: HextantArray) -> char {
    let as_binary = hextant_array_to_binary(hextant_array);
    let _before_half_left = '🬓';
    let _after_half_left = '🬔';
    let _before_half_right = '🬧';
    let _after_half_right = '🬨';

    match as_binary {
        HEX_SPACE => SPACE,
        HEX_LEFT => LEFT_HALF_BLOCK,
        HEX_RIGHT => RIGHT_HALF_BLOCK,
        HEX_FULL => FULL_BLOCK,
        _ => {
            let unadjusted_value = FIRST_HEXTANT as u32 + as_binary as u32;
            let offset = if unadjusted_value
                < hextant_character_to_value_it_damn_well_should_have(LEFT_HALF_BLOCK)
            {
                1
            } else if unadjusted_value
                < hextant_character_to_value_it_damn_well_should_have(RIGHT_HALF_BLOCK)
            {
                2
            } else {
                3
            };
            char::from_u32(unadjusted_value - offset).unwrap()
        }
    }
}

fn binary_to_hextant_char(binary: u8) -> char {
    hextant_array_to_char(binary_to_hextant_array(binary))
}

fn local_character_point_to_local_hextant_point(
    local_character_point: LocalCharacterPoint,
) -> LocalHextantPoint {
    // the origin hextant square is the lower left square of a character
    // (0,0) -> (0.5, 1.0)
    // (-0.25, -1/3) -> (0,0)

    point2(
        (local_character_point.x + 0.25) * 2.0,
        (local_character_point.y + 1.0 / 3.0) * 3.0,
    )
}

pub fn snap_to_hextant_grid(point: WorldPoint) -> WorldPoint {
    let hx = 4.0;
    let dx = 1.0 / 8.0;
    let hy = 3.0;
    point2(
        ((point.x + dx) * hx).round() / hx - dx,
        (point.y * hy).round() / hy,
    )
}

pub const fn char_is_hextant(character: char) -> bool {
    character == SPACE
        || character == LEFT_HALF_BLOCK
        || character == RIGHT_HALF_BLOCK
        || character == FULL_BLOCK
        || (FIRST_HEXTANT <= character && character <= LAST_HEXTANT)
}

fn hextant_array_to_binary(hextant_array: HextantArray) -> u8 {
    let mut out = 0;
    for row in 0..hextant_array.len() {
        for column in 0..hextant_array[row].len() {
            let position_value = 4u8.pow(row as u32) * 2u8.pow(column as u32);
            if hextant_array[row][column] {
                out += position_value;
            }
        }
    }
    out
}

fn binary_to_hextant_array(mut binary: u8) -> HextantArray {
    let mut out: HextantArray = [[false; 2]; 3];
    for row in 0..3 {
        for column in 0..2 {
            let this_bit_is_set = binary % 2 == 1;
            binary /= 2;
            out[row][column] = this_bit_is_set;
        }
    }
    out
}

/// Public for the floating-square coherence tests' coverage model; not
/// intended as general API.
#[doc(hidden)]
pub const fn hextant_character_to_binary(hextant_character: char) -> u8 {
    assert!(char_is_hextant(hextant_character));
    let before_half_left = '🬓';
    let before_half_right = '🬧';
    match hextant_character {
        SPACE => 0,
        LEFT_HALF_BLOCK => 1 + 4 + 16,
        RIGHT_HALF_BLOCK => 2 + 8 + 32,
        FULL_BLOCK => 1 + 2 + 4 + 8 + 16 + 32,
        _ => {
            let raw_value = hextant_character as u32;
            let offset = if raw_value <= before_half_left as u32 {
                1
            } else if raw_value <= before_half_right as u32 {
                2
            } else {
                3
            };
            (raw_value - FIRST_HEXTANT as u32 + offset) as u8
        }
    }
}

pub fn combine_hextant_characters(a: char, b: char) -> char {
    assert!(char_is_hextant(a));
    assert!(char_is_hextant(b));
    binary_to_hextant_char(hextant_character_to_binary(a) | hextant_character_to_binary(b))
}

fn hextant_character_to_value_it_damn_well_should_have(character: char) -> u32 {
    // If its empty, full, and horizontal halfblocks weren't already taken
    assert!(char_is_hextant(character));
    FIRST_HEXTANT as u32 + hextant_character_to_binary(character) as u32
}

fn local_hextant_squares_to_char(local_hextant_squares: HashSet<LocalHextantSquare>) -> char {
    let init_hex_array: HextantArray = [[false; 2]; 3];
    let final_hex_array = local_hextant_squares.into_iter().fold(
        init_hex_array,
        |mut array, local_hextant_square| {
            // column is x coordinate, row is flipped y coordinate
            assert!(local_hextant_square.x >= 0 && local_hextant_square.x <= 1);
            assert!(local_hextant_square.y >= 0 && local_hextant_square.y <= 2);
            let row: usize = (2 - local_hextant_square.y) as usize;
            let column: usize = local_hextant_square.x as usize;
            array[row][column] = true;
            array
        },
    );
    hextant_array_to_char(final_hex_array)
}

/// Bins points into left/right 2x3 hextant grids per world square, skipping
/// the deprecated world character grid. The world-square key and character
/// index reuse the old path's exact arithmetic (div/rem_euclid on the
/// rounded character x) so binning is identical by construction —
/// round-half-away-from-zero makes naive sign/fraction rules disagree at
/// negative exact-integer x. Character-level equivalence enforced by
/// `test_direct_hextant_binning_matches_paired_char_grid`.
pub fn points_to_hextant_double_glyphs(
    points: Vec<WorldPoint>,
    color: RGB8,
) -> HashMap<WorldSquare, DoubleGlyph> {
    let mut dots: HashMap<WorldSquare, [HashSet<LocalHextantSquare>; 2]> = HashMap::new();
    for point in points {
        let char_point_x = point.x * 2.0 + 0.5;
        // euclid's Point2D::round (used by the old path) is (x+0.5).floor(),
        // NOT f32::round — they disagree at negative half-integers, which
        // flips the left/right character index
        let char_x = (char_point_x + 0.5).floor() as i32;
        // same rounding rule as char_x (euclid half-up), on both axes
        let world_square: WorldSquare = point2(char_x.div_euclid(2), (point.y + 0.5).floor() as i32);
        let char_index = char_x.rem_euclid(2) as usize;
        // local point within the character, matching
        // world_point_to_local_character_point's arithmetic
        let local_char_point =
            LocalCharacterPoint::new(char_point_x - char_x as f32, point.y - world_square.y as f32);
        let dot = local_character_point_to_local_hextant_point(local_char_point)
            .round()
            .to_i32();
        dots.entry(world_square).or_default()[char_index].insert(dot);
    }
    dots.into_iter()
        .map(|(square, halves)| {
            // empty half -> transparent glyph, matching the old
            // pair_up_character_square_map(.., Glyph::transparent_glyph())
            (square, halves.map(|set| {
                if set.is_empty() {
                    Glyph::transparent_glyph()
                } else {
                    Glyph::fg_only(local_hextant_squares_to_char(set), color)
                }
            }))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use ntest::assert_about_eq;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_hextant_array_to_char() {
        assert_eq!(
            hextant_array_to_char([[false, false], [false, false], [false, false],]),
            SPACE
        );
        assert_eq!(
            hextant_array_to_char([[true, true], [true, true], [true, true],]),
            FULL_BLOCK
        );
        assert_eq!(
            hextant_array_to_char([[true, false], [true, false], [true, false],]),
            LEFT_HALF_BLOCK
        );
        assert_eq!(
            hextant_array_to_char([[false, true], [false, true], [false, true],]),
            RIGHT_HALF_BLOCK
        );
        assert_eq!(
            hextant_array_to_char([[true, false], [false, false], [false, false],]),
            '🬀'
        );
        assert_eq!(
            hextant_array_to_char([[false, true], [true, true], [false, false],]),
            '🬍'
        );
    }

    #[test]
    fn test_points_to_hextant_chars() {
        // 00
        // 00
        // 01
        //
        // 01 10  00 00
        // 11 00  00 10
        // 01 10  00 00

        let points: Vec<WorldPoint> = vec![
            // lower left
            point2(-0.1, 0.0),
            point2(-0.3, 0.0),
            point2(-0.24, 1.0 / 6.0 + 0.1),
            point2(-0.2, -0.4),
            // upper
            point2(-0.2, 0.7),
            // one right
            point2(0.01, 0.167),
            point2(0.01, -0.467),
            // far right
            point2(1.1, 0.0),
        ];

        // world-square keyed: [left, right] characters per square (' ' is
        // the transparent_glyph character for empty halves)
        let glyphs = points_to_hextant_double_glyphs(points, RGB8::new(1, 2, 3));
        let chars: HashMap<WorldSquare, [char; 2]> = glyphs
            .into_iter()
            .map(|(square, half)| (square, half.map(|g| g.character)))
            .collect();

        assert_eq!(chars.len(), 3);
        assert_eq!(chars.get(&point2(0, 0)).unwrap(), &['🬫', '🬐']);
        assert_eq!(chars.get(&point2(0, 1)).unwrap(), &['🬞', ' ']);
        assert_eq!(chars.get(&point2(1, 0)).unwrap(), &[' ', '🬃']);
    }

    #[test]
    fn test_local_character_point_to_local_hextant_point() {
        let char_point1 = LocalCharacterPoint::new(0.0, 0.0);
        let char_point2 = LocalCharacterPoint::new(-0.25, -1.0 / 3.0);

        let hextant_point1 = local_character_point_to_local_hextant_point(char_point1);
        let hextant_point2 = local_character_point_to_local_hextant_point(char_point2);

        assert_about_eq!(hextant_point1.x, 0.5);
        assert_about_eq!(hextant_point1.y, 1.0);
        assert_about_eq!(hextant_point2.x, 0.0);
        assert_about_eq!(hextant_point2.y, 0.0);
    }

    #[test]
    fn test_snap_to_hextant_grid() {
        let snapped = snap_to_hextant_grid(point2(0.1, 0.0));
        assert_about_eq!(snapped.x, 1.0 / 8.0);
        assert_about_eq!(snapped.y, 0.0);

        let snapped = snap_to_hextant_grid(point2(-0.1, -0.4));
        assert_about_eq!(snapped.x, -1.0 / 8.0);
        assert_about_eq!(snapped.y, -1.0 / 3.0);
    }

    #[test]
    fn test_hextant_array_to_binary_and_back() {
        let arrays: Vec<HextantArray> = vec![
            [[false, false], [false, false], [false, false]],
            [[true, true], [true, true], [true, true]],
            [[false, false], [true, false], [false, false]],
            [[false, false], [true, false], [false, true]],
        ];

        for array in arrays {
            assert_eq!(
                array,
                binary_to_hextant_array(hextant_array_to_binary(array))
            );
        }
    }

}
