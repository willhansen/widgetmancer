use crate::glyph::glyph_constants::SPACE;
use crate::DoubleChar;
use euclid::{point2, Point2D};
use std::collections::HashMap;
use std::ops::BitXor;
use utility::coordinate_frame_conversions::*;
use utility::geometry2::PointExt;
use utility::geometry2::FPointExt;
use utility::*;

pub struct BrailleGridInWorldFrame;
pub type WorldBrailleSquare = Point2D<i32, BrailleGridInWorldFrame>;
pub type WorldBraillePoint = Point2D<f32, BrailleGridInWorldFrame>;

pub struct BrailleGridInCharacterFrame;
pub type LocalCharacterBrailleSquare = Point2D<i32, BrailleGridInCharacterFrame>;
pub type LocalCharacterBraillePoint = Point2D<f32, BrailleGridInCharacterFrame>;

pub struct BrailleGridInLocalSquareFrame;
pub type LocalBrailleSquare = Point2D<i32, BrailleGridInLocalSquareFrame>;
pub type LocalBraillePoint = Point2D<f32, BrailleGridInLocalSquareFrame>;

pub const EMPTY_BRAILLE: char = '\u{2800}';
pub const FULL_BRAILLE: char = '⣿';

// All the braille unicode consecutively for easy reference
pub const ALL_NON_EMPTY_BRAILLE_IN_ONE_STRING: &str = "⠁⠂⠃⠄⠅⠆⠇⠈⠉⠊⠋⠌⠍⠎⠏⠐⠑⠒⠓⠔⠕⠖⠗⠘⠙⠚⠛⠜⠝⠞⠟⠠⠡⠢⠣⠤⠥⠦⠧⠨⠩⠪⠫⠬⠭⠮⠯⠰⠱⠲⠳⠴⠵⠶⠷⠸⠹⠺⠻⠼⠽⠾⠿⡀⡁⡂⡃⡄⡅⡆⡇⡈⡉⡊⡋⡌⡍⡎⡏⡐⡑⡒⡓⡔⡕⡖⡗⡘⡙⡚⡛⡜⡝⡞⡟⡠⡡⡢⡣⡤⡥⡦⡧⡨⡩⡪⡫⡬⡭⡮⡯⡰⡱⡲⡳⡴⡵⡶⡷⡸⡹⡺⡻⡼⡽⡾⡿⢀⢁⢂⢃⢄⢅⢆⢇⢈⢉⢊⢋⢌⢍⢎⢏⢐⢑⢒⢓⢔⢕⢖⢗⢘⢙⢚⢛⢜⢝⢞⢟⢠⢡⢢⢣⢤⢥⢦⢧⢨⢩⢪⢫⢬⢭⢮⢯⢰⢱⢲⢳⢴⢵⢶⢷⢸⢹⢺⢻⢼⢽⢾⢿⣀⣁⣂⣃⣄⣅⣆⣇⣈⣉⣊⣋⣌⣍⣎⣏⣐⣑⣒⣓⣔⣕⣖⣗⣘⣙⣚⣛⣜⣝⣞⣟⣠⣡⣢⣣⣤⣥⣦⣧⣨⣩⣪⣫⣬⣭⣮⣯⣰⣱⣲⣳⣴⣵⣶⣷⣸⣹⣺⣻⣼⣽⣾⣿";

pub type BrailleArray = BoolArray2D<2, 4>;
pub type DoubleBrailleArray = SquareBoolArray2D<4>;

pub trait BrailleArrayExt {
    fn from_char(c: char) -> Self;
    fn char(&self) -> char;
}
impl BrailleArrayExt for BrailleArray {
    fn from_char(mut c: char) -> Self {
        if c == SPACE {
            c = EMPTY_BRAILLE;
        }
        assert!(char_is_braille(c), "NOT BRAILLE: {}", c as u32);
        let mut braille_array = BrailleArray::empty();
        let dot_val = (c as u32).bitxor(EMPTY_BRAILLE as u32);
        for x in 0..2 {
            for y in 0..4 {
                let just_the_bit = braille_bit_for_pos(point2(x as i32, y as i32));
                let there_is_a_dot_here = just_the_bit & dot_val != 0;
                if there_is_a_dot_here {
                    braille_array.set_xy(x, y, true);
                }
            }
        }
        braille_array
    }
    fn char(&self) -> char {
        let mut dot_val: u32 = 0;
        for x in 0..2 {
            for y in 0..4 {
                if self.get_xy(x, y) {
                    dot_val |= braille_bit_for_pos(point2(x as i32, y as i32));
                }
            }
        }
        let c = char::from_u32(EMPTY_BRAILLE as u32 | dot_val).unwrap();
        if c == EMPTY_BRAILLE {
            SPACE
        } else {
            c
        }
    }
}

pub trait DoubleBrailleArrayExt {
    fn from_chars(chars: DoubleChar) -> Self;
    fn chars(&self) -> DoubleChar;
    fn from_two_braille_arrays(arrays: [BrailleArray; 2]) -> Self;
    fn to_two_braille_arrays(&self) -> [BrailleArray; 2];
}
impl DoubleBrailleArrayExt for DoubleBrailleArray {
    fn from_chars(chars: DoubleChar) -> Self {
        Self::from_two_braille_arrays(chars.map(|c| BrailleArray::from_char(c)))
    }
    fn chars(&self) -> DoubleChar {
        self.to_two_braille_arrays()
            .map(|braille_array| braille_array.char())
    }
    fn from_two_braille_arrays(arrays: [BrailleArray; 2]) -> Self {
        let mut double_braille_array = Self::empty();
        for row in 0..Self::height() {
            for col in 0..Self::width() {
                let sub_col = col % BrailleArray::width();
                let index = col / BrailleArray::width();
                double_braille_array.set_row_col(row, col, arrays[index].get_row_col(row, sub_col));
            }
        }
        double_braille_array
    }
    fn to_two_braille_arrays(&self) -> [BrailleArray; 2] {
        let mut arrays = [BrailleArray::empty(), BrailleArray::empty()];
        for row in 0..Self::height() {
            for col in 0..Self::width() {
                let sub_col = col % BrailleArray::width();
                let index = col / BrailleArray::width();
                arrays[index].set_row_col(row, sub_col, self.get_row_col(row, col));
            }
        }
        arrays
    }
}

pub fn braille_bit_for_pos(p: Point2D<i32, BrailleGridInWorldFrame>) -> u32 {
    let braille_value_map = vec![vec![7, 3, 2, 1], vec![8, 6, 5, 4]];
    1 << (braille_value_map[p.x as usize][p.y as usize] - 1)
}

pub fn add_braille_dot(character: char, p: Point2D<i32, BrailleGridInWorldFrame>) -> char {
    char::from_u32(character as u32 | braille_bit_for_pos(p)).unwrap()
}

pub fn char_is_braille(c: char) -> bool {
    let x = c as u32;
    // The unicode braille block
    // TODO: This includes empty braille.  Bad?
    x >= 0x2800 && x <= 0x28FF
}

pub fn count_braille_dots(character: char) -> u32 {
    if !char_is_braille(character) {
        return 0;
    }
    let num_good_bits = 8;
    let mut sum = 0;
    let bits = character as u32;
    for i in 0..num_good_bits {
        sum += (bits >> i) & 1;
    }
    return sum as u32;
}

pub fn combine_braille_characters(c1: char, c2: char) -> char {
    assert!(char_is_braille(c1));
    assert!(char_is_braille(c2));
    char::from_u32(c1 as u32 | c2 as u32).unwrap()
}



pub fn world_point_to_world_braille_point(pos: WorldPoint) -> WorldBraillePoint {
    // Inlined deprecated char-grid hop:
    // world_character_point_to_braille_point(world_point_to_world_character_point(pos))
    // == ((2x+0.5)*2+0.5, 4y+1.5)
    point2(pos.x * 4.0 + 1.5, pos.y * 4.0 + 1.5)
}
pub fn world_braille_point_to_world_point(pos: WorldBraillePoint) -> WorldPoint {
    // Inlined deprecated char-grid hop:
    // world_character_point_to_world_point(world_braille_point_to_world_character_point(pos))
    // == (((x-0.5)/2 - 0.5)/2, (y-1.5)/4)
    point2((pos.x - 1.5) / 4.0, (pos.y - 1.5) / 4.0)
}

pub fn world_points_for_braille_line(
    start_pos: WorldPoint,
    end_pos: WorldPoint,
) -> Vec<WorldPoint> {
    let braille_start_square = world_point_to_world_braille_point(start_pos)
        .round()
        .to_i32();
    let braille_end_square = world_point_to_world_braille_point(end_pos).round().to_i32();

    line_drawing::Bresenham::new(
        braille_start_square.to_tuple(),
        braille_end_square.to_tuple(),
    )
    .map(|(x, y)| WorldBraillePoint::new(x as f32, y as f32))
    .map(world_braille_point_to_world_point)
    .collect()
}
// TODO: name by origin at bottom left
pub fn braille_char_by_pos_in_char(pos_in_char: geometry2::FPoint) -> char {
    assert!(pos_in_char.x() >= 0.0);
    assert!(pos_in_char.x() < 1.0);
    assert!(pos_in_char.y() >= 0.0);
    assert!(pos_in_char.y() < 1.0);

    let braille_point = [pos_in_char.x() * 2.0, pos_in_char.y() * 4.0];
    let braille_square = braille_point.snap_to_grid();
    add_braille_dot(EMPTY_BRAILLE, braille_square.into())
}

pub fn local_braille_squares_to_braille_char2(squares: Vec<geometry2::IPoint>) -> char {
    let mut output_array: BrailleArray = BrailleArray::empty();
    for square in squares {
        assert!(square.x() >= 0 || square.x() < 2);
        assert!(square.y() >= 0 || square.y() < 4);
        output_array.set_xy(square.x() as usize, square.y() as usize, true);
    }
    output_array.char()
}

pub fn get_braille_arrays_for_braille_line(
    start_pos: WorldPoint,
    end_pos: WorldPoint,
) -> HashMap<WorldSquare, DoubleBrailleArray> {
    let start_braille_square = world_point_to_world_braille_point(start_pos).round().to_i32();
    let end_braille_square = world_point_to_world_braille_point(end_pos).round().to_i32();

    let mut arrays = HashMap::<WorldSquare, DoubleBrailleArray>::new();
    for (x, y) in line_drawing::Bresenham::new(
        start_braille_square.to_tuple(),
        end_braille_square.to_tuple(),
    ) {
        // Bin by world square directly. The char_x/char_y formulas are the
        // old world_braille_square_to_world_character_square (euclid-round
        // semantics: (v+0.5).floor()) inlined; pairing is div/rem_euclid.
        let char_x = ((x as f32 - 0.5) / 2.0 + 0.5).floor() as i32;
        let world_square: WorldSquare = point2(
            char_x.div_euclid(2),
            ((y as f32 - 1.5) / 4.0 + 0.5).floor() as i32,
        );
        let local_dot: LocalBrailleSquare =
            point2(x - 4 * world_square.x, y - 4 * world_square.y);
        debug_assert!((0..4).contains(&local_dot.x) && (0..4).contains(&local_dot.y));
        arrays
            .entry(world_square)
            .or_insert_with(DoubleBrailleArray::empty)
            .set_xy(local_dot.x as usize, local_dot.y as usize, true);
    }
    arrays
}
/// Bins points into a 4x4 braille-dot grid per world square.
pub fn points_to_braille_double_arrays(
    points: Vec<impl Into<WorldPoint>>,
) -> HashMap<WorldSquare, DoubleBrailleArray> {
    let mut dots_by_square = HashMap::<WorldSquare, DoubleBrailleArray>::new();
    for point in points {
        let point: WorldPoint = point.into();
        let world_square: WorldSquare = point.round().to_i32();
        let braille_square = world_point_to_world_braille_point(point).round().to_i32();
        // euclid won't subtract across units; the braille grid is exactly
        // 4x finer than the world-square grid in both axes
        let local_dot: LocalBrailleSquare = point2(
            braille_square.x - world_square.x * 4,
            braille_square.y - world_square.y * 4,
        );
        debug_assert!((0..4).contains(&local_dot.x) && (0..4).contains(&local_dot.y));
        dots_by_square
            .entry(world_square)
            .or_insert_with(DoubleBrailleArray::empty)
            .set_xy(local_dot.x as usize, local_dot.y as usize, true);
    }
    dots_by_square
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::glyph::glyph_constants::SPACE;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_array_to_braille_char() {
        // 10
        // 00
        // 01
        // 01
        assert_eq!(
            BrailleArray::from_array([[true, false], [false, false], [false, true], [false, true]])
                .char(),
            '⢡'
        );

        // 00
        // 11
        // 01
        // 00
        assert_eq!(
            BrailleArray::from_array([[false, false], [true, true], [false, true], [false, false]])
                .char(),
            '⠲'
        );
    }

    #[test]
    fn test_set_braille_dot() {
        let mut b = EMPTY_BRAILLE;
        b = add_braille_dot(b, point2(0, 0));
        b = add_braille_dot(b, point2(1, 1));
        assert_eq!(b, '⡠');
    }

    #[test]
    fn test_combine_braille_character() {
        assert_eq!(
            combine_braille_characters('\u{2800}', '\u{2820}'),
            '\u{2820}'
        );
        assert_eq!(
            combine_braille_characters('\u{2801}', '\u{28C0}'),
            '\u{28C1}'
        );
    }

    #[test]
    fn test_count_braille_dots() {
        assert_eq!(count_braille_dots('\u{2800}'), 0);
        assert_eq!(count_braille_dots('\u{2818}'), 2);
        assert_eq!(count_braille_dots('\u{28C0}'), 2);
        assert_eq!(count_braille_dots('\u{28FF}'), 8);
        assert_eq!(count_braille_dots('A'), 0);
        assert_eq!(count_braille_dots('#'), 0);
    } //⠁⠂⠃⠄⠅⠆⠇⠈⠉⠊⠋⠌⠍⠎⠏⠐⠑⠒⠓⠔⠕⠖⠗⠘⠙⠚⠛⠜⠝⠞⠟⠠⠡⠢⠣⠤⠥⠦⠧⠨⠩⠪⠫⠬⠭⠮⠯⠰⠱⠲⠳⠴⠵⠶⠷⠸⠹⠺⠻⠼⠽⠾⠿⡀⡁⡂⡃⡄⡅⡆⡇⡈⡉⡊⡋⡌⡍⡎⡏⡐⡑⡒⡓⡔⡕⡖⡗⡘⡙⡚⡛⡜⡝⡞⡟⡠⡡⡢⡣⡤⡥⡦⡧⡨⡩⡪⡫⡬⡭⡮⡯⡰⡱⡲⡳⡴⡵⡶⡷⡸⡹⡺⡻⡼⡽⡾⡿⢀⢁⢂⢃⢄⢅⢆⢇⢈⢉⢊⢋⢌⢍⢎⢏⢐⢑⢒⢓⢔⢕⢖⢗⢘⢙⢚⢛⢜⢝⢞⢟⢠⢡⢢⢣⢤⢥⢦⢧⢨⢩⢪⢫⢬⢭⢮⢯⢰⢱⢲⢳⢴⢵⢶⢷⢸⢹⢺⢻⢼⢽⢾⢿⣀⣁⣂⣃⣄⣅⣆⣇⣈⣉⣊⣋⣌⣍⣎⣏⣐⣑⣒⣓⣔⣕⣖⣗⣘⣙⣚⣛⣜⣝⣞⣟⣠⣡⣢⣣⣤⣥⣦⣧⣨⣩⣪⣫⣬⣭⣮⣯⣰⣱⣲⣳⣴⣵⣶⣷⣸⣹⣺⣻⣼⣽⣾⣿

    fn double_chars_for_braille_line(
        start: WorldPoint,
        end: WorldPoint,
    ) -> HashMap<WorldSquare, DoubleChar> {
        get_braille_arrays_for_braille_line(start, end)
            .into_iter()
            .map(|(square, dots)| (square, dots.chars()))
            .collect()
    }

    #[test]
    fn test_chars_for_horizontal_braille_line_without_rounding() {
        // inputs converted from world character points to world points via
        // world = ((char_x - 0.5) / 2, char_y)
        let start: WorldPoint = point2(-0.375, -0.4);
        let end: WorldPoint = point2(0.625, -0.4);

        // Expected braille:
        // 00 00 00
        // 00 00 00
        // 00 00 00
        // 11 11 10

        let line_chars = double_chars_for_braille_line(start, end);
        assert_eq!(line_chars.len(), 2);

        assert_eq!(line_chars.get(&point2(0, 0)).unwrap(), &['\u{28C0}', '\u{28C0}']);
        assert_eq!(line_chars.get(&point2(1, 0)).unwrap(), &['\u{2840}', SPACE]);
    }

    #[test]
    fn test_chars_for_horizontal_braille_line_with_offset_without_rounding() {
        let start = WorldPoint::new(-0.375, 0.4);
        let end = WorldPoint::new(0.625, 0.4);

        // Expected braille:
        // 11 11 10
        // 00 00 00
        // 00 00 00
        // 00 00 00

        let line_glyphs = double_chars_for_braille_line(start, end);
        assert_eq!(line_glyphs.len(), 2);

        assert_eq!(line_glyphs.get(&point2(0, 0)).unwrap(), &['\u{2809}', '\u{2809}']);
        assert_eq!(line_glyphs.get(&point2(1, 0)).unwrap(), &['\u{2801}', SPACE]);
    }

    #[test]
    fn test_chars_for_vertical_braille_line_without_rounding() {
        let start = WorldPoint::new(-0.375, -0.4);
        let end = WorldPoint::new(-0.375, 0.875);

        // Expected braille:
        // 00
        // 00
        // 10
        // 10

        // 10
        // 10
        // 10
        // 10

        let line_glyphs = double_chars_for_braille_line(start, end);
        assert_eq!(line_glyphs.len(), 2);

        assert_eq!(line_glyphs.get(&point2(0, 0)).unwrap(), &['\u{2847}', SPACE]);
        assert_eq!(line_glyphs.get(&point2(0, 1)).unwrap(), &['\u{2844}', SPACE]);
    }

    #[test]
    fn test_points_to_braille_double_arrays() {
        // ┌──┬──┐┌──┬──┐
        // │  │  ││  │  │
        // │  │o ││  │oo│
        // │ o│  ││  │  │
        // │  │  ││  │  │
        // └──┴──┘└──┴──┘

        // 00 00  00 00
        // 00 10  00 11
        // 01 00  00 00
        // 00 00  00 00

        let points = vec![
            WorldPoint::new(0.1, 0.1),
            WorldPoint::new(0.1, 0.1), // duplicate for funsies
            WorldPoint::new(-0.1, -0.1),
            WorldPoint::new(1.1, 0.1),
            WorldPoint::new(1.4, 0.1),
        ];

        // world-square keyed: [left, right] characters per square
        let chars: HashMap<WorldSquare, DoubleChar> = points_to_braille_double_arrays(points)
            .into_iter()
            .map(|(square, dots)| (square, dots.chars()))
            .collect();

        assert_eq!(chars.len(), 2);
        assert_eq!(chars.get(&point2(0, 0)).unwrap(), &['⠠', '⠂']);
        assert_eq!(chars.get(&point2(1, 0)).unwrap(), &[SPACE, '⠒']);
    }
    #[test]
    fn test_the_big_braille_string() {
        ALL_NON_EMPTY_BRAILLE_IN_ONE_STRING
            .chars()
            .for_each(|c| assert!(char_is_braille(c)));
    }
    #[test]
    fn test_braille_array_to_and_from_char() {
        ALL_NON_EMPTY_BRAILLE_IN_ONE_STRING
            .chars()
            .for_each(|c| assert_eq!(BrailleArray::from_char(c).char(), c));
    }
    #[test]
    fn test_double_braille_array_rotation() {
        let mut array = DoubleBrailleArray::empty();
        array.set_xy(2, 0, true);
        array.set_xy(2, 1, true);
        // ....
        // ....
        // ..o.
        // ..o.
        let turns_xys = vec![
            (1, vec![(2, 2), (3, 2)]),
            (2, vec![(1, 3), (1, 2)]),
            (3, vec![(0, 1), (1, 1)]),
            (-1, vec![(0, 1), (1, 1)]),
        ];
        for t in turns_xys {
            for p in t.1 {
                assert_eq!(
                    array
                        .rotated(QuarterTurnsAnticlockwise::new(t.0))
                        .get_xy(p.0, p.1),
                    true
                );
            }
        }
    }
    #[test]
    fn test_braille_array_parse_space() {
        assert_eq!(BrailleArray::from_char(SPACE).char(), SPACE);
    }

}

