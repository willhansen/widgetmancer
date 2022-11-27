use crate::utility::*;
use euclid::{point2, Point2D};

pub struct BrailleGridInWorldFrame;
pub type WorldBrailleSquare = Point2D<i32, BrailleGridInWorldFrame>;
pub type WorldBraillePoint = Point2D<f32, BrailleGridInWorldFrame>;

pub type BrailleArray = [[bool; 4]; 2];

pub const EMPTY_BRAILLE: char = '\u{2800}';

// All the braille unicode consecutively for easy reference
//⠁⠂⠃⠄⠅⠆⠇⠈⠉⠊⠋⠌⠍⠎⠏⠐⠑⠒⠓⠔⠕⠖⠗⠘⠙⠚⠛⠜⠝⠞⠟⠠⠡⠢⠣⠤⠥⠦⠧⠨⠩⠪⠫⠬⠭⠮⠯⠰⠱⠲⠳⠴⠵⠶⠷⠸⠹⠺⠻⠼⠽⠾⠿⡀⡁⡂⡃⡄⡅⡆⡇⡈⡉⡊⡋⡌⡍⡎⡏⡐⡑⡒⡓⡔⡕⡖⡗⡘⡙⡚⡛⡜⡝⡞⡟⡠⡡⡢⡣⡤⡥⡦⡧⡨⡩⡪⡫⡬⡭⡮⡯⡰⡱⡲⡳⡴⡵⡶⡷⡸⡹⡺⡻⡼⡽⡾⡿⢀⢁⢂⢃⢄⢅⢆⢇⢈⢉⢊⢋⢌⢍⢎⢏⢐⢑⢒⢓⢔⢕⢖⢗⢘⢙⢚⢛⢜⢝⢞⢟⢠⢡⢢⢣⢤⢥⢦⢧⢨⢩⢪⢫⢬⢭⢮⢯⢰⢱⢲⢳⢴⢵⢶⢷⢸⢹⢺⢻⢼⢽⢾⢿⣀⣁⣂⣃⣄⣅⣆⣇⣈⣉⣊⣋⣌⣍⣎⣏⣐⣑⣒⣓⣔⣕⣖⣗⣘⣙⣚⣛⣜⣝⣞⣟⣠⣡⣢⣣⣤⣥⣦⣧⣨⣩⣪⣫⣬⣭⣮⣯⣰⣱⣲⣳⣴⣵⣶⣷⣸⣹⣺⣻⣼⣽⣾⣿

pub fn braille_array_to_braille_char(input: BrailleArray) -> char {
    let mut dot_val: u32 = 0;
    for x in 0..2 {
        for y in 0..4 {
            if input[x][y] {
                dot_val |= braille_bit_for_pos(point2(x as i32, y as i32));
            }
        }
    }
    return char::from_u32('\u{2800}' as u32 | dot_val).unwrap();
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

pub fn world_character_point_to_braille_point(
    pos: Point2D<f32, CharacterGridInWorldFrame>,
) -> Point2D<f32, BrailleGridInWorldFrame> {
    point2(pos.x * 2.0 + 0.5, pos.y * 4.0 + 1.5)
}

pub fn braille_pos_to_character_world_pos(
    pos: Point2D<f32, BrailleGridInWorldFrame>,
) -> Point2D<f32, CharacterGridInWorldFrame> {
    point2((pos.x - 0.5) / 2.0, (pos.y - 1.5) / 4.0)
}

pub fn world_point_to_world_braille_point(pos: WorldPoint) -> WorldBraillePoint {
    world_character_point_to_braille_point(world_point_to_world_character_point(pos))
}
pub fn world_braille_point_to_world_point(pos: WorldBraillePoint) -> WorldPoint {
    world_character_point_to_world_point(world_braille_point_to_world_character_point(pos))
}

pub fn braille_square_to_dot_in_character(
    pos: Point2D<i32, BrailleGridInWorldFrame>,
) -> Point2D<i32, BrailleGridInWorldFrame> {
    point2((pos.x % 2).abs(), (pos.y % 4).abs())
}

pub fn world_braille_point_to_world_character_point(
    braille_point: WorldBraillePoint,
) -> WorldCharacterPoint {
    point2(
        (braille_point.x as f32 - 0.5) / 2.0,
        (braille_point.y as f32 - 1.5) / 4.0,
    )
}

pub fn world_braille_square_to_world_character_square(
    braille_square: WorldBrailleSquare,
) -> WorldCharacterSquare {
    world_braille_point_to_world_character_point(braille_square.to_f32())
        .round()
        .to_i32()
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

pub fn character_world_pos_to_braille_char(
    world_pos: Point2D<f32, CharacterGridInWorldFrame>,
) -> char {
    let character = EMPTY_BRAILLE;
    add_braille_dot(
        character,
        braille_square_to_dot_in_character(
            world_character_point_to_braille_point(world_pos)
                .round()
                .to_i32(),
        ),
    )
}

pub fn local_braille_squares_to_braille_array(squares: Vec<WorldBrailleSquare>) -> BrailleArray {
    let mut output_array: BrailleArray = [[false; 4]; 2];
    for square in squares {
        assert!(square.x >= 0 || square.x < 2);
        assert!(square.y >= 0 || square.y < 4);
        output_array[square.x as usize][square.y as usize] = true;
    }
    output_array
}

pub fn local_braille_squares_to_braille_char(squares: Vec<WorldBrailleSquare>) -> char {
    braille_array_to_braille_char(local_braille_squares_to_braille_array(squares))
}
#[cfg(test)]
mod tests {
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;
    #[test]
    fn test_array_to_braille_char() {
        // 10
        // 00
        // 01
        // 01
        let input = [[false, false, false, true], [true, true, false, false]];
        assert_eq!(braille_array_to_braille_char(input), '⢡');

        // 00
        // 11
        // 01
        // 00
        let input = [[false, false, true, false], [false, true, true, false]];
        assert_eq!(braille_array_to_braille_char(input), '⠲');
    }

    #[test]
    fn test_set_braille_dot() {
        let mut b = EMPTY_BRAILLE;
        b = add_braille_dot(b, point2(0, 0));
        b = add_braille_dot(b, point2(1, 1));
        assert_eq!(b, '⡠');
    }

    #[test]
    fn test_braille_grid_to_character_grid() {
        assert_eq!(
            world_braille_square_to_world_character_square(point2(0, 0)),
            point2(0, 0)
        );
        assert_eq!(
            world_braille_square_to_world_character_square(point2(1, 3)),
            point2(0, 0)
        );
        assert_eq!(
            world_braille_square_to_world_character_square(point2(-1, -1)),
            point2(-1, -1)
        );
        assert_eq!(
            world_braille_square_to_world_character_square(point2(2, 8)),
            point2(1, 2)
        );
        assert_eq!(
            world_braille_square_to_world_character_square(point2(21, 80)),
            point2(10, 20)
        );
    }

    #[test]
    fn test_world_pos_to_braille_pos() {
        assert_eq!(
            world_character_point_to_braille_point(point2(0.0, 0.0)),
            point2(0.5, 1.5)
        );
        assert_eq!(
            world_character_point_to_braille_point(point2(1.0, 0.0)),
            point2(2.5, 1.5)
        );
        assert_eq!(
            world_character_point_to_braille_point(point2(0.25, 0.375)),
            point2(1.0, 3.0)
        );
    }

    #[test]
    fn test_braille_pos_to_world_pos() {
        assert_eq!(
            braille_pos_to_character_world_pos(point2(0.5, 1.5)),
            point2(0.0, 0.0)
        );
        assert_eq!(
            braille_pos_to_character_world_pos(point2(2.5, 1.5)),
            point2(1.0, 0.0)
        );
        assert_eq!(
            braille_pos_to_character_world_pos(point2(1.0, 3.0)),
            point2(0.25, 0.375)
        );
    }

    #[test]
    fn test_braille_square_to_dot_in_character() {
        assert_eq!(
            braille_square_to_dot_in_character(point2(0, 0)),
            point2(0, 0)
        );
        assert_eq!(
            braille_square_to_dot_in_character(point2(1, 3)),
            point2(1, 3)
        );
        assert_eq!(
            braille_square_to_dot_in_character(point2(25, 4)),
            point2(1, 0)
        );
        assert_eq!(
            braille_square_to_dot_in_character(point2(-3, 4)),
            point2(1, 0)
        );
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
    fn test_world_point_to_braille_char() {
        assert_eq!(
            character_world_pos_to_braille_char(point2(0.0, 0.0)),
            '\u{2810}'
        );
        assert_eq!(
            character_world_pos_to_braille_char(point2(-0.4, -0.4)),
            '\u{2840}'
        );
        assert_eq!(
            character_world_pos_to_braille_char(point2(0.2, 0.4)),
            '\u{2808}'
        );
    }

    #[test]
    fn test_world_point_to_braille_char_is_always_braille() {
        for _ in 0..200 {
            //let random_point = p(rand_in_range(0.0, 30.0), rand_in_range(0.0, 30.0));
            let random_point = point2(23.2273, 2.05);

            assert!(char_is_braille(character_world_pos_to_braille_char(
                random_point
            )));
        }
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
}
