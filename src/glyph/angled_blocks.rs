use std::collections::{HashMap, HashSet};
use std::f32::consts::PI;

use euclid::{point2, vec2, Angle, Point2D};
use ordered_float::OrderedFloat;

use crate::glyph::glyph_constants::{
    FULL_BLOCK, LEFT_HALF_BLOCK, LOWER_LEFT_HALF_BLOCK_TRIANGLE, LOWER_ONE_THIRD_BLOCK,
    LOWER_RIGHT_HALF_BLOCK_TRIANGLE, LOWER_TWO_THIRD_BLOCK, RIGHT_HALF_BLOCK, SPACE,
    UPPER_LEFT_HALF_BLOCK_TRIANGLE, UPPER_ONE_THIRD_BLOCK, UPPER_RIGHT_HALF_BLOCK_TRIANGLE,
    UPPER_TWO_THIRD_BLOCK,
};
use crate::utility::angle_interval::AngleInterval;
use crate::utility::coordinate_frame_conversions::*;
use crate::utility::{
    is_clockwise, point_to_string, snap_angle_to_diagonal, unit_vector_from_angle, HalfPlane, Line,
};

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct AngleBlockSnapGridInLocalFrame;

pub type SnapGridPoint = Point2D<i32, AngleBlockSnapGridInLocalFrame>;

fn local_snap_grid_to_local_character_frame(grid_point: SnapGridPoint) -> LocalCharacterPoint {
    point2(
        grid_point.x as f32 * 0.5 - 0.5,
        grid_point.y as f32 * 1.0 / 3.0 - 0.5,
    )
}

fn snap_to_grid(local_character_point: LocalCharacterPoint) -> SnapGridPoint {
    point2(
        (local_character_point.x / 0.5 + 1.0).round() as i32,
        (local_character_point.y * 3.0 + 1.5).round() as i32,
    )
}

fn valid_snap_points_on_angle_block() -> HashSet<SnapGridPoint> {
    // snap grid origin is bottom left of character, because the center doesn't really line up with the snap grid
    let mut valid_points = Vec::<SnapGridPoint>::new();
    for x in 0..3 {
        for y in 0..4 {
            if x == 1 && [1, 2].contains(&y) {
                continue;
            }
            valid_points.push(point2(x, y));
        }
    }
    valid_points.into_iter().collect()
}

pub fn angled_block_flip_y(c: char) -> char {
    if c == FULL_BLOCK {
        return FULL_BLOCK;
    }
    if c == SPACE {
        return SPACE;
    }
    let points = angled_block_char_to_snap_points_map()
        .get(&c)
        .unwrap()
        .clone();
    let y_flipped_points = (
        point2(points.1.x, 3 - points.1.y),
        point2(points.0.x, 3 - points.0.y),
    );
    *points_to_angled_block_mapping()
        .get(&y_flipped_points)
        .unwrap()
}

pub fn angled_block_char_to_snap_points_map() -> HashMap<char, (SnapGridPoint, SnapGridPoint)> {
    points_to_angled_block_mapping()
        .into_iter()
        .map(|(a, b)| (b, a))
        .collect()
}

pub fn angle_block_char_complement(c: char) -> char {
    if c == FULL_BLOCK {
        return SPACE;
    }
    if c == SPACE {
        return FULL_BLOCK;
    }
    let points_for_block = angled_block_char_to_snap_points_map()
        .get(&c)
        .unwrap()
        .clone();
    let points_for_complement = (points_for_block.1, points_for_block.0);
    *points_to_angled_block_mapping()
        .get(&points_for_complement)
        .unwrap()
}

//                                                           🬼 	🬽 	🬾 	🬿
//U+1FB4x 	🭀 	🭁 	🭂 	🭃 	🭄 	🭅 	🭆 	🭇 	🭈 	🭉 	🭊 	🭋 	🭌 	🭍 	🭎 	🭏
//U+1FB5x 	🭐 	🭑 	🭒 	🭓 	🭔 	🭕 	🭖 	🭗 	🭘 	🭙 	🭚 	🭛 	🭜 	🭝 	🭞 	🭟
//U+1FB6x 	🭠 	🭡 	🭢 	🭣 	🭤 	🭥 	🭦 	🭧
fn points_to_angled_block_mapping() -> HashMap<(SnapGridPoint, SnapGridPoint), char> {
    // The inside of the angled block is CLOCKWISE from the vector point1_to_point2
    // The coordinates start at the lower-left corner of the character

    //      0  1  2
    //      |  |  |
    // 3 -- o──o──o -- 3
    //      │     │
    // 2 -- o     o -- 2
    //      │     │
    // 1 -- o     o -- 1
    //      │     │
    // 0 -- o──o──o -- 0
    //      |  |  |
    //      0  1  2
    let mut block_map = HashMap::<(SnapGridPoint, SnapGridPoint), char>::new();

    // TODO: find an actual pattern for these
    block_map.insert((point2(0, 1), point2(1, 0)), '🬼');
    block_map.insert((point2(0, 1), point2(2, 0)), '🬽');
    block_map.insert((point2(0, 2), point2(1, 0)), '🬾');
    block_map.insert((point2(0, 2), point2(2, 0)), '🬿');
    block_map.insert((point2(0, 3), point2(1, 0)), '🭀');
    block_map.insert((point2(0, 2), point2(1, 3)), '🭁');
    block_map.insert((point2(0, 2), point2(2, 3)), '🭂');
    block_map.insert((point2(0, 1), point2(1, 3)), '🭃');
    block_map.insert((point2(0, 1), point2(2, 3)), '🭄');
    block_map.insert((point2(0, 0), point2(1, 3)), '🭅');
    block_map.insert((point2(0, 1), point2(2, 2)), '🭆');
    block_map.insert((point2(1, 0), point2(2, 1)), '🭇');
    block_map.insert((point2(0, 0), point2(2, 1)), '🭈');
    block_map.insert((point2(1, 0), point2(2, 2)), '🭉');
    block_map.insert((point2(0, 0), point2(2, 2)), '🭊');
    block_map.insert((point2(1, 0), point2(2, 3)), '🭋');
    block_map.insert((point2(1, 3), point2(2, 2)), '🭌');
    block_map.insert((point2(0, 3), point2(2, 2)), '🭍');
    block_map.insert((point2(1, 3), point2(2, 1)), '🭎');
    block_map.insert((point2(0, 3), point2(2, 1)), '🭏');
    block_map.insert((point2(1, 3), point2(2, 0)), '🭐');
    block_map.insert((point2(0, 2), point2(2, 1)), '🭑');
    block_map.insert((point2(1, 0), point2(0, 1)), '🭒');
    block_map.insert((point2(2, 0), point2(0, 1)), '🭓');
    block_map.insert((point2(1, 0), point2(0, 2)), '🭔');
    block_map.insert((point2(2, 0), point2(0, 2)), '🭕');
    block_map.insert((point2(1, 0), point2(0, 3)), '🭖');
    block_map.insert((point2(1, 3), point2(0, 2)), '🭗');
    block_map.insert((point2(2, 3), point2(0, 2)), '🭘');
    block_map.insert((point2(1, 3), point2(0, 1)), '🭙');
    block_map.insert((point2(2, 3), point2(0, 1)), '🭚');
    block_map.insert((point2(1, 3), point2(0, 0)), '🭛');
    block_map.insert((point2(2, 2), point2(0, 1)), '🭜');
    block_map.insert((point2(2, 1), point2(1, 0)), '🭝');
    block_map.insert((point2(2, 1), point2(0, 0)), '🭞');
    block_map.insert((point2(2, 2), point2(1, 0)), '🭟');
    block_map.insert((point2(2, 2), point2(0, 0)), '🭠');
    block_map.insert((point2(2, 3), point2(1, 0)), '🭡');
    block_map.insert((point2(2, 2), point2(1, 3)), '🭢');
    block_map.insert((point2(2, 2), point2(0, 3)), '🭣');
    block_map.insert((point2(2, 1), point2(1, 3)), '🭤');
    block_map.insert((point2(2, 1), point2(0, 3)), '🭥');
    block_map.insert((point2(2, 0), point2(1, 3)), '🭦');
    block_map.insert((point2(2, 1), point2(0, 2)), '🭧');

    block_map.insert((point2(1, 0), point2(1, 3)), RIGHT_HALF_BLOCK);
    block_map.insert((point2(1, 3), point2(1, 0)), LEFT_HALF_BLOCK);
    block_map.insert((point2(0, 1), point2(2, 1)), LOWER_ONE_THIRD_BLOCK);
    block_map.insert((point2(0, 2), point2(2, 2)), LOWER_TWO_THIRD_BLOCK);
    block_map.insert((point2(2, 1), point2(0, 1)), UPPER_TWO_THIRD_BLOCK);
    block_map.insert((point2(2, 2), point2(0, 2)), UPPER_ONE_THIRD_BLOCK);

    block_map.insert(
        (point2(0, 0), point2(2, 3)),
        LOWER_RIGHT_HALF_BLOCK_TRIANGLE,
    );
    block_map.insert((point2(2, 3), point2(0, 0)), UPPER_LEFT_HALF_BLOCK_TRIANGLE);
    block_map.insert(
        (point2(2, 0), point2(0, 3)),
        UPPER_RIGHT_HALF_BLOCK_TRIANGLE,
    );
    block_map.insert((point2(0, 3), point2(2, 0)), LOWER_LEFT_HALF_BLOCK_TRIANGLE);
    block_map
}

fn get_character_from_snap_points(line: Line<i32, AngleBlockSnapGridInLocalFrame>) -> char {
    let pointA = line.p1;
    let pointB = line.p2;

    let block_map = points_to_angled_block_mapping();

    if let Some(&character) = block_map.get(&(pointA, pointB)) {
        character
    } else {
        let same_x = pointA.x == pointB.x;
        let same_y = pointA.y == pointB.y;
        let both_on_same_edge =
            (same_x && [0, 2].contains(&pointA.x)) || (same_y && [0, 3].contains(&pointA.y));
        let center_is_clockwise = is_clockwise(
            local_snap_grid_to_local_character_frame(pointA),
            local_snap_grid_to_local_character_frame(pointB),
            point2(0.0, 0.0),
        );
        if both_on_same_edge {
            if center_is_clockwise {
                FULL_BLOCK
            } else {
                SPACE
            }
        } else {
            if pointA == pointB {
                panic!(
                    "Can't be same points: A: {}, B: {}",
                    point_to_string(pointA),
                    point_to_string(pointB)
                );
            }

            let valid_points = valid_snap_points_on_angle_block();
            if !valid_points.contains(&pointA) || !valid_points.contains(&pointB) {
                panic!(
                    "gave non-valid points: A: {}, B: {}",
                    point_to_string(pointA),
                    point_to_string(pointB)
                );
            }

            panic!(
                "Missed case for points: A: {}, B: {}",
                point_to_string(pointA),
                point_to_string(pointB)
            );
        }
    }
}

pub fn half_plane_to_angled_block_character(
    half_plane: HalfPlane<f32, CharacterGridInLocalCharacterFrame>,
    bias_direction: Angle<f32>,
) -> char {
    // angle blocks have important edge points

    //    0  0.5
    //    |  |
    // o──o──o -- 0.5
    // │     │
    // o     o -- 1/6
    // │     │ -- 0
    // o     o
    // │     │
    // o──o──o

    let valid_snap_points: Vec<LocalCharacterPoint> = valid_snap_points_on_angle_block()
        .into_iter()
        .map(local_snap_grid_to_local_character_frame)
        .collect();

    let raw_intersection_points = half_plane
        .dividing_line()
        .line_intersections_with_centered_unit_square();
    assert!(raw_intersection_points.len() <= 2);

    // slightly offsetting these intersection points, so rationally sloped sight lines don't hit the points exactly halfway between the angle block snap points
    let snapped_bias_direction = snap_angle_to_diagonal(bias_direction);
    let biased_intersection_points: Vec<_> = raw_intersection_points
        .into_iter()
        .map(|point| {
            point + unit_vector_from_angle(snapped_bias_direction).cast_unit() * (PI / 1000.0)
        })
        .collect();

    let snapped_points: Vec<LocalCharacterPoint> = biased_intersection_points
        .iter()
        .map(|&intersection_point| {
            *valid_snap_points
                .iter()
                .min_by_key(|&&snap_point| OrderedFloat((intersection_point - snap_point).length()))
                .unwrap()
        })
        .collect();

    if snapped_points.len() < 2 || snapped_points[0] == snapped_points[1] {
        if half_plane
            .dividing_line()
            .same_side_of_line(half_plane.point_on_half_plane(), point2(0.0, 0.0))
        {
            FULL_BLOCK
        } else {
            SPACE
        }
    } else {
        let mut grid_line = Line::new(
            snap_to_grid(snapped_points[0]),
            snap_to_grid(snapped_points[1]),
        );
        if !is_clockwise(
            half_plane.dividing_line().p1,
            half_plane.dividing_line().p2,
            half_plane.point_on_half_plane(),
        ) {
            grid_line.reverse();
        }
        get_character_from_snap_points(grid_line)
    }
}

pub fn angle_block_chars_are_horizontally_continuous(left_char: char, right_char: char) -> bool {
    let angle_block_to_snap_points_map = angled_block_char_to_snap_points_map();
    let chars = [left_char, right_char];
    let snap_points_optional = chars.map(|ch| angle_block_to_snap_points_map.get(&ch));

    let left_char_right_snap_point = if let Some(left_snap_line) = snap_points_optional[0] {
        Some(if left_snap_line.0.x == 2 {
            left_snap_line.0
        } else {
            left_snap_line.1
        })
    } else {
        None
    };
    let right_char_left_snap_point = if let Some(right_snap_line) = snap_points_optional[1] {
        Some(if right_snap_line.0.x == 0 {
            right_snap_line.0
        } else {
            right_snap_line.1
        })
    } else {
        None
    };
    if left_char_right_snap_point.is_some() && right_char_left_snap_point.is_some() {
        left_char_right_snap_point.unwrap().y == right_char_left_snap_point.unwrap().y
    } else if left_char_right_snap_point.is_some() && right_char_left_snap_point.is_none() {
        let left_y = left_char_right_snap_point.unwrap().y;
        left_y == 0 || left_y == 3
    } else if left_char_right_snap_point.is_none() && right_char_left_snap_point.is_some() {
        let right_y = right_char_left_snap_point.unwrap().y;
        right_y == 0 || right_y == 3
    } else {
        panic!("actually_fully_visible");
    }
}

#[cfg(test)]
mod tests {
    use crate::glyph::glyph_constants::{LOWER_ONE_THIRD_BLOCK, RIGHT_HALF_BLOCK};
    use crate::utility::Line;
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    #[test]
    fn test_line_and_inside_point_to_angled_block_character__left_edge_full_block() {
        let line_point_A: LocalCharacterPoint = point2(-0.5, -0.5);
        assert_eq!(
            half_plane_to_angled_block_character(
                HalfPlane::from_line_and_point_on_half_plane(
                    Line {
                        p1: point2(-0.5, -0.5),
                        p2: point2(-0.5, 0.5),
                    },
                    point2(0.0, 0.0),
                ),
                Angle::degrees(45.0)
            ),
            FULL_BLOCK,
            "on left edge, full block"
        );
    }
    #[test]
    fn test_line_and_inside_point_to_angled_block_character__left_edge_empty_block() {
        assert_eq!(
            half_plane_to_angled_block_character(
                HalfPlane::from_line_and_point_on_half_plane(
                    Line {
                        p1: point2(-0.5, -0.5),
                        p2: point2(-0.5, 0.5)
                    },
                    point2(-20.0, 0.0),
                ),
                Angle::degrees(45.0)
            ),
            SPACE,
            "on left edge, empty block"
        );
    }
    #[test]
    fn test_line_and_inside_point_to_angled_block_character__lower_right_diagonal() {
        assert_eq!(
            half_plane_to_angled_block_character(
                HalfPlane::from_line_and_point_on_half_plane(
                    Line {
                        p1: point2(-0.5, -0.5),
                        p2: point2(-0.4, -0.4)
                    },
                    point2(2.0, 0.0),
                ),
                Angle::degrees(45.0)
            ),
            '◢',
            "lower-right diagonal given short line"
        );
    }
    #[test]
    fn test_line_and_inside_point_to_angled_block_character__notch_off_bottom_right() {
        assert_eq!(
            half_plane_to_angled_block_character(
                HalfPlane::from_line_and_point_on_half_plane(
                    Line {
                        p1: point2(0.0, -0.5),
                        p2: point2(0.5, -0.15),
                    },
                    point2(0.0, 0.0),
                ),
                Angle::degrees(45.0)
            ),
            '🭝',
            "Notch off bottom-right"
        );
    }

    #[test]
    fn test_snap_to_grid() {
        assert_eq!(
            snap_to_grid(point2(-0.45, -0.51)),
            point2(0, 0),
            "origin in correct place"
        );
        assert_eq!(
            snap_to_grid(point2(-0.1, 0.1)),
            point2(1, 2),
            "should snap to non-angle-block points"
        );
        assert_eq!(
            snap_to_grid(point2(0.5, 0.5)),
            point2(2, 3),
            "top right corner of character"
        );
    }

    #[test]
    fn test_snap_points_to_character() {
        assert_eq!(
            get_character_from_snap_points(Line::new(point2(0, 0), point2(2, 3))),
            '◢',
            "lower-right diagonal"
        );
        assert_eq!(
            get_character_from_snap_points(Line::new(point2(2, 3), point2(0, 0))),
            '◤',
            "swap points"
        );
        assert_eq!(
            get_character_from_snap_points(Line::new(point2(2, 1), point2(0, 3))),
            '🭥',
            "upper right corner"
        );
        assert_eq!(
            get_character_from_snap_points(Line::new(point2(0, 1), point2(2, 1))),
            LOWER_ONE_THIRD_BLOCK
        );
        assert_eq!(
            get_character_from_snap_points(Line::new(point2(1, 0), point2(1, 3))),
            RIGHT_HALF_BLOCK
        );
        assert_eq!(
            get_character_from_snap_points(Line::new(point2(0, 0), point2(2, 1))),
            '🭈'
        );
    }

    #[test]
    fn test_half_plane_to_character__from_failure_data() {
        let half_plane = HalfPlane::from_line_and_point_on_half_plane(
            Line {
                p1: point2(-1.5, -1.0),
                p2: point2(-0.08, -0.3),
            },
            point2(-0.06, -0.3),
        );
        let the_char = half_plane_to_angled_block_character(half_plane, Angle::degrees(45.0));
        assert!(['🭈', '🭊'].contains(&the_char));
    }

    //                                                           🬼 	🬽 	🬾 	🬿
    //U+1FB4x 	🭀 	🭁 	🭂 	🭃 	🭄 	🭅 	🭆 	🭇 	🭈 	🭉 	🭊 	🭋 	🭌 	🭍 	🭎 	🭏
    //U+1FB5x 	🭐 	🭑 	🭒 	🭓 	🭔 	🭕 	🭖 	🭗 	🭘 	🭙 	🭚 	🭛 	🭜 	🭝 	🭞 	🭟
    //U+1FB6x 	🭠 	🭡 	🭢 	🭣 	🭤 	🭥 	🭦 	🭧

    #[test]
    fn test_angle_block_char_complement() {
        assert_eq!(angle_block_char_complement('🭦'), '🭐');
        assert_eq!(angle_block_char_complement('🭗'), '🭁');
        assert_eq!(angle_block_char_complement(FULL_BLOCK), SPACE);
        assert_eq!(angle_block_char_complement(SPACE), FULL_BLOCK);
    }

    #[test]
    fn test_top_half_visible_with_exactly_horizontal_line() {
        assert_eq!(
            half_plane_to_angled_block_character(
                HalfPlane::from_line_and_point_on_half_plane(
                    Line {
                        p1: point2(0.5, 0.0),
                        p2: point2(-1.5, 0.0),
                    },
                    point2(0.0, 25.0),
                ),
                Angle::degrees(-90.0)
            ),
            '🬎',
        );
    }
    #[test]
    fn test_left_half_visible_with_exactly_vertical_line() {
        assert_eq!(
            half_plane_to_angled_block_character(
                HalfPlane::from_line_and_point_on_half_plane(
                    Line {
                        p1: point2(0.0, 0.5),
                        p2: point2(0.0, -1.5),
                    },
                    point2(-20.0, 0.0),
                ),
                Angle::degrees(0.0)
            ),
            LEFT_HALF_BLOCK,
        );
    }
}
