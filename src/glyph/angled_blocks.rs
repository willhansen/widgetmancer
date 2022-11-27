use crate::glyph::glyph_constants::{FULL_BLOCK, SPACE};
use crate::utility::{
    line_intersections_with_centered_unit_square, same_side_of_line, LocalCharacterPoint,
};
use euclid::{point2, Point2D};
use ordered_float::OrderedFloat;

pub fn line_and_inside_point_to_angled_block_character(
    line_point_A: LocalCharacterPoint,
    line_point_B: LocalCharacterPoint,
    inside_point: LocalCharacterPoint,
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

    assert_ne!(line_point_A, line_point_B);
    assert_ne!(inside_point, line_point_B);
    assert_ne!(line_point_A, inside_point);

    // snap grid origin is bottom left of character, because the center doesn't really line up with the snap grid

    struct AngleBlockSnapGridInLocalFrame;
    type SnapGridPoint = Point2D<i32, AngleBlockSnapGridInLocalFrame>;
    fn local_snap_grid_to_local_character_frame(grid_point: &SnapGridPoint) -> LocalCharacterPoint {
        point2(
            grid_point.x as f32 * 0.5 - 0.5,
            grid_point.y as f32 * 1.0 / 3.0 - 0.5,
        )
    }

    fn local_character_frame_to_local_snap_grid(
        local_character_point: &LocalCharacterPoint,
    ) -> SnapGridPoint {
        point2(
            (local_character_point.x / 0.5) as i32 + 1,
            (local_character_point.y + 0.5) as i32 * 3,
        )
    }

    let mut valid_snap_grid_points = Vec::<SnapGridPoint>::new();
    for x in 0..3 {
        for y in 0..4 {
            if x == 1 && [1, 2].contains(&y) {
                continue;
            }
            valid_snap_grid_points.push(point2(x, y));
        }
    }

    let snap_points: Vec<LocalCharacterPoint> = valid_snap_grid_points
        .iter()
        .map(local_snap_grid_to_local_character_frame)
        .collect();

    let raw_intersection_points =
        line_intersections_with_centered_unit_square(line_point_A, line_point_B);
    assert!(raw_intersection_points.len() <= 2);

    let snapped_points: Vec<LocalCharacterPoint> = raw_intersection_points
        .iter()
        .map(|&intersection_point| {
            *snap_points
                .iter()
                .min_by_key(|&&snap_point| OrderedFloat((intersection_point - snap_point).length()))
                .unwrap()
        })
        .collect();

    if snapped_points.len() < 2 {
        if same_side_of_line(line_point_A, line_point_B, inside_point, point2(0.0, 0.0)) {
            FULL_BLOCK
        } else {
            SPACE
        }
    } else {
        todo!()
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn test_line_and_inside_point_to_angled_block_character() {
        let line_point_A: LocalCharacterPoint = point2(-0.5, -0.5);
        assert_eq!(
            line_and_inside_point_to_angled_block_character(
                point2(-0.5, -0.5),
                point2(-0.5, 0.5),
                point2(0.0, 0.0),
            ),
            FULL_BLOCK,
            "on left edge, full block"
        );
        assert_eq!(
            line_and_inside_point_to_angled_block_character(
                point2(-0.5, -0.5),
                point2(-0.5, 0.5),
                point2(-20.0, 0.0),
            ),
            SPACE,
            "on left edge, empty block"
        );
        assert_eq!(
            line_and_inside_point_to_angled_block_character(
                point2(-0.5, -0.5),
                point2(-0.4, -0.4),
                point2(2.0, 0.0),
            ),
            '◢',
            "lower-right diagonal given short line"
        );
        assert_eq!(
            line_and_inside_point_to_angled_block_character(
                point2(0.0, -0.5),
                point2(0.5, -0.15),
                point2(0.0, 0.0),
            ),
            '🭝',
            "Notch off bottom-right"
        );
    }
}
