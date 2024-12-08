// automod::dir!(pub "src");

mod square_with_orthogonal_direction;
mod square_with_king_direction;


use misc_utilities::*;
use coordinates::*;


#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy, CopyGetters)]
#[get_copy = "pub"]
pub struct TranslationAndRotationTransform {
    translation: WorldStep,
    quarter_rotations_counterclockwise: u32,
}
impl<SquareType> RigidlyTransformable for SquareWithOrthogonalDirection<SquareType>
where
    SquareType: Copy + RigidlyTransformable + WorldIntCoordinateOps,
{
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        Self::from_square_and_step(
            self.square().apply_rigid_transform(tf),
            self.dir().quarter_rotated_ccw(tf.rotation()),
        )
    }
}
pub fn faces_away_from_center_at_rel_square(
    step: impl Into<WorldStep>,
) -> HashSet<RelativeSquareWithOrthogonalDir> {
    let step = step.into();
    ORTHOGONAL_STEPS
        .iter()
        .filter(|&&face_step| step.dot(face_step) >= 0)
        .map(|&face_step| (step, face_step).into())
        .collect()
}

pub fn squares_sharing_face<SquareType: WorldIntCoordinateOps>(
    face: SquareWithOrthogonalDirection<SquareType>,
) -> [SquareType; 2] {
    [face.square(), face.stepped().square()]
}

pub fn check_faces_in_ccw_order<T: IntoIterator<Item = impl Into<RelativeFace>> + Clone>(
    v: T,
) -> OkOrMessage {
    check_vectors_in_ccw_order(v.into_iter().map(|e| e.into().center_point_of_face()))
}

#[cfg(test)]
mod tests {

    use ntest::{assert_about_eq, assert_false, assert_true, timeout};
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    #[test]
    fn test_step_back_pose() {
        let pose = WorldSquareWithOrthogonalDir::from_square_and_step(point2(4, 6), STEP_RIGHT);
        let back = WorldSquareWithOrthogonalDir::from_square_and_step(point2(3, 6), STEP_RIGHT);
        assert_eq!(pose.stepped_back(), back);
    }

    #[test]
    fn test_step_or_turn_pose() {
        let p = WorldSquareWithOrthogonalDir::from_square_and_step;
        let s = point2(5, 5);
        assert_eq!(p(s, STEP_RIGHT).stepped(), p(s + STEP_RIGHT, STEP_RIGHT));
        assert_eq!(p(s, STEP_UP).stepped(), p(s + STEP_UP, STEP_UP));
        assert_eq!(p(s, STEP_DOWN).strafed_left(), p(s + STEP_RIGHT, STEP_DOWN));
        assert_eq!(p(s, STEP_LEFT).strafed_right(), p(s + STEP_UP, STEP_LEFT));
        assert_eq!(p(s, STEP_LEFT).turned_left(), p(s, STEP_DOWN));
        assert_eq!(p(s, STEP_LEFT).turned_right(), p(s, STEP_UP));
        assert_eq!(p(s, STEP_LEFT).turned_back(), p(s, STEP_RIGHT));
    }
    #[test]
    fn test_face_is_on_same_line() {
        let f = |a, b| WorldSquareWithOrthogonalDir::from(a).face_is_on_same_line(b);
        // facing each other left-right
        assert!(f((point2(3, 5), STEP_RIGHT), (point2(4, 5), STEP_LEFT)));
        // facing each other left-right, with vertical offset
        assert!(f((point2(3, 5), STEP_RIGHT), (point2(4, 25), STEP_LEFT)));
        //facing each other left-right, too far apart
        assert_false!(f((point2(2, 5), STEP_RIGHT), (point2(4, 5), STEP_LEFT)));

        // facing each other up-down
        assert!(f((point2(3, 5), STEP_UP), (point2(3, 6), STEP_DOWN)));

        // Same face
        assert!(f((point2(3, 5), STEP_RIGHT), (point2(3, 5), STEP_RIGHT)));
        // Same face, vertical offset
        assert!(f((point2(3, 5), STEP_RIGHT), (point2(3, 45), STEP_RIGHT)));
    }
    #[test]
    fn test_faces_away_from_center_at_relative_square() {
        let step = vec2(3, 4);
        assert_eq!(
            faces_away_from_center_at_rel_square(step),
            HashSet::from([(step, STEP_UP).into(), (step, STEP_RIGHT).into()])
        );
        let step = vec2(0, -40);
        assert_eq!(
            faces_away_from_center_at_rel_square(step),
            HashSet::from([
                (step, STEP_LEFT).into(),
                (step, STEP_DOWN).into(),
                (step, STEP_RIGHT).into()
            ])
        );
    }
    #[test]
    fn test_make_face_face_center() {
        let x_y_dir_shouldflip = [
            (1, 1, STEP_RIGHT, false),
            (1, 1, STEP_LEFT, true),
            (1, 1, STEP_UP, false),
            (1, 1, STEP_DOWN, true),
            (0, 1, STEP_RIGHT, false),
            (0, 1, STEP_LEFT, false),
            (0, 1, STEP_UP, false),
            (0, 1, STEP_DOWN, true),
            (-1, 1, STEP_RIGHT, true),
            (-1, 1, STEP_LEFT, false),
            (-1, 1, STEP_UP, false),
            (-1, 1, STEP_DOWN, true),
            (1, 0, STEP_RIGHT, false),
            (1, 0, STEP_LEFT, true),
            (1, 0, STEP_UP, false),
            (1, 0, STEP_DOWN, false),
            (0, 0, STEP_RIGHT, false),
            (0, 0, STEP_LEFT, false),
            (0, 0, STEP_UP, false),
            (0, 0, STEP_DOWN, false),
            (-1, 0, STEP_RIGHT, true),
            (-1, 0, STEP_LEFT, false),
            (-1, 0, STEP_UP, false),
            (-1, 0, STEP_DOWN, false),
            (1, -1, STEP_RIGHT, false),
            (1, -1, STEP_LEFT, true),
            (1, -1, STEP_UP, true),
            (1, -1, STEP_DOWN, false),
            (0, -1, STEP_RIGHT, false),
            (0, -1, STEP_LEFT, false),
            (0, -1, STEP_UP, true),
            (0, -1, STEP_DOWN, false),
            (-1, -1, STEP_RIGHT, true),
            (-1, -1, STEP_LEFT, false),
            (-1, -1, STEP_UP, true),
            (-1, -1, STEP_DOWN, false),
        ];

        x_y_dir_shouldflip
            .into_iter()
            .for_each(|(x, y, dir, should_flip)| {
                let edge: RelativeFace = ((5 * x, 5 * y), dir).into();
                let correct = if should_flip {
                    edge.stepped().turned_back()
                } else {
                    edge
                };
                let canonicalized = edge.face_flipped_to_face_origin();
                assert_eq!(
                    canonicalized, correct,
                    "edge: {:?}\ncanonicalized: {:?}\ncorrect: {:?}",
                    edge, canonicalized, correct
                );
            });
    }
    #[test]
    fn test_rotate_vs_revolve_a_face() {
        let rel_face: RelativeFace = (3, 5, UP).into();

        assert_eq!(
            rel_face.quarter_rotated_ccw_in_place(1),
            (3, 5, LEFT).into()
        );
        assert_eq!(
            rel_face.quarter_revolved_ccw_around_origin(1),
            (-5, 3, LEFT).into()
        );
        assert_eq!(
            rel_face.quarter_rotated_ccw_in_place(2),
            (3, 5, DOWN).into()
        );
        assert_eq!(
            rel_face.quarter_revolved_ccw_around_origin(2),
            (-3, -5, DOWN).into()
        );

        let abs_face: FaceOfWorldSquare = (5, 1, LEFT).into();

        assert_eq!(abs_face.quarter_rotated_ccw_in_place(-1), (5, 1, UP).into());
        assert_eq!(
            abs_face.quarter_revolved_ccw_around_origin(-1),
            (1, -5, UP).into()
        );
        assert_eq!(
            abs_face.quarter_rotated_ccw_in_place(2),
            (5, 1, RIGHT).into()
        );
        assert_eq!(
            abs_face.quarter_revolved_ccw_around_origin(2),
            (-5, -1, RIGHT).into()
        );
    }
    #[test]
    fn test_square_relative_to_pose__absolute_to_relative() {
        // |
        // |.....x
        // |..>  .
        // |  .  .
        // +-------
        let abs_square = ICoord::new(6, 3);
        let observer_pose = WorldSquareWithOrthogonalDir::from_square_and_step((3, 2), STEP_RIGHT);
        let correct_rel_square = WorldStep::new(-1, 3);
        assert_eq!(
            observer_pose.other_square_absolute_to_relative(abs_square),
            correct_rel_square
        );
    }

    #[test]
    fn test_square_relative_to_pose__reversibility() {
        // pretty arbitrary
        let pose_square = vec![
            ((0, 0, UP), (0, 0)),    // origin
            ((0, 0, UP), (5, 0)),    // pure x
            ((0, 0, LEFT), (5, 0)),  // rotation
            ((3, 4, UP), (5, 7)),    // translation
            ((3, 4, RIGHT), (5, 7)), // rotation and translation
        ];
        pose_square.into_iter().for_each(|(p, s)| {
            let pose: WorldSquareWithOrthogonalDir = p.into();
            let square: ICoord = s.into();
            assert_eq!(
                pose.other_square_relative_to_absolute(
                    pose.other_square_absolute_to_relative(square)
                ),
                square
            );
        })
    }

    #[test]
    fn test_rigid_transform_of_relative_versus_absolute_pose() {
        let base_tuple = (4, 3, RIGHT);
        let abs_pose: WorldSquareWithOrthogonalDir = base_tuple.into();
        let rel_pose: RelativeSquareWithOrthogonalDir = base_tuple.into();
        let tf = RigidTransform::from_start_and_end_poses((5, 4, UP), (7, 4, RIGHT));

        assert_eq!(abs_pose.apply_rigid_transform(tf), (6, 5, DOWN).into());
        assert_eq!(
            rel_pose.apply_rigid_transform(tf),
            abs_pose.apply_rigid_transform(tf)
        );
    }
}
