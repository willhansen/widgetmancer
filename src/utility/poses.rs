use crate::glyph::{glyph_constants::FACE_ARROWS, Glyph};

use super::{
    coordinate_frame_conversions::*,
    coordinates::*,
    general_utility::*,
    line::{square_face_as_line, TwoDifferentWorldPoints},
    RigidTransform, RigidlyTransformable,
};

#[derive(
    Clone, Hash, Eq, PartialEq, Neg, Debug, Copy, getset::CopyGetters, derive_more::Constructor,
)]
#[get_copy = "pub"]
pub struct StepWithQuarterRotations {
    stepp: WorldStep,
    rotation: QuarterTurnsCcw,
}

impl StepWithQuarterRotations {
    pub fn from_direction_squares(
        start: SquareWithOrthogonalDir,
        end: SquareWithOrthogonalDir,
    ) -> Self {
        let translation = end.square() - start.square();
        let rotation = end.direction_in_quarter_turns() - start.direction_in_quarter_turns();
        Self::new(translation, rotation)
    }
}

impl Default for StepWithQuarterRotations {
    fn default() -> Self {
        StepWithQuarterRotations::new(STEP_ZERO, QuarterTurnsCcw::new(0))
    }
}

impl Add for StepWithQuarterRotations {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        StepWithQuarterRotations::new(self.stepp + rhs.stepp, self.rotation + rhs.rotation)
    }
}

#[derive(Clone, Hash, Eq, PartialEq, getset::CopyGetters)]
#[get_copy = "pub"]
pub struct OrthogonalFacingIntPose<SquareType>
where
    SquareType: WorldIntCoordinate,
{
    square: SquareType,
    dir: OrthogonalUnitCoordinate<SquareType>,
}
impl<S: WorldIntCoordinate> Copy for OrthogonalFacingIntPose<S> {}

pub type SquareWithOrthogonalDir = OrthogonalFacingIntPose<WorldSquare>;
pub type Face = SquareWithOrthogonalDir;
pub type RelativeSquareWithOrthogonalDir = OrthogonalFacingIntPose<WorldStep>;
pub type RelativeFace = RelativeSquareWithOrthogonalDir;

impl<SquareType> OrthogonalFacingIntPose<SquareType>
where
    SquareType: WorldIntCoordinate,
{
    pub fn direction_in_quarter_turns(&self) -> QuarterTurnsCcw {
        QuarterTurnsCcw::from_start_and_end_directions(STEP_RIGHT.cast_metadata(), self.dir.step())
    }
    pub fn from_square_and_step(
        square: impl Into<SquareType>,
        direction: impl Into<OrthogonalWorldStep>,
    ) -> Self {
        Self {
            square: square.into(),
            dir: Into::into(direction),
        }
    }
    pub fn from_square_and_turns(square: SquareType, quarter_turns: QuarterTurnsCcw) -> Self {
        Self::from_square_and_step(square, quarter_turns.to_orthogonal_direction())
    }
    pub fn direction(&self) -> OrthogonalUnitCoordinate<SquareType> {
        self.dir()
    }
    pub fn stepped(&self) -> Self {
        Self::from_square_and_step(
            self.square + self.direction().step(),
            self.direction().step(),
        )
    }
    pub fn stepped_n(&self, n: i32) -> Self {
        Self::from_square_and_step(
            self.square + self.direction().step() * n,
            self.direction().step(),
        )
    }
    pub fn stepped_back(&self) -> Self {
        Self::from_square_and_step(
            self.square - self.direction().step(),
            self.direction().step(),
        )
    }
    pub fn strafed_left(&self) -> Self {
        self.strafed_right_n(-1)
    }
    pub fn strafed_right(&self) -> Self {
        self.strafed_right_n(1)
    }
    pub fn strafed_right_n(&self, n: i32) -> Self {
        Self::from_square_and_step(self.square + self.right().step() * n, self.direction())
    }
    pub fn strafed_left_n(&self, n: i32) -> Self {
        self.strafed_right_n(-n)
    }

    pub fn quarter_revolved_ccw_around_origin(
        &self,
        quarter_turns_ccw: impl Into<QuarterTurnsCcw>,
    ) -> Self {
        let quarter_turns_ccw = quarter_turns_ccw.into();
        (
            SquareType::from_any_relativity(self.square.quarter_rotated_ccw(quarter_turns_ccw)),
            self.dir.quarter_rotated_ccw(quarter_turns_ccw),
        )
            .into()
    }
    pub fn quarter_rotated_ccw_in_place(
        &self,
        quarter_turns_ccw: impl Into<QuarterTurnsCcw> + Copy,
    ) -> Self {
        (self.square, self.dir.quarter_rotated_ccw(quarter_turns_ccw)).into()
    }
    pub fn quadrant_revolutions_in_ccw_order(&self) -> [Self; 4] {
        (0..4)
            .map(|i| self.quarter_revolved_ccw_around_origin(i))
            .collect_vec()
            .try_into()
            .unwrap()
    }

    pub fn turned_left(&self) -> Self {
        Self::from_square_and_step(self.square, self.left())
    }
    pub fn turned_right(&self) -> Self {
        Self::from_square_and_step(self.square, self.right())
    }
    fn left(&self) -> OrthogonalWorldStep {
        self.direction().quarter_rotated_ccw(1)
    }
    fn right(&self) -> OrthogonalWorldStep {
        self.direction().quarter_rotated_ccw(3)
    }
    pub fn turned_back(&self) -> Self {
        Self::from_square_and_step(self.square, -self.direction().step())
    }
    pub fn with_offset(&self, offset: WorldStep) -> Self {
        Self::from_square_and_step(self.square + offset, self.dir())
    }
    pub fn at_square(&self, position: impl Into<SquareType>) -> Self {
        Self::from_square_and_step(position, self.dir())
    }
    pub fn with_direction(&self, dir: WorldStep) -> Self {
        Self::from_square_and_step(self.square, dir)
    }
    pub fn reversed(&self) -> Self {
        self.with_direction(-self.direction().step())
    }
    // TODO: remove this?
    fn as_relative_face(&self) -> RelativeSquareWithOrthogonalDir {
        RelativeSquareWithOrthogonalDir::from_square_and_step(
            self.square() - SquareType::zero(),
            self.dir(),
        )
    }
    // TODO: remove this?
    fn as_absolute_face(&self) -> SquareWithOrthogonalDir {
        SquareWithOrthogonalDir::from_square_and_step(
            WorldSquare::zero() + self.as_relative_face().square(),
            self.dir(),
        )
    }
    pub fn face_is_on_same_line<OtherType: Into<Self>>(&self, other: OtherType) -> bool {
        let other_face: Self = other.into();
        let directions_are_parallel = self.dir.step().dot(other_face.dir.step()) != 0;
        if !directions_are_parallel {
            return false;
        }

        let pos_on_dir_axis = self.dir().pos_on_axis(self.as_relative_face().square());
        let stepped_pos_on_dir_axis = self
            .dir()
            .pos_on_axis(self.stepped().as_relative_face().square().into());
        let other_pos_on_dir_axis = self
            .dir()
            .pos_on_axis(other_face.as_relative_face().square().into());

        let same_direction = self.dir() == other_face.dir();
        if same_direction {
            other_pos_on_dir_axis == pos_on_dir_axis
        } else {
            other_pos_on_dir_axis == stepped_pos_on_dir_axis
        }
    }
    pub fn faces_overlap<OtherType: Into<Self> + std::marker::Copy>(
        &self,
        other_face: OtherType,
    ) -> bool {
        *self == other_face.into() || *self == other_face.into().stepped().turned_back()
    }
    // TODO: make return type relative to Self::UnitType?
    // TODO: return AbsOrRelWorldLine
    pub fn face_line_segment(&self) -> TwoDifferentWorldPoints {
        let abs_face = self.as_absolute_face();
        square_face_as_line(abs_face.square, abs_face.dir)
    }

    pub fn face_crosses_positive_x_axis(&self) -> bool {
        if self.square == SquareType::zero() {
            return self.direction() == STEP_RIGHT.into();
        }

        self.square.x() > SquareType::DataType::zero()
            && self.square.y() == 0
            && self.direction().step().is_horizontal()
    }
}

impl<T: WorldIntCoordinate> Debug for OrthogonalFacingIntPose<T>
where
    Self: Display,
{
    fn fmt(&self, mut f: &mut Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&(&self), &mut f)
    }
}
impl<T: WorldIntCoordinate> Display for OrthogonalFacingIntPose<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // TODO: tidy
        write!(
            f,
            "Pos: (x:{}, y:{}), Dir: (x: {}, y: {}) {} ",
            self.square().x(),
            self.square().y(),
            self.dir().step().x(),
            self.dir().step().y(),
            Glyph::extract_arrow_from_arrow_string(self.dir().step(), FACE_ARROWS)
        )
    }
}

// ambiguous.  use the revolve or rotate in place functions instead
// impl<T: WorldGridCoordinate> QuarterTurnRotatable for AbsOrRelSquareWithOrthogonalDir<T> {
//     fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<QuarterTurnsCcw> + Copy) -> Self {
//         (
//             self.square.quarter_rotated_ccw(quarter_turns_ccw),
//             self.dir
//                 .step()
//                 .quarter_rotated_ccw(quarter_turns_ccw.into().quarter_turns()),
//         )
//             .into()
//     }
// }

static_assertions::assert_not_impl_any!(SquareWithOrthogonalDir: QuarterTurnRotatable);
static_assertions::assert_not_impl_any!(RelativeSquareWithOrthogonalDir: QuarterTurnRotatable);
static_assertions::assert_not_impl_any!(SquareWithKingDir: QuarterTurnRotatable);

impl SquareWithOrthogonalDir {
    pub fn middle_point_of_face(&self) -> WorldPoint {
        self.square.to_f32() + self.direction().step().to_f32() * 0.5
    }

    // TODO: replace with just subtraction, returning whatever a relative pose is (probably a translation and rotation)
    pub fn other_pose_absolute_to_relative(&self, other: impl Into<Self>) -> Self {
        let other = other.into();

        let naive_translation: WorldStep = other.square - self.square;
        let rotation = QuarterTurnsCcw::from_start_and_end_directions(self.dir, STEP_UP);
        Self::from_square_and_step(
            naive_translation.quarter_rotated_ccw(rotation),
            other.dir.quarter_rotated_ccw(rotation),
        )
    }
    pub fn other_pose_relative_to_absolute(&self, other: impl Into<Self>) -> Self {
        let other: Self = other.into();

        let relative_translation: WorldStep = other.square;
        let rotation = QuarterTurnsCcw::from_start_and_end_directions(self.dir, STEP_UP);
        Self::from_square_and_step(
            self.square + relative_translation.quarter_rotated_ccw(-rotation),
            other.dir.quarter_rotated_ccw(-rotation),
        )
    }
    pub fn other_square_absolute_to_relative(&self, other: impl Into<WorldSquare>) -> WorldStep {
        self.other_pose_absolute_to_relative((other.into(), STEP_UP))
            .square()
    }
    pub fn other_square_relative_to_absolute(&self, other: impl Into<WorldStep>) -> WorldSquare {
        self.other_pose_relative_to_absolute((other.into(), STEP_UP))
            .square()
    }
}

// TODO: generalize for absolute squares too
impl RelativeSquareWithOrthogonalDir {
    pub fn center_point_of_face(&self) -> WorldMove {
        self.square().to_f32() + self.dir().step().to_f32() * 0.5
    }
    pub fn end_points_of_face(&self) -> [WorldMove; 2] {
        [self.left(), self.right()]
            .map(|dir| self.center_point_of_face() + dir.step().to_f32() * 0.5)
    }
    pub fn end_points_of_face_in_ccw_order(&self) -> [WorldMove; 2] {
        let mut ps = self.end_points_of_face();
        if !two_in_ccw_order(ps[0], ps[1]) {
            ps.reverse();
        }
        ps
    }
    pub fn cw_end_of_face(&self) -> WorldMove {
        self.end_points_of_face_in_ccw_order()[0]
    }
    pub fn ccw_end_of_face(&self) -> WorldMove {
        self.end_points_of_face_in_ccw_order()[1]
    }
    pub fn face_end_point_approx_touches_point(&self, point: WorldMove) -> bool {
        let tolerance = 1e-6;
        self.end_points_of_face()
            .into_iter()
            .any(|end_point| about_eq_2d(end_point, point, tolerance))
    }
    pub fn flipped_to_face_origin(&self) -> Self {
        if self.square().dot(self.direction().into()) < 0 {
            self.stepped().turned_back()
        } else {
            *self
        }
    }
}

impl TryFrom<SquareWithKingDir> for SquareWithOrthogonalDir {
    type Error = ();

    fn try_from(value: SquareWithKingDir) -> Result<Self, Self::Error> {
        if value.direction().into().is_orthogonal() {
            Ok(SquareWithOrthogonalDir::from_square_and_step(
                value.square(),
                value.direction(),
            ))
        } else {
            Err(())
        }
    }
}
impl Add<StepWithQuarterRotations> for SquareWithOrthogonalDir {
    type Output = Self;

    fn add(self, rhs: StepWithQuarterRotations) -> Self::Output {
        SquareWithOrthogonalDir::from_square_and_turns(
            self.square + rhs.stepp,
            self.direction_in_quarter_turns() + rhs.rotation,
        )
    }
}

impl Sub<SquareWithOrthogonalDir> for SquareWithOrthogonalDir {
    type Output = StepWithQuarterRotations;

    fn sub(self, rhs: SquareWithOrthogonalDir) -> Self::Output {
        StepWithQuarterRotations::new(
            self.square - rhs.square,
            self.direction_in_quarter_turns() - rhs.direction_in_quarter_turns(),
        )
    }
}

impl<ConvertableToSquareType, SquareType, DirectionType>
    From<(ConvertableToSquareType, DirectionType)> for OrthogonalFacingIntPose<SquareType>
where
    ConvertableToSquareType: Into<SquareType>,
    SquareType: WorldIntCoordinate,
    DirectionType: Into<OrthogonalWorldStep>,
{
    fn from(value: (ConvertableToSquareType, DirectionType)) -> Self {
        Self::from_square_and_step(value.0.into(), value.1)
    }
}

impl<T, SquareType, DirectionType> From<(T, T, DirectionType)>
    for OrthogonalFacingIntPose<SquareType>
where
    (T, T): Into<SquareType>,
    SquareType: WorldIntCoordinate,
    DirectionType: Into<OrthogonalWorldStep>,
{
    fn from(value: (T, T, DirectionType)) -> Self {
        Self::from_square_and_step((value.0, value.1).into(), value.2)
    }
}

impl<SquareType> From<OrthogonalFacingIntPose<SquareType>> for (SquareType, OrthogonalWorldStep)
where
    SquareType: WorldIntCoordinate,
{
    fn from(value: OrthogonalFacingIntPose<SquareType>) -> (SquareType, OrthogonalWorldStep) {
        (value.square, value.direction())
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy, CopyGetters)]
#[get_copy = "pub"]
pub struct SquareWithKingDir {
    square: WorldSquare,
    direction: KingWorldStep,
}

impl SquareWithKingDir {
    pub fn new(square: WorldSquare, direction: KingWorldStep) -> Self {
        SquareWithKingDir { square, direction }
    }
    pub fn from_square_and_step(square: WorldSquare, direction: WorldStep) -> SquareWithKingDir {
        Self::new(square, direction.into())
    }
    pub fn tuple(&self) -> (WorldSquare, KingWorldStep) {
        (self.square, self.direction)
    }
    pub fn stepped(&self) -> SquareWithKingDir {
        SquareWithKingDir::from_square_and_step(
            self.square + self.direction.step(),
            self.direction.into(),
        )
    }
}

impl From<SquareWithOrthogonalDir> for SquareWithKingDir {
    fn from(value: SquareWithOrthogonalDir) -> Self {
        SquareWithKingDir {
            square: value.square(),
            direction: value.direction().into(),
        }
    }
}

impl From<(WorldSquare, KingWorldStep)> for SquareWithKingDir {
    fn from(value: (WorldSquare, KingWorldStep)) -> Self {
        Self::new(value.0, value.1)
    }
}
impl From<SquareWithKingDir> for (WorldSquare, KingWorldStep) {
    fn from(value: SquareWithKingDir) -> (WorldSquare, KingWorldStep) {
        (value.square, value.direction)
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy, CopyGetters)]
#[get_copy = "pub"]
pub struct TranslationAndRotationTransform {
    translation: WorldStep,
    quarter_rotations_counterclockwise: u32,
}
impl<SquareType> RigidlyTransformable for OrthogonalFacingIntPose<SquareType>
where
    SquareType: Copy + RigidlyTransformable + WorldIntCoordinate,
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

pub fn squares_sharing_face<SquareType: WorldIntCoordinate>(
    face: OrthogonalFacingIntPose<SquareType>,
) -> [SquareType; 2] {
    [face.square, face.stepped().square]
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
        let pose = SquareWithOrthogonalDir::from_square_and_step(point2(4, 6), STEP_RIGHT);
        let back = SquareWithOrthogonalDir::from_square_and_step(point2(3, 6), STEP_RIGHT);
        assert_eq!(pose.stepped_back(), back);
    }

    #[test]
    fn test_step_or_turn_pose() {
        let p = SquareWithOrthogonalDir::from_square_and_step;
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
        let f = |a, b| SquareWithOrthogonalDir::from(a).face_is_on_same_line(b);
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
                let canonicalized = edge.flipped_to_face_origin();
                assert_eq!(
                    canonicalized, correct,
                    "edge: {:?}\ncanonicalized: {:?}\ncorrect: {:?}",
                    edge, canonicalized, correct
                );
            });
    }
    #[test]
    fn test_rotate_vs_revolve_a_face() {
        let rel_face: RelativeFace = (3, 5, STEP_UP).into();

        assert_eq!(
            rel_face.quarter_rotated_ccw_in_place(1),
            (3, 5, STEP_LEFT).into()
        );
        assert_eq!(
            rel_face.quarter_revolved_ccw_around_origin(1),
            (-5, 3, STEP_LEFT).into()
        );
        assert_eq!(
            rel_face.quarter_rotated_ccw_in_place(2),
            (3, 5, STEP_DOWN).into()
        );
        assert_eq!(
            rel_face.quarter_revolved_ccw_around_origin(2),
            (-3, -5, STEP_DOWN).into()
        );

        let abs_face: Face = (5, 1, STEP_LEFT).into();

        assert_eq!(
            abs_face.quarter_rotated_ccw_in_place(-1),
            (5, 1, STEP_UP).into()
        );
        assert_eq!(
            abs_face.quarter_revolved_ccw_around_origin(-1),
            (1, -5, STEP_UP).into()
        );
        assert_eq!(
            abs_face.quarter_rotated_ccw_in_place(2),
            (5, 1, STEP_RIGHT).into()
        );
        assert_eq!(
            abs_face.quarter_revolved_ccw_around_origin(2),
            (-5, -1, STEP_RIGHT).into()
        );
    }
    #[test]
    fn test_square_relative_to_pose__absolute_to_relative() {
        // |
        // |.....x
        // |..>  .
        // |  .  .
        // +-------
        let abs_square = WorldSquare::new(6, 3);
        let observer_pose = SquareWithOrthogonalDir::from_square_and_step((3, 2), STEP_RIGHT);
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
            ((0, 0, STEP_UP), (0, 0)),    // origin
            ((0, 0, STEP_UP), (5, 0)),    // pure x
            ((0, 0, STEP_LEFT), (5, 0)),  // rotation
            ((3, 4, STEP_UP), (5, 7)),    // translation
            ((3, 4, STEP_RIGHT), (5, 7)), // rotation and translation
        ];
        pose_square.into_iter().for_each(|(p, s)| {
            let pose: SquareWithOrthogonalDir = p.into();
            let square: WorldSquare = s.into();
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
        let base_tuple = (4, 3, STEP_RIGHT);
        let abs_pose: SquareWithOrthogonalDir = base_tuple.into();
        let rel_pose: RelativeSquareWithOrthogonalDir = base_tuple.into();
        let tf = RigidTransform::from_start_and_end_poses((5, 4, STEP_UP), (7, 4, STEP_RIGHT));

        assert_eq!(abs_pose.apply_rigid_transform(tf), (6, 5, STEP_DOWN).into());
        assert_eq!(
            rel_pose.apply_rigid_transform(tf),
            abs_pose.apply_rigid_transform(tf).as_relative_face()
        );
    }
}
