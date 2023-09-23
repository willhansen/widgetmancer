use crate::glyph::{glyph_constants::FACE_ARROWS, Glyph};

use super::{
    coordinate_frame_conversions::*,
    coordinates::*,
    general_utility::*,
    line::{square_face_as_line, WorldLine},
    RigidTransform, RigidlyTransformable,
};

#[derive(
    Clone, Hash, Eq, PartialEq, Neg, Debug, Copy, getset::CopyGetters, derive_more::Constructor,
)]
#[get_copy = "pub"]
pub struct StepWithQuarterRotations {
    stepp: WorldStep,
    rotation: QuarterTurnsAnticlockwise,
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
        StepWithQuarterRotations::new(STEP_ZERO, QuarterTurnsAnticlockwise::new(0))
    }
}

impl Add for StepWithQuarterRotations {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        StepWithQuarterRotations::new(self.stepp + rhs.stepp, self.rotation + rhs.rotation)
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Copy, getset::CopyGetters)]
#[get_copy = "pub"]
pub struct AbsOrRelSquareWithOrthogonalDir<SquareType: Copy> {
    square: SquareType,
    dir: OrthogonalWorldStep,
}

pub type SquareWithOrthogonalDir = AbsOrRelSquareWithOrthogonalDir<WorldSquare>;
pub type Face = SquareWithOrthogonalDir;
pub type RelativeSquareWithOrthogonalDir = AbsOrRelSquareWithOrthogonalDir<WorldStep>;
pub type RelativeFace = RelativeSquareWithOrthogonalDir;

impl<SquareType> AbsOrRelSquareWithOrthogonalDir<SquareType>
where
    SquareType: AbsOrRelSquareTrait<SquareType> + Copy + PartialEq,
{
    pub fn direction_in_quarter_turns(&self) -> QuarterTurnsAnticlockwise {
        QuarterTurnsAnticlockwise::from_start_and_end_directions(STEP_RIGHT, self.dir.into())
    }
    pub fn from_square_and_step<S: Into<OrthogonalWorldStep>>(
        square: SquareType,
        direction: S,
    ) -> Self {
        Self {
            square,
            dir: direction.into(),
        }
    }
    pub fn from_square_and_turns(
        square: SquareType,
        quarter_turns: QuarterTurnsAnticlockwise,
    ) -> Self {
        Self::from_square_and_step(square, quarter_turns.to_vector())
    }
    pub fn direction(&self) -> OrthogonalWorldStep {
        self.dir()
    }
    pub fn stepped(&self) -> Self {
        Self::from_square_and_step(self.square + self.direction().step(), self.direction())
    }
    pub fn stepped_n(&self, n: i32) -> Self {
        Self::from_square_and_step(self.square + self.direction().step() * n, self.direction())
    }
    pub fn stepped_back(&self) -> Self {
        Self::from_square_and_step(self.square - self.direction().step(), self.direction())
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

    pub fn turned_left(&self) -> Self {
        Self::from_square_and_step(self.square, self.left())
    }
    pub fn turned_right(&self) -> Self {
        Self::from_square_and_step(self.square, self.right())
    }
    fn left(&self) -> OrthogonalWorldStep {
        rotated_n_quarter_turns_counter_clockwise(self.direction().into(), 1).into()
    }
    fn right(&self) -> OrthogonalWorldStep {
        rotated_n_quarter_turns_counter_clockwise(self.direction().into(), 3).into()
    }
    pub fn turned_back(&self) -> Self {
        Self::from_square_and_step(self.square, -self.direction().step())
    }
    pub fn with_offset(&self, offset: WorldStep) -> Self {
        Self::from_square_and_step(self.square + offset, self.dir())
    }
    pub fn with_direction(&self, dir: WorldStep) -> Self {
        Self::from_square_and_step(self.square, dir)
    }
    pub fn reversed(&self) -> Self {
        self.with_direction(-self.direction().step())
    }
    fn as_relative_face(&self) -> RelativeSquareWithOrthogonalDir {
        RelativeSquareWithOrthogonalDir::from_square_and_step(
            self.square() - SquareType::zero(),
            self.dir(),
        )
    }
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
    // TODO: return AbsOrRelWorldLine
    pub fn line(&self) -> WorldLine {
        let abs_face = self.as_absolute_face();
        square_face_as_line(abs_face.square, abs_face.dir)
    }
    pub fn cw_end_of_face(&self) -> PointType {
        todo!()
    }
    pub fn ccw_end_of_face(&self) -> PointType {
        todo!()
    }
}

impl<T: Debug + Copy> Debug for AbsOrRelSquareWithOrthogonalDir<T>
where
    Self: Display,
{
    fn fmt(&self, mut f: &mut Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&(&self), &mut f)
    }
}
impl<T: Debug + Copy> Display for AbsOrRelSquareWithOrthogonalDir<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Pos: {:?}, Dir: {:?} {} ",
            self.square(),
            self.dir().step(),
            Glyph::extract_arrow_from_arrow_string(self.dir().step(), FACE_ARROWS)
        )
    }
}

// TODO: generalize for absolute squares too
impl RelativeSquareWithOrthogonalDir {
    pub fn face_center_point(&self) -> WorldMove {
        self.square().to_f32() + self.dir().step().to_f32() * 0.5
    }
    pub fn face_end_points(&self) -> [WorldMove; 2] {
        [self.left(), self.right()].map(|dir| self.face_center_point() + dir.step().to_f32() * 0.5)
    }
    pub fn face_end_points_in_ccw_order(&self) -> [WorldMove; 2] {
        let mut ps = self.face_end_points();
        if !two_in_ccw_order(ps[0], ps[1]) {
            ps.reverse();
        }
        ps
    }
    pub fn face_end_point_approx_touches_point(&self, point: WorldMove) -> bool {
        let tolerance = 1e-6;
        self.face_end_points()
            .into_iter()
            .any(|end_point| about_eq_2d(end_point, point, tolerance))
    }
}

impl TryFrom<SquareWithKingDir> for SquareWithOrthogonalDir {
    type Error = ();

    fn try_from(value: SquareWithKingDir) -> Result<Self, Self::Error> {
        if is_orthogonal(value.direction().into()) {
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
    From<(ConvertableToSquareType, DirectionType)> for AbsOrRelSquareWithOrthogonalDir<SquareType>
where
    ConvertableToSquareType: Into<SquareType>,
    SquareType: AbsOrRelSquareTrait<SquareType>,
    DirectionType: Into<OrthogonalWorldStep>,
{
    fn from(value: (ConvertableToSquareType, DirectionType)) -> Self {
        Self::from_square_and_step(value.0.into(), value.1)
    }
}
impl<SquareType> From<AbsOrRelSquareWithOrthogonalDir<SquareType>>
    for (SquareType, OrthogonalWorldStep)
where
    SquareType: AbsOrRelSquareTrait<SquareType>,
{
    fn from(
        value: AbsOrRelSquareWithOrthogonalDir<SquareType>,
    ) -> (SquareType, OrthogonalWorldStep) {
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
impl<SquareType> RigidlyTransformable for AbsOrRelSquareWithOrthogonalDir<SquareType>
where
    SquareType: Copy + RigidlyTransformable + AbsOrRelSquareTrait<SquareType>,
{
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        Self::from_square_and_step(
            self.square().apply_rigid_transform(tf),
            self.dir().apply_rigid_transform(tf),
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

pub fn squares_sharing_face<SquareType: AbsOrRelSquareTrait<SquareType>>(
    face: AbsOrRelSquareWithOrthogonalDir<SquareType>,
) -> [SquareType; 2] {
    [face.square, face.stepped().square]
}

pub fn faces_in_ccw_order<T: IntoIterator<Item = T2> + Clone, T2: Into<RelativeFace>>(
    v: T,
) -> bool {
    in_ccw_order(v.into_iter().map(|e| e.into().face_center_point()))
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
}
