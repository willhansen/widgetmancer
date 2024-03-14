use std::{
    fmt::{Debug, Display, Formatter},
    ops::{Add, Sub},
};

use crate::utility::*;

#[derive(Clone, Copy, Hash, Eq, PartialEq, getset::CopyGetters)]
#[get_copy = "pub"]
pub struct OrthogonalFacingIntPose<SquareType>
where
    SquareType: IntCoordinate,
{
    square: SquareType,
    dir: OrthogonalUnitCoordinate<SquareType>,
}

impl<SquareType> OrthogonalFacingIntPose<SquareType>
where
    SquareType: IntCoordinate,
{
    pub fn direction_in_quarter_turns(&self) -> QuarterTurnsCcw {
        QuarterTurnsCcw::turns_from_x_axis(self.dir.step())
    }
    pub fn from_square_and_step(
        square: impl Into<SquareType>,
        direction: impl Into<OrthogonalUnitCoordinate<SquareType>>,
    ) -> Self {
        Self {
            square: square.into(),
            dir: direction.into(),
        }
    }
    pub fn from_square_and_turns(square: SquareType, quarter_turns: QuarterTurnsCcw) -> Self {
        Self::from_square_and_step(square, quarter_turns)
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
            self.square.quarter_rotated_ccw(quarter_turns_ccw),
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
    fn left(&self) -> OrthogonalUnitCoordinate<SquareType> {
        self.direction().quarter_rotated_ccw(1)
    }
    fn right(&self) -> OrthogonalUnitCoordinate<SquareType> {
        self.direction().quarter_rotated_ccw(3)
    }
    pub fn turned_back(&self) -> Self {
        Self::from_square_and_step(self.square, -self.direction().step())
    }
    pub fn with_offset(&self, offset: SquareType) -> Self {
        Self::from_square_and_step(self.square + offset, self.dir())
    }
    pub fn at_square(&self, position: impl Into<SquareType>) -> Self {
        Self::from_square_and_step(position, self.dir())
    }
    pub fn with_direction(&self, dir: impl Into<OrthogonalUnitCoordinate<SquareType>>) -> Self {
        Self::from_square_and_step(self.square, dir)
    }
    pub fn reversed(&self) -> Self {
        self.with_direction(-self.direction())
    }
    pub fn face_is_on_same_line<OtherType: Into<Self>>(&self, other: OtherType) -> bool {
        let other_face: Self = other.into();
        let directions_are_parallel = self.dir.step().dot(other_face.dir.step()) != 0;
        if !directions_are_parallel {
            return false;
        }

        let pos_on_dir_axis = self.dir().pos_on_axis(self.square());
        let stepped_pos_on_dir_axis = self.dir().pos_on_axis(self.stepped().square().into());
        let other_pos_on_dir_axis = self.dir().pos_on_axis(other_face.square().into());

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
    // TODO: return
    pub fn face_line_segment(&self) -> impl LineSegment<PointType = SquareType::Floating> {
        square_face_as_line(self.square, self.dir)
    }

    pub fn face_crosses_positive_x_axis(&self) -> bool {
        if self.square == SquareType::zero() {
            return self.direction().step() == SquareType::right();
        }

        self.square.x() > SquareType::DataType::zero()
            && self.square.y() == 0
            && self.direction().step().is_horizontal()
    }
}
impl<T: IntCoordinate> Debug for OrthogonalFacingIntPose<T>
where
    Self: Display,
{
    fn fmt(&self, mut f: &mut Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&(&self), &mut f)
    }
}
impl<T: IntCoordinate> Display for OrthogonalFacingIntPose<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // TODO: tidy
        write!(
            f,
            "Pos: (x:{}, y:{}), Dir: (x: {}, y: {}) {} ",
            self.square().x(),
            self.square().y(),
            self.dir().step().x(),
            self.dir().step().y(),
            Glyph::extract_arrow_from_arrow_string(self.dir().step().cast_unit(), FACE_ARROWS)
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

static_assertions::assert_not_impl_any!(WorldSquareWithOrthogonalDir: QuarterTurnRotatable);
static_assertions::assert_not_impl_any!(RelativeSquareWithOrthogonalDir: QuarterTurnRotatable);
static_assertions::assert_not_impl_any!(SquareWithKingDir: QuarterTurnRotatable);

impl WorldSquareWithOrthogonalDir {
    pub fn middle_point_of_face(&self) -> WorldPoint {
        self.square.to_f32() + self.direction().step().to_f32() * 0.5
    }

    // TODO: replace with just subtraction, returning whatever a relative pose is (probably a translation and rotation)
    pub fn other_pose_absolute_to_relative(&self, other: impl Into<Self>) -> Self {
        let other = other.into();

        let naive_translation: WorldStep = other.square - self.square;
        let rotation = QuarterTurnsCcw::from_start_and_end_directions(self.dir.step(), STEP_UP);
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

impl TryFrom<SquareWithKingDir> for WorldSquareWithOrthogonalDir {
    type Error = ();

    fn try_from(value: SquareWithKingDir) -> Result<Self, Self::Error> {
        if value.direction().into().is_orthogonal() {
            Ok(WorldSquareWithOrthogonalDir::from_square_and_step(
                value.square(),
                value.direction(),
            ))
        } else {
            Err(())
        }
    }
}
impl<T: IntCoordinate> Add<OrthogonalFacingIntPose<T>> for OrthogonalFacingIntPose<T> {
    type Output = Self;

    fn add(self, rhs: OrthogonalFacingIntPose<T>) -> Self::Output {
        OrthogonalFacingIntPose::from_square_and_turns(
            self.square + rhs.square,
            self.direction_in_quarter_turns() + rhs.direction_in_quarter_turns(),
        )
    }
}

impl<T: IntCoordinate> Sub<OrthogonalFacingIntPose<T>> for OrthogonalFacingIntPose<T> {
    type Output = Self;

    fn sub(self, rhs: OrthogonalFacingIntPose<T>) -> Self::Output {
        OrthogonalFacingIntPose::from_square_and_turns(
            self.square - rhs.square,
            self.direction_in_quarter_turns() - rhs.direction_in_quarter_turns(),
        )
    }
}
impl<ConvertableToSquareType, SquareType, DirectionType>
    From<(ConvertableToSquareType, DirectionType)> for OrthogonalFacingIntPose<SquareType>
where
    ConvertableToSquareType: Into<SquareType>,
    SquareType: IntCoordinate,
    DirectionType: Into<OrthogonalUnitCoordinate<SquareType>>,
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
    DirectionType: Into<OrthogonalUnitCoordinate<SquareType>>,
{
    fn from(value: (T, T, DirectionType)) -> Self {
        Self::from_square_and_step((value.0, value.1).into(), value.2)
    }
}

impl<SquareType> From<OrthogonalFacingIntPose<SquareType>>
    for (SquareType, OrthogonalUnitCoordinate<SquareType>)
where
    SquareType: WorldIntCoordinate,
{
    fn from(
        value: OrthogonalFacingIntPose<SquareType>,
    ) -> (SquareType, OrthogonalUnitCoordinate<SquareType>) {
        (value.square, value.direction())
    }
}
