use std::{
    fmt::{Debug, Display, Formatter},
    ops::{Add, Sub},
};

use map_macro::hash_set;
use misc_utilities::*;
use angles::*;
use coordinates::*;
// use static_assertions;

#[derive(Clone, Copy, Hash, Eq, PartialEq)]
pub struct SquareWithOrthogonalDirection<SquareType>
where
    SquareType: IntCoordinateOperations,
{
    square: SquareType,
    dir: OrthogonalDirection,
}

impl<SquareType> SquareWithOrthogonalDirection<SquareType>
where
    SquareType: IntCoordinateOperations,
{
    pub fn square(&self) -> SquareType {
        self.square
    }
    pub fn dir(&self) -> OrthogonalDirection {
        self.dir
    }

    pub fn angle(&self) -> NormalizedOrthoAngle {
        self.dir.into()
    }
    pub fn from_square_and_step(
        square: impl Into<SquareType>,
        step: impl Into<SquareType>,
    ) -> Self {
        let step = step.into();
        Self::from_square_and_dir(
            square,
            OrthogonalDirection::try_from_coordinate(step).unwrap(),
        )
    }
    pub fn from_square_and_dir(
        square: impl Into<SquareType>,
        direction: impl Into<OrthogonalDirection>,
    ) -> Self {
        Self {
            square: square.into(),
            dir: direction.into(),
        }
    }
    pub fn from_x_y_dir(
        x: SquareType::DataType,
        y: SquareType::DataType,
        dir: OrthogonalDirection,
    ) -> Self {
        Self::from_square_and_dir((x, y), dir)
    }
    pub fn direction(&self) -> OrthogonalDirection {
        self.dir
    }
    pub fn stepped(&self) -> Self {
        Self::from_square_and_dir(self.square + self.direction().to_step(), self.direction())
    }
    pub fn stepped_n(&self, n: i32) -> Self {
        Self::from_square_and_dir(
            self.square + self.direction().to_step::<SquareType>() * n,
            self.direction(),
        )
    }
    pub fn stepped_back(&self) -> Self {
        Self::from_square_and_dir(self.square - self.direction().to_step(), self.direction())
    }
    pub fn strafed_left(&self) -> Self {
        self.strafed_right_n(-1)
    }
    pub fn strafed_right(&self) -> Self {
        self.strafed_right_n(1)
    }
    pub fn strafed_right_n(&self, n: i32) -> Self {
        Self::from_square_and_step(
            self.square + self.right().to_step::<SquareType>() * n,
            self.direction(),
        )
    }
    pub fn strafed_left_n(&self, n: i32) -> Self {
        self.strafed_right_n(-n)
    }

    pub fn quarter_revolved_ccw_around_origin(
        &self,
        quarter_turns_ccw: impl Into<NormalizedOrthoAngle>,
    ) -> Self {
        let quarter_turns_ccw = quarter_turns_ccw.into();
        (
            self.square.quarter_rotated_ccw(quarter_turns_ccw),
            self.dir.quarter_rotated_ccw(quarter_turns_ccw),
        )
            .try_into()
            .unwrap()
    }
    pub fn quarter_rotated_ccw_in_place(
        &self,
        quarter_turns_ccw: impl Into<NormalizedOrthoAngle> + Copy,
    ) -> Self {
        (self.square, self.dir.quarter_rotated_ccw(quarter_turns_ccw))
            .try_into()
            .unwrap()
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
    fn left(&self) -> OrthogonalDirection {
        self.direction().left()
    }
    fn right(&self) -> OrthogonalDirection {
        self.direction().right()
    }
    pub fn turned_back(&self) -> Self {
        Self::from_square_and_dir(self.square, -self.direction())
    }
    pub fn with_offset(&self, offset: SquareType) -> Self {
        Self::from_square_and_step(self.square + offset, self.dir())
    }
    pub fn at_square(&self, position: impl Into<SquareType>) -> Self {
        Self::from_square_and_step(position, self.dir())
    }
    pub fn with_direction(&self, dir: impl Into<OrthogonalDirection>) -> Self {
        Self::from_square_and_dir(self.square, dir)
    }
    pub fn reversed(&self) -> Self {
        self.with_direction(-self.direction())
    }
}
impl<T: IntCoordinateOperations> Debug for SquareWithOrthogonalDirection<T>
where
    Self: Display,
{
    fn fmt(&self, mut f: &mut Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&(&self), &mut f)
    }
}
impl<T: IntCoordinateOperations> Display for SquareWithOrthogonalDirection<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // TODO: tidy
        write!(
            f,
            "Pos: (x:{}, y:{}), Dir: {}",
            self.square().x(),
            self.square().y(),
            self.dir()
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

static_assertions::assert_not_impl_any!(SquareWithOrthogonalDirection: QuarterTurnRotatable);
static_assertions::assert_not_impl_any!(RelativeSquareWithOrthogonalDir: QuarterTurnRotatable);
static_assertions::assert_not_impl_any!(SquareWithKingDir: QuarterTurnRotatable);

// TODO: Generalize these functions for any unit
impl SquareWithOrthogonalDirection {

    // TODO: replace with just subtraction, returning whatever a relative pose is (probably a translation and rotation)
    pub fn other_pose_absolute_to_relative(&self, other: impl Into<Self>) -> Self {
        let other = other.into();

        let naive_translation: WorldStep = other.square - self.square;
        let rotation =
            NormalizedOrthoAngle::from_start_and_end_directions(self.dir.to_step(), STEP_UP);
        Self::from_square_and_step(
            naive_translation.quarter_rotated_ccw(rotation),
            other.dir.quarter_rotated_ccw(rotation),
        )
    }
    pub fn convert_other_pose_from_using_this_origin_to_absolute_origin(
        &self,
        other: impl Into<Self>,
    ) -> Self {
        let other: Self = other.into();

        let relative_translation: WorldStep = other.square;
        let rotation =
            NormalizedOrthoAngle::from_start_and_end_directions(self.dir.to_step(), STEP_UP);
        Self::from_square_and_step(
            self.square + relative_translation.quarter_rotated_ccw(-rotation),
            other.dir.quarter_rotated_ccw(-rotation),
        )
    }
    // TODO: Rename
    pub fn other_square_absolute_to_relative(&self, other: impl Into<WorldSquare>) -> WorldStep {
        self.other_pose_absolute_to_relative((other.into(), STEP_UP))
            .square()
    }
    // TODO: Rename
    pub fn other_square_relative_to_absolute(&self, other: impl Into<WorldStep>) -> WorldSquare {
        self.convert_other_pose_from_using_this_origin_to_absolute_origin((other.into(), STEP_UP))
            .square()
    }
}

impl TryFrom<SquareWithKingDir> for SquareWithOrthogonalDirection {
    type Error = &'static str;

    fn try_from(value: SquareWithKingDir) -> Result<Self, Self::Error> {
        Ok(Self::from_square_and_step(
            value.square(),
            value.direction().step(),
        ))
    }
}

impl<T: IntCoordinateOperations> Add<SquareWithOrthogonalDirection<T>> for SquareWithOrthogonalDirection<T> {
    type Output = Self;

    fn add(self, rhs: SquareWithOrthogonalDirection<T>) -> Self::Output {
        SquareWithOrthogonalDirection::from_square_and_dir(
            self.square + rhs.square,
            self.angle() + rhs.angle(),
        )
    }
}

impl<T: IntCoordinateOperations> Sub<SquareWithOrthogonalDirection<T>> for SquareWithOrthogonalDirection<T> {
    type Output = Self;

    fn sub(self, rhs: SquareWithOrthogonalDirection<T>) -> Self::Output {
        SquareWithOrthogonalDirection::from_square_and_dir(
            self.square - rhs.square,
            self.angle() - rhs.angle(),
        )
    }
}

// Generic pair to single type thing
// TODO: implement for base Pose trait
// TODO: make base Pose trait
impl<IntoSquareType, IntoStepType, SquareType> From<(IntoSquareType, IntoStepType)>
    for SquareWithOrthogonalDirection<SquareType>
where
    SquareType: IntCoordinateOperations,
    IntoSquareType: Into<SquareType>,
    IntoStepType: Into<SquareType>,
{
    // type Error = &'static str;

    // fn try_from(value: (IntoSquareType, IntoStepType)) -> Result<Self, Self::Error> {
    fn from(value: (IntoSquareType, IntoStepType)) -> Self {
        Self::from_square_and_step(value.0.into(), value.1.into())
    }
}

impl<T, SquareType, IntoDir> From<(T, T, IntoDir)> for SquareWithOrthogonalDirection<SquareType>
where
    (T, T): Into<SquareType>,
    SquareType: WorldIntCoordinateOps,
    IntoDir: Into<OrthogonalDirection>,
{
    fn from(value: (T, T, IntoDir)) -> Self {
        Self::from_square_and_dir((value.0, value.1).into(), value.2)
    }
}

impl<SquareType> From<SquareWithOrthogonalDirection<SquareType>> for (SquareType, OrthogonalDirection)
where
    SquareType: WorldIntCoordinateOps,
{
    fn from(value: SquareWithOrthogonalDirection<SquareType>) -> (SquareType, OrthogonalDirection) {
        (value.square, value.direction())
    }
}

// TODO: Add tolerance?
// TODO: Was in float_coordinates.  Need to adapt
// fn on_same_square_face(&self, other: Self) -> bool {
//     HashSet::<SquareWithOrthogonalDirection<Self::OnGrid>>::from_iter(self.touched_square_faces())
//         .intersection(&HashSet::from_iter(other.touched_square_faces()))
//         .count()
//         > 0
// }
// // TODO: Add tolerance?
// fn touched_square_faces(&self) -> HashSet<SquareWithOrthogonalDirection<Self::OnGrid>> {
//     let on_border_by_axis = [0, 1].map(|i| self.on_square_border_on_axis(i));
//     match on_border_by_axis {
//         [true, true] => [-1, 1]
//             .into_iter()
//             .cartesian_product([-1, 1])
//             .flat_map(|(x_nudge, y_nudge)| {
//                 let nudge_vector = Self::OnGrid::new(x_nudge, y_nudge);
//                 let offset_point = *self + nudge_vector.to_f32() * 0.1;
//                 let square = offset_point.snap_to_grid();
//                 [
//                     (square, NonZeroSign::try_from(-x_nudge).unwrap() * RIGHT).into(),
//                     (square, NonZeroSign::try_from(-y_nudge).unwrap() * UP).into(),
//                 ]
//             })
//             .collect(),
//         [false, false] => hash_set![],
//         [x_border, y_border] => {
//             let border_axis_index = if x_border { 0 } else { 1 };
//             let non_border_axis_index = 1 - border_axis_index;
//             let normal_to_border = Self::nth_basis_vector(border_axis_index);
//             let one_face = Face::from_square_and_dir(
//                 (*self + normal_to_border * 0.1).snap_to_grid(),
//                 -normal_to_border.nearest_orthogonal_direction(),
//             );
//             HashSet::from(one_face.both_sides_of_face())
//         }
//     }
// }

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


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_touched_square_faces() {
        [
            ((0.0, 0.0), hash_set![]),
            ((0.5, 0.0), hash_set![(0, 0, RIGHT), (1, 0, LEFT)]),
            ((-8.5, 0.0), hash_set![(-9, 0, RIGHT), (-8, 0, LEFT)]),
            ((0.2, 0.5), hash_set![(0, 0, UP), (0, 1, DOWN)]),
            (
                (0.5, 0.5),
                hash_set![
                    (0, 0, RIGHT),
                    (1, 0, LEFT),
                    (0, 1, RIGHT),
                    (1, 1, LEFT),
                    (0, 0, UP),
                    (0, 1, DOWN),
                    (1, 0, UP),
                    (1, 1, DOWN),
                ],
            ),
            (
                (-0.5, 0.5),
                hash_set![
                    (-1, 0, RIGHT),
                    (0, 0, LEFT),
                    (-1, 1, RIGHT),
                    (0, 1, LEFT),
                    (-1, 0, UP),
                    (-1, 1, DOWN),
                    (0, 0, UP),
                    (0, 1, DOWN),
                ],
            ),
        ]
        .into_iter()
        .for_each(|((x, y), faces)| {
            let faces: HashSet<SquareWithOrthogonalDirection<WorldSquare>> = map_into(faces).collect();
            assert_eq!(WorldPoint::new(x, y).touched_square_faces(), faces);
        })
    }
}
