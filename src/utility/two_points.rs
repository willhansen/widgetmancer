use crate::utility::*;

pub type TwoDifferentFloatPoints<U> = TwoDifferentPoints<Point2D<f32, U>>;

// TODO: convert to trait
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TwoDifferentPoints<PointType: Coordinate> {
    p1: PointType,
    p2: PointType,
}
impl<P: Coordinate> TwoDifferentPoints<P> {
    pub fn reversed(&self) -> Self {
        Self::try_new_from_points(self.p2, self.p1).unwrap()
    }
    fn new_from_directed_line(line: impl DirectedLine<PointType = P>) -> Self {
        let [p1, p2] = line.two_points_on_line_in_order();
        Self::new_from_points(p1, p2)
    }
}
pub trait TwoPointsOnASquareTrait<P: FloatCoordinate> {
    fn which_square(&self) -> P::OnGrid;
    fn points_relative_to_the_square(&self) -> TwoDifferentPointsOnCenteredUnitSquare<P>;
}

impl<P: FloatCoordinate> TwoPointsOnASquareTrait<P> for TwoDifferentPointsOnCenteredUnitSquare<P> {
    fn which_square(&self) -> P::OnGrid {
        <P::OnGrid as euclid::num::Zero>::zero()
    }
    fn points_relative_to_the_square(&self) -> TwoDifferentPointsOnCenteredUnitSquare<P> {
        *self
    }
}
impl<P: FloatCoordinate> TwoPointsOnASquareTrait<P> for TwoDifferentPointsOnGridSquare<P> {
    fn which_square(&self) -> P::OnGrid {
        self.the_square()
    }
    fn points_relative_to_the_square(&self) -> TwoDifferentPointsOnCenteredUnitSquare<P> {
        self.as_local()
    }
}

// TODO: Switch from template to associated point type
pub trait TwoPointsWithRestriction<P: Coordinate>:
    Sized + Copy + PartialEq + TryFromTwoPoints<P>
{
    #[deprecated(note = "use TryFromTwoPoints::try_from_two_points instead")]
    fn try_new_from_points(p1: P, p2: P) -> Result<Self, String> {
        Self::try_from_two_exact_points(p1, p2)
    }
    fn point_by_index(&self, point_index: usize) -> P;
    fn new(p1: P, p2: P) -> Self {
        Self::try_new_from_points(p1, p2).unwrap()
    }
    fn new_from_points(p1: P, p2: P) -> Self {
        Self::new(p1, p2)
    }
    fn try_new_from_point_and_radial(
        p1: impl Into<P>,
        angle: FAngle,
        length: f32,
    ) -> Result<Self, String>
    where
        P: FloatCoordinate,
    {
        let p1 = p1.into();
        Self::try_new_from_points(p1, naive_ray_endpoint(p1, angle, length))
    }
    fn new_from_point_and_radial(p1: impl Into<P>, angle: FAngle, length: f32) -> Self
    where
        P: FloatCoordinate,
    {
        Self::try_new_from_point_and_radial(p1, angle, length).unwrap()
    }
    fn p1(&self) -> P {
        self.point_by_index(0)
    }
    fn p2(&self) -> P {
        self.point_by_index(1)
    }
    fn cast_unit<Other, OtherPointType>(&self) -> Other
    where
        Other: TwoPointsWithRestriction<OtherPointType>,
        OtherPointType: Coordinate<DataType = P::DataType>,
    {
        Other::from_array(self.to_array().map(|p| p.cast_unit()))
    }
    fn to_array(&self) -> [P; 2] {
        [0, 1].map(|i| self.point_by_index(i))
    }
    fn from_array(arr: [P; 2]) -> Self {
        Self::try_new_from_points(arr[0], arr[1]).unwrap()
    }
    fn x_min(&self) -> P::DataType {
        min_for_partial_ord(self.p1().x(), self.p2().x())
    }
    fn x_max(&self) -> P::DataType {
        max_for_partial_ord(self.p1().x(), self.p2().x())
    }
    fn y_min(&self) -> P::DataType {
        min_for_partial_ord(self.p1().y(), self.p2().y())
    }
    fn y_max(&self) -> P::DataType {
        max_for_partial_ord(self.p1().y(), self.p2().y())
    }
    fn width(&self) -> P::DataType {
        self.x_max() - self.x_min()
    }
    fn height(&self) -> P::DataType {
        self.y_max() - self.y_min()
    }
}

impl<P: SignedCoordinate> FromDirectedLine<P> for TwoDifferentPoints<P> {
    fn try_new_from_directed_line(line: impl DirectedLine<PointType = P>) -> Result<Self, String>
    where
        Self: Sized,
    {
        let [p1, p2] = line.two_points_on_line_in_order();
        Ok(Self::new_from_points(p1, p2))
    }
}
impl<P: SignedCoordinate> FromLine<P> for TwoDifferentPoints<P> {
    fn try_new_from_line(line: impl Line<PointType = P>) -> Result<Self, String>
    where
        Self: Sized,
    {
        Self::try_new_from_directed_line(line.with_arbitrary_direction())
    }
}
impl<P: Coordinate> TryFromTwoPoints<P> for TwoDifferentPoints<P> {
    fn try_from_two_exact_points(p1: P, p2: P) -> Result<Self, String> {
        if p1 == p2 {
            Err(format!("Points are equal: {:?}, {:?}", p1, p2))
        } else {
            Ok(TwoDifferentPoints { p1, p2 })
        }
    }
}

impl<P: Coordinate> TwoPointsWithRestriction<P> for TwoDifferentPoints<P> {
    fn point_by_index(&self, pi: usize) -> P {
        match pi {
            0 => self.p1,
            1 => self.p2,
            i => panic!("invalid index: {}", i),
        }
    }
}
impl<P: FloatCoordinate> Ray for TwoDifferentPoints<P> {
    type PointType = P;

    fn new_from_point_and_dir(point: Self::PointType, dir: FAngle) -> Self
    where
        P: FloatCoordinate,
    {
        Self::new(point, point + Self::PointType::unit_vector_from_angle(dir))
    }

    fn point(&self) -> Self::PointType {
        self.p1()
    }

    fn angle(&self) -> FAngle {
        let dir = self.p2() - self.p1();
        dir.better_angle_from_x_axis()
    }
}
// TODO: Maybe add restriction that the points are also on different faces of the square.
// TODO: Make this just a special case for TwoDifferentPointsOnGridSquare, where the grid square is (0,0).
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct TwoDifferentPointsOnCenteredUnitSquare<P: Coordinate>(TwoDifferentPoints<P>);

impl<P: FloatCoordinate> TwoPointsWithRestriction<P> for TwoDifferentPointsOnCenteredUnitSquare<P> {
    fn point_by_index(&self, pi: usize) -> P {
        self.0.point_by_index(pi)
    }
}

impl<P: FloatCoordinate> TwoDifferentPointsOnCenteredUnitSquare<P> {}

impl<P: FloatCoordinate> FromDirectedLine<P> for TwoDifferentPointsOnCenteredUnitSquare<P> {
    fn try_new_from_directed_line(line: impl DirectedLine<PointType = P>) -> Result<Self, String>
    where
        Self: Sized,
    {
        let points: Vec<P> = line.ordered_line_intersections_with_centered_unit_square();
        if points.len() < 2 {
            Err(format!(
                "Wrong number of intersection points: {:?},",
                points
            ))
        } else {
            Self::try_new_from_points(points[0], points[1])
        }
    }
}
impl<P: FloatCoordinate> FromLine<P> for TwoDifferentPointsOnCenteredUnitSquare<P> {
    fn try_new_from_line(line: impl Line<PointType = P>) -> Result<Self, String>
    where
        Self: Sized,
    {
        Self::try_new_from_directed_line(line.with_arbitrary_direction())
    }
}
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct TwoDifferentPointsOnGridSquare<P: Coordinate> {
    points_on_the_square: TwoDifferentPointsOnCenteredUnitSquare<P>,
    the_square: P::OnGrid,
}

impl<P: FloatCoordinate> TwoPointsWithRestriction<P> for TwoDifferentPointsOnGridSquare<P> {
    fn point_by_index(&self, point_index: usize) -> P {
        self.points_on_the_square.point_by_index(point_index) + self.square_center()
    }
}
impl<P> TwoDifferentPointsOnGridSquare<P>
where
    P: FloatCoordinate,
{
    pub fn try_new_from_line_and_square<L: DirectedFloatLine<_PointType = P>>(
        line: L,
        square: P::OnGrid,
    ) -> Result<Self, String> {
        let intersection_points = line.ordered_line_intersections_with_square(square);
        if intersection_points.len() != 2 {
            return Err(format!(
                "Wrong number of intersection points: {:?},",
                intersection_points
            ));
        }

        Ok(Self::try_new_from_points(
            intersection_points[0],
            intersection_points[1],
        )?)
    }
    pub fn the_square(&self) -> P::OnGrid {
        self.the_square
    }

    pub fn square_center(&self) -> P {
        self.the_square().to_f32()
    }

    pub fn as_local(&self) -> TwoDifferentPointsOnCenteredUnitSquare<P> {
        self.points_on_the_square
    }
}
impl<PointType: SignedCoordinate> LineLike for TwoDifferentPoints<PointType> {
    type PointType = PointType;
    fn two_different_arbitrary_points_on_line(&self) -> [PointType; 2] {
        [self.p2, self.p1] // order chosen by coin flip
    }
}
macro_rules! impl_for_two_different_points {
    ($TheStruct:ident, $point_trait:ident) => {
        impl<PointType: $point_trait> DirectedLineLike for $TheStruct<PointType> {
            fn two_points_on_line_in_order(&self) -> [Self::PointType; 2] {
                <Self as TwoPointsWithRestriction<PointType>>::to_array(self)
            }
        }

        impl<PointType: $point_trait> Line for $TheStruct<PointType> {}

        impl<PointType: $point_trait> Reversible for $TheStruct<PointType> {
            fn reversed(&self) -> Self {
                Self::new_from_points(self.p2(), self.p1())
            }
        }

        impl<PointType: $point_trait> QuarterTurnRotatable for $TheStruct<PointType> {
            fn quarter_rotated_ccw(
                &self,
                quarter_turns_ccw: impl Into<NormalizedOrthoAngle>,
            ) -> Self {
                let quarter_turns_ccw = quarter_turns_ccw.into();
                let new_points = self
                    .to_array()
                    .map(|p| p.quarter_rotated_ccw(quarter_turns_ccw));
                Self::from_array(new_points)
            }
        }
    };
}

// TODO: combine with other macros
// TODO: remove coordinate trait parameter
impl_for_two_different_points!(TwoDifferentPoints, SignedCoordinate);
impl_for_two_different_points!(TwoDifferentPointsOnCenteredUnitSquare, FloatCoordinate);
impl_for_two_different_points!(TwoDifferentPointsOnGridSquare, FloatCoordinate);

macro_rules! impl_traits_for_two_points_with_restriction {
    ($TheStruct:ident) => {
        impl<P> Add<P> for $TheStruct<P>
        where
            Self: TwoPointsWithRestriction<P>,
            P: Coordinate,
        {
            type Output = Self;

            fn add(self, rhs: P) -> Self::Output {
                Self::try_new_from_points(self.p1() + rhs, self.p2() + rhs).unwrap()
            }
        }
        impl<P> Sub<P> for $TheStruct<P>
        where
            Self: TwoPointsWithRestriction<P>,
            P: Coordinate,
        {
            type Output = Self;

            fn sub(self, rhs: P) -> Self::Output {
                Self::try_new_from_points(self.p1() - rhs, self.p2() - rhs).unwrap()
            }
        }
    };
}

// TODO: combine into one macro call
impl_traits_for_two_points_with_restriction!(TwoDifferentPoints);
impl_traits_for_two_points_with_restriction!(TwoDifferentPointsOnCenteredUnitSquare);
impl_traits_for_two_points_with_restriction!(TwoDifferentPointsOnGridSquare);

impl<PointType: FloatCoordinate> LineLike for TwoDifferentPointsOnCenteredUnitSquare<PointType> {
    type PointType = PointType;
    // fn new_from_two_points_on_line(p1: impl Into<PointType>, p2: impl Into<PointType>) -> Self {
    //     let less_constrained_line = TwoDifferentPoints::new_from_two_points_on_line(p1, p2);
    //     Self::try_from_line(less_constrained_line).unwrap()
    // }

    fn two_different_arbitrary_points_on_line(&self) -> [PointType; 2] {
        self.0.two_different_arbitrary_points_on_line()
    }
}
impl<PointType: FloatCoordinate> LineLike for TwoDifferentPointsOnGridSquare<PointType> {
    type PointType = PointType;

    fn two_different_arbitrary_points_on_line(&self) -> [PointType; 2] {
        [0, 1].map(|i| self.point_by_index(i))
    }
}
impl<PointType: SignedCoordinate, CanBePointType> From<(CanBePointType, CanBePointType)>
    for TwoDifferentPoints<PointType>
where
    CanBePointType: Into<PointType>,
{
    fn from(value: (CanBePointType, CanBePointType)) -> Self {
        Self::easy_from_two_exact_points(value.0, value.1)
    }
}

// TODO: Can generalize to any line from any line?
impl<P: FloatCoordinate> From<TwoDifferentPointsOnCenteredUnitSquare<P>> for TwoDifferentPoints<P> {
    fn from(value: TwoDifferentPointsOnCenteredUnitSquare<P>) -> Self {
        value.0
    }
}

impl<P: FloatCoordinate> TryFrom<TwoDifferentPoints<P>>
    for TwoDifferentPointsOnCenteredUnitSquare<P>
{
    type Error = String;

    fn try_from(value: TwoDifferentPoints<P>) -> Result<Self, Self::Error> {
        Self::try_new_from_points(value.p1, value.p2)
    }
}
impl TwoDifferentWorldPoints {
    pub fn touched_squares(&self) -> Vec<WorldSquare> {
        let start_square = world_point_to_world_square(self.p1);
        let end_square = world_point_to_world_square(self.p2);
        // TODO: use better line algorithm.  Account for floating point start and ends
        line_drawing::WalkGrid::new(start_square.to_tuple(), end_square.to_tuple())
            .map(|(x, y)| point2(x, y))
            .collect_vec()
    }
}

impl<PointType: SignedCoordinate> Display for TwoDifferentPoints<PointType> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "p1: {}, p2: {}\n\tx-intercept: {}\n\ty-intercept: {}\n\tslope: {}",
            self.p1.to_string(),
            self.p2.to_string(),
            self.x_intercept()
                .map_or("N/A".to_owned(), |v| v.to_string()),
            self.y_intercept()
                .map_or("N/A".to_owned(), |v| v.to_string()),
            self.slope().map_or("inf".to_owned(), |v| v.to_string()),
        )
    }
}

impl<P: FloatCoordinate> TryFromTwoPoints<P> for TwoDifferentPointsOnCenteredUnitSquare<P> {
    fn try_from_two_exact_points(p1: P, p2: P) -> Result<Self, String> {
        // TODO: Add a tolerance to this check, or maybe snap to square along angle from origin
        let points_are_valid = p1.on_centered_unit_square() && p2.on_centered_unit_square();
        if points_are_valid {
            Ok(Self(TwoDifferentPoints::try_new_from_points(p1, p2)?))
        } else {
            Err(format!(
                "At least one point not on centered unit square: {:?}, {:?}",
                p1, p2
            ))
        }
    }
    fn try_from_two_points_allowing_snap_along_line(p1: P, p2: P) -> Result<Self, String> {
        Self::try_new_from_two_ordered_points_on_line(p1, p2)
    }
}
impl<P: FloatCoordinate> TryFromTwoPoints<P> for TwoDifferentPointsOnGridSquare<P> {
    fn try_from_two_exact_points(p1: P, p2: P) -> Result<Self, String> {
        // NOTE: this potentially leaves ambiguity between two squares if the points are both on the same face of a square.  Tie break by default rounding direction for now because why not (I think it's away from zero).
        let square_center = p1.lerp2d(p2, 0.5).round();
        let centered_p1 = p1 - square_center;
        let centered_p2 = p2 - square_center;
        Ok(Self {
            points_on_the_square: TwoDifferentPointsOnCenteredUnitSquare::try_new_from_points(
                centered_p1,
                centered_p2,
            )?,
            the_square: square_center.to_i32(),
        })
    }
    fn try_from_two_points_allowing_snap_along_line(p1: P, p2: P) -> Result<Self, String> {
        let square_center = p1.lerp2d(p2, 0.5).round();
        let square: P::OnGrid = square_center.to_i32();
        let line = TwoDifferentPoints::<P>::from_two_exact_points(p1, p2);
        let intersections = line.ordered_line_intersections_with_square(square);
        let Ok(two_points): Result<[P; 2], _> = intersections.clone().try_into() else {
            return Err(format!(
                "There are not two intersection points with square {:?}: {:?}",
                square, intersections
            ));
        };
        Self::try_from_array_of_two_exact_points(two_points)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::array::from_fn;

    #[test]
    fn test_orthogonal_line_constructors() {
        TwoDifferentWorldPoints::new_horizontal(5.0);
        TwoDifferentWorldPoints::new_vertical(5.0);

        TwoDifferentPointsOnCenteredUnitSquare::<WorldPoint>::new_horizontal(0.3);
        TwoDifferentPointsOnCenteredUnitSquare::<WorldPoint>::new_vertical(0.3);

        TwoDifferentPointsOnGridSquare::<WorldPoint>::new_horizontal(0.3);
        TwoDifferentPointsOnGridSquare::<WorldPoint>::new_vertical(0.3);
    }
    #[test]
    fn test_point_snap_along_line() {
        TwoDifferentPointsOnCenteredUnitSquare::<WorldPoint>::from_two_points_allowing_snap_along_line(point2(0.3,1.0), point2(0.3, 0.0));
    }
}
