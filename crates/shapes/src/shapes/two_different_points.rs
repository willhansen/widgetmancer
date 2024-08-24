use crate::utility::*;

pub type TwoDifferentFloatPoints<U> = Shape<Point2D<f32, U>>;

trait_alias!(pub trait PointReqs = signed_coordinate::Operations);

// TODO: generalize to N points, and a refinement that the points are different
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Shape<P: PointReqs> {
    p1: P,
    p2: P,
}
impl<P: PointReqs> Shape<P> {
    // TODO: this impl should be empty

    // TODO: Should already be implied by the Abstraction implementation
    fn new_from_directed_line(line: impl directed_line::Operations<P>) -> Self {
        let [p1, p2] = line.two_points_on_line_in_order();
        Self::from_two_points(p1, p2)
    }
}

// impl<P: Coordinate> Debug for Shape<P> {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         write!(f, "p1: {:?}\tp2: {:?}", self.p1, self.p2)
//     }
// }

pub trait Operations<P: PointReqs>: Sized + Copy + PartialEq + Constructors<P> {
    fn point_by_index(&self, point_index: usize) -> P;
    fn p1(&self) -> P {
        self.point_by_index(0)
    }
    fn p2(&self) -> P {
        self.point_by_index(1)
    }
    fn points(&self) -> [P; 2] {
        self.to_array()
    }
    fn cast_unit<Other, OtherPointType>(&self) -> Other
    where
        Other: Operations<OtherPointType>,
        // TODO: find a way around not being able to restrict the base DataType associated type
        OtherPointType: PointReqs<_DataType = P::DataType>,
    {
        Other::from_array(self.to_array().map(|p| p.cast_unit()))
    }
    fn to_array(&self) -> [P; 2] {
        [0, 1].map(|i| self.point_by_index(i))
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

pub trait Constructors<P: PointReqs>: Sized {
    // entrypoint
    fn try_from_two_exact_points(p1: P, p2: P) -> Result<Self, String>;
    fn from_two_points(p1: P, p2: P) -> Self {
        Self::try_from_two_exact_points(p1, p2).unwrap()
    }
    fn from_array_of_two_exact_points(p: [P; 2]) -> Self {
        Self::try_from_array_of_two_exact_points(p).unwrap()
    }
    fn try_from_array_of_two_exact_points(p: [P; 2]) -> Result<Self, String> {
        Self::try_from_two_exact_points(p[0], p[1])
    }
    fn easy_from_two_exact_points(p1: impl Into<P>, p2: impl Into<P>) -> Self {
        Self::from_two_points(p1.into(), p2.into())
    }
    /// NOTE: This is a very similar use case as TryFromDirectedLine::from_two_ordered_points_on_line.
    /// There is a difference between "allow snapping along one axis" and "define a line, and then use the line" though.  in the second case, the hinting power of giving specific points is intentionally discarded.
    fn try_from_two_points_allowing_snap_along_line(p1: P, p2: P) -> Result<Self, String> {
        Self::try_from_two_exact_points(p1, p2)
    }
    fn from_two_points_allowing_snap_along_line(p1: P, p2: P) -> Self {
        Self::try_from_two_points_allowing_snap_along_line(p1, p2).unwrap()
    }
    fn try_from_two_points_object_allowing_snap_along_line(
        p: impl Operations<P>,
    ) -> Result<Self, String> {
        Self::try_from_two_points_allowing_snap_along_line(p.p1(), p.p2())
    }
    fn try_from_point_and_radial(
        p1: impl Into<P>,
        angle: FAngle,
        length: f32,
    ) -> Result<Self, String>
    where
        P: float_coordinate::Operations,
    {
        let p1 = p1.into();
        Self::try_from_two_exact_points(p1, floating_point_step(p1, angle, length))
    }
    fn from_point_and_radial(p1: impl Into<P>, angle: FAngle, length: f32) -> Self
    where
        P: float_coordinate::Operations,
    {
        Self::try_from_point_and_radial(p1, angle, length).unwrap()
    }
    fn from_point_and_unit_step_in_direction(
        point: impl Into<P>,
        direction: impl Into<FAngle>,
    ) -> Self
    where
        P: float_coordinate::Operations,
    {
        Self::from_point_and_radial(point.into(), direction.into(), 1.0)
    }
    fn from_array(arr: [P; 2]) -> Self {
        Self::try_from_two_exact_points(arr[0], arr[1]).unwrap()
    }
}
impl<P: PointReqs> Constructors<P> for Shape<P> {
    fn try_from_two_exact_points(p1: P, p2: P) -> Result<Self, String> {
        // TODO: move this check to refinement validation function
        if p1 == p2 {
            Err(format!("Points are equal: {:?}, {:?}", p1, p2))
        } else {
            Ok(Shape { p1, p2 })
        }
    }
}

impl<P: PointReqs, T> Constructors<P> for T
where
    T: AbstractionOf<Shape<P>>,
{
    fn try_from_two_exact_points(p1: P, p2: P) -> Result<Self, String> {
        Shape::try_from_two_exact_points(p1, p2).map(|x| x.into())
    }
}

macro_rules! impl_constructors_via_base {
    ($SelfType:ident<P: $reqs:ident>, base= $BaseType:ident$(::$BaseType2:ident)*<P>) => {
        impl<P: $reqs> two_different_points::Constructors<P> for $SelfType<P>
        {
            fn try_from_two_exact_points(p1: P, p2: P) -> Result<Self, String> {
                $BaseType$(::$BaseType2)*::<P>::try_from_two_exact_points(p1, p2)?.try_into()
            }
        }
    };
}
pub(crate) use impl_constructors_via_base;


impl<P: PointReqs> AbstractsTo<DirectedLine<P>> for Shape<P> {
    fn set_with_abstraction(&self, val: &DirectedLine<P>) -> Self {
        Self::from_point_and_unit_step_in_direction(self.p1(), val.direction())
    }
}
impl<P: PointReqs> AbstractsTo<Ray<P>> for Shape<P> {
    fn set_with_abstraction(&self, val: &Ray<P>) -> Self {
        Self::from_point_and_unit_step_in_direction(self.p1(), val.direction())
    }
}
impl<P: PointReqs> AbstractsTo<DirectedLineSegment<P>> for Shape<P> {
    fn set_with_abstraction(&self, val: &DirectedLineSegment<P>) -> Self {
        Self::from_two_points(val.start(), val.end())
    }
}

impl<P: PointReqs> Operations<P> for Shape<P> {
    fn point_by_index(&self, pi: usize) -> P {
        match pi {
            0 => self.p1,
            1 => self.p2,
            i => panic!("invalid index: {}", i),
        }
    }
}

// TODO: operations for refinements should be failable
impl<P: PointReqs, T> Operations<P> for T
where
    T: RefinementOf<Shape<P>>,
{
    fn point_by_index(&self, point_index: usize) -> P {
        let intermediate: Shape<P> = self.into();
        intermediate.point_by_index(point_index)
    }
}

// TODO: separate file and also int rays
impl<P: ray::PointReqs> ray::Operations<P> for Shape<P> {
    fn new_from_point_and_dir(point: P, dir: FAngle) -> Self
    where
        P: float_coordinate::Operations,
    {
        Self::new(point, point + P::unit_vector_from_angle(dir))
    }

    fn point(&self) -> P {
        self.p1()
    }

    fn angle(&self) -> FAngle {
        let dir = self.p2() - self.p1();
        dir.better_angle_from_x_axis()
    }
}

macro_rules! impls {
    ($TheStruct:ident<P: $PointReqs:path>) => {
        impl<P: $PointReqs> directed_line::Operations<P> for $TheStruct<P> {
            fn two_points_on_line_in_order(&self) -> [P; 2] {
                Self::to_array(self)
            }
        }

        impl<P: $PointReqs> directed_line::Constructors<P> for $TheStruct<P> {
            fn try_new_from_directed_line(
                line: impl directed_line::Operations<P>,
            ) -> Result<Self, String>
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

        impl<P: $PointReqs> line::Operations<P> for $TheStruct<P> {
            fn two_different_arbitrary_points_on_line(&self) -> [P; 2] {
                self.to_array()
            }
        }
        impl<P: $PointReqs> line::Constructors<P> for $TheStruct<P> {}

        impl<P: $PointReqs> Reversible for $TheStruct<P> {
            fn reversed(&self) -> Self {
                Self::new_from_points(self.p2(), self.p1())
            }
        }

        impl<P: $PointReqs> QuarterTurnRotatable for $TheStruct<P> {
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
pub(crate) use impls;

// TODO: combine with other macros
// TODO: remove coordinate trait parameter
impls!(Shape<P: PointReqs>);

impl<P: PointReqs> Add<P> for Shape<P> {
    type Output = Self;

    fn add(self, rhs: P) -> Self::Output {
        Self::try_new_from_points(self.p1() + rhs, self.p2() + rhs).unwrap()
    }
}
impl<P: PointReqs> Sub<P> for Shape<P> {
    type Output = Self;

    fn sub(self, rhs: P) -> Self::Output {
        Self::try_new_from_points(self.p1() - rhs, self.p2() - rhs).unwrap()
    }
}


impl<P: PointReqs, CanBePointType> From<(CanBePointType, CanBePointType)> for Shape<P>
where
    CanBePointType: Into<P>,
{
    fn from(value: (CanBePointType, CanBePointType)) -> Self {
        Self::easy_from_two_exact_points(value.0, value.1)
    }
}

impl<P: float_coordinate::Operations> From<TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>> for Shape<P> {
    fn from(value: TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>) -> Self {
        Self::from_array(value.points())
    }
}

impl<P: float_coordinate::Operations> TryFrom<Shape<P>> for TwoPointsOnDifferentFacesOfCenteredUnitSquare<P> {
    type Error = String;

    fn try_from(value: Shape<P>) -> Result<Self, Self::Error> {
        Self::try_from_two_exact_points(value.p1, value.p2)
    }
}
impl TwoDifferentWorldPoints {
    // TODO: move to line segment
    pub fn touched_squares(&self) -> Vec<WorldSquare> {
        let start_square = world_point_to_world_square(self.p1);
        let end_square = world_point_to_world_square(self.p2);
        // TODO: use better line algorithm.  Account for floating point start and ends
        line_drawing::WalkGrid::new(start_square.to_tuple(), end_square.to_tuple())
            .map(|(x, y)| point2(x, y))
            .collect_vec()
    }
}

// TODO: allow for unsigned
impl<P: PointReqs> Display for Shape<P> {
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::array::from_fn;

    #[test]
    fn test_orthogonal_line_constructors() {
        TwoDifferentWorldPoints::new_horizontal(5.0);
        TwoDifferentWorldPoints::new_vertical(5.0);

        TwoPointsOnDifferentFacesOfCenteredUnitSquare::<WorldPoint>::new_horizontal(0.3);
        TwoPointsOnDifferentFacesOfCenteredUnitSquare::<WorldPoint>::new_vertical(0.3);

        TwoPointsOnDifferentFacesOfGridSquare::<WorldPoint>::new_horizontal(0.3);
        TwoPointsOnDifferentFacesOfGridSquare::<WorldPoint>::new_vertical(0.3);
    }
    #[test]
    fn test_point_snap_along_line() {
        TwoPointsOnDifferentFacesOfCenteredUnitSquare::<WorldPoint>::from_two_points_allowing_snap_along_line(point2(0.3,1.0), point2(0.3, 0.0));
    }
    #[test]
    fn test_initilize_from_unordered_points_on_line() {
        let a = point2(5.0, 5.0);
        let b = point2(10.0, 5.0);
        let line = TwoDifferentWorldPoints::from_two_unordered_points_on_line(a, b);
        assert!(line.point_is_on_line(a));
        assert!(line.point_is_on_line(b));
    }
}
