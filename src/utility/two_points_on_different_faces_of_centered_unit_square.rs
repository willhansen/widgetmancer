use crate::utility::*;

trait_alias_macro!(pub trait PointReqsForTwoPointsOnDifferentFaces = PointReqsForTwoDifferentPoints + FloatCoordinateOps);
// pub trait PointReqsForTwoPointsOnDifferentFaces: PointReqsForTwoDifferentPoints + FloatCoordinateOps { }
// impl<T> PointReqsForTwoPointsOnDifferentFaces for T where T: PointReqsForTwoDifferentPoints + FloatCoordinateOps { }

// TODO: Make this just a special case for TwoDifferentPointsOnGridSquare, where the grid square is (0,0).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TwoPointsOnDifferentFacesOfCenteredUnitSquare<P: PointReqsForTwoPointsOnDifferentFaces>(
    TwoDifferentPoints<P>,
);

impls_for_two_different_points!(
    TwoPointsOnDifferentFacesOfCenteredUnitSquare<P: FloatCoordinateOps>
);
impl_translate_for_two_points_with_restriction!(
    TwoPointsOnDifferentFacesOfCenteredUnitSquare<P: FloatCoordinateOps>
);

impl<P: PointReqsForTwoPointsOnDifferentFaces> TwoPointsWithRestriction<P>
    for TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>
{
    fn point_by_index(&self, pi: usize) -> P {
        self.0.point_by_index(pi)
    }
}
impl<P: FloatCoordinateOps> Display for TwoPointsOnDifferentFacesOfCenteredUnitSquare<P> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TwoPointsOnDifferentFacesOfCenteredUnitSquare")
            .field("points", &self.0)
            .field(
                "point angles",
                &self.points().map(|p| {
                    let a = p.better_angle_from_x_axis();
                    format!(
                        "radians: {}\t degrees: {}\t turns:   {}",
                        a.radians,
                        a.to_degrees(),
                        a.radians / TAU
                    )
                }),
            )
            .finish()
    }
}

impl<P: FloatCoordinateOps> TwoPointsOnDifferentFacesOfCenteredUnitSquare<P> {
    fn points_are_valid(p1: P, p2: P) -> bool {
        p1.on_centered_unit_square() && p2.on_centered_unit_square() && !p1.on_same_square_face(p2)
    }
}

impl<P: FloatCoordinateOps> DirectedLineConstructors
    for TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>
{
    type _PointType = P;
    fn try_new_from_directed_line(
        line: impl DirectedLineOps<PointType = Self::_PointType>,
    ) -> Result<Self, String>
    where
        Self: Sized,
    {
        let points: Vec<Self::_PointType> =
            line.ordered_line_intersections_with_centered_unit_square();
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
impl<PointType: FloatCoordinateOps> LineOps
    for TwoPointsOnDifferentFacesOfCenteredUnitSquare<PointType>
{
    type PointType = PointType;
    // type P = Self::PointType;
    // fn new_from_two_points_on_line(p1: impl Into<PointType>, p2: impl Into<PointType>) -> Self {
    //     let less_constrained_line = TwoDifferentPoints::new_from_two_points_on_line(p1, p2);
    //     Self::try_from_line(less_constrained_line).unwrap()
    // }

    fn two_different_arbitrary_points_on_line(&self) -> [PointType; 2] {
        self.0.two_different_arbitrary_points_on_line()
    }
}
impl<P: FloatCoordinateOps> ConstructorsForTwoDifferentPoints<P>
    for TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>
{
    fn try_from_two_exact_points(p1: P, p2: P) -> Result<Self, String> {
        // TODO: Add a tolerance to this check, or maybe snap to square along angle from origin
        if Self::points_are_valid(p1, p2) {
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
