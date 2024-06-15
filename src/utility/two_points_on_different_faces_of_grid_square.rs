use crate::utility::*;

trait_alias_macro!(pub trait PointReqsTwoPointsOnDifferentFacesOfCenteredUnitSquare = FloatCoordinateOps);
trait_alias_macro!(trait PointReqs =PointReqsTwoPointsOnDifferentFacesOfCenteredUnitSquare);

pub type TwoPointsOnDifferentFacesOfGridSquare<P: PointReqs> =
    ThingRelToSquare<TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>, OnGrid<P>>;

impl<P: PointReqs> TwoPointsOnDifferentFacesOfGridSquare<P> {
    pub fn points_on_the_square(&self) -> TwoPointsOnDifferentFacesOfCenteredUnitSquare<P> {
        *self.thing()
    }
}
impl<P: PointReqs> OperationsForTwoDifferentPoints<P> for TwoPointsOnDifferentFacesOfGridSquare<P> {
    fn point_by_index(&self, point_index: usize) -> P {
        self.points_on_the_square().point_by_index(point_index) + self.square_center()
    }
}

// impls_for_two_different_points!(TwoPointsOnDifferentFacesOfGridSquare<P: PointReqs>);
impl_translate_for_two_points_with_restriction!(
    TwoPointsOnDifferentFacesOfGridSquare<P: PointReqs>
);

impl_constructors_for_two_different_points_for_refinement!(TwoPointsOnDifferentFacesOfGridSquare<P: PointReqs>, unrefined= TwoDifferentPoints<P>);

pub trait ConstructorsForTwoPointsOnDifferentFacesOfGridSquare<P: PointReqs>: Sized {
    fn try_new_from_line_and_square<L: DirectedFloatLineOps<P>>(
        line: L,
        square: P::OnGrid,
    ) -> Result<Self, String>;
}

impl<P: PointReqs> ConstructorsForTwoPointsOnDifferentFacesOfGridSquare<P>
    for TwoPointsOnDifferentFacesOfGridSquare<P>
{
    fn try_new_from_line_and_square<L: DirectedFloatLineOps<P>>(
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

        Ok(Self::try_from_two_exact_points(
            intersection_points[0],
            intersection_points[1],
        )?)
    }
}

pub trait OperationsForTwoPointsOnDifferentFacesOfGridSquare<P: PointReqs> {
    fn which_square(&self) -> OnGrid<P>;
    fn points_relative_to_the_square(&self) -> TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>;
}

impl<P: PointReqs> TwoPointsOnDifferentFacesOfGridSquare<P> {
    pub fn the_square(&self) -> P::OnGrid {
        self.square()
    }

    pub fn square_center(&self) -> P {
        self.the_square().to_f32()
    }

    pub fn as_local(&self) -> TwoPointsOnDifferentFacesOfCenteredUnitSquare<P> {
        self.points_on_the_square
    }
}
impl<P: PointReqs> OperationsForTwoPointsOnDifferentFacesOfGridSquare<P>
    for TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>
{
    fn which_square(&self) -> P::OnGrid {
        <P::OnGrid as euclid::num::Zero>::zero()
    }
    fn points_relative_to_the_square(&self) -> TwoPointsOnDifferentFacesOfCenteredUnitSquare<P> {
        *self
    }
}
impl<P: PointReqs> OperationsForTwoPointsOnDifferentFacesOfGridSquare<P>
    for TwoPointsOnDifferentFacesOfGridSquare<P>
{
    fn which_square(&self) -> P::OnGrid {
        self.the_square()
    }
    fn points_relative_to_the_square(&self) -> TwoPointsOnDifferentFacesOfCenteredUnitSquare<P> {
        *self.to_local()
    }
}
impl<P: PointReqs> ConstructorsForTwoDifferentPoints<P>
    for TwoPointsOnDifferentFacesOfGridSquare<P>
{
    fn try_from_two_exact_points(p1: P, p2: P) -> Result<Self, String> {
        let square_center = p1.lerp2d(p2, 0.5).round();
        let centered_p1 = p1 - square_center;
        let centered_p2 = p2 - square_center;
        Ok(Self::new(
            TwoPointsOnDifferentFacesOfCenteredUnitSquare::try_from_two_exact_points(
                centered_p1,
                centered_p2,
            )?,
            square_center.to_i32(),
        ))
    }
    fn try_from_two_points_allowing_snap_along_line(p1: P, p2: P) -> Result<Self, String> {
        let square_center = p1.lerp2d(p2, 0.5).round();
        let square: P::OnGrid = square_center.to_i32();
        let line = TwoDifferentPoints::<P>::from_two_points(p1, p2);
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
