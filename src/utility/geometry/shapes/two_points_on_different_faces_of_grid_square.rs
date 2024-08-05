use crate::utility::*;

trait_alias!(pub trait PointReqs = float_coordinate::Operations);

pub type Shape<P: PointReqs> =
    ThingRelToSquare<TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>, OnGrid<P>>;

impl<P: PointReqs> Shape<P> {
    pub fn points_on_the_square(&self) -> TwoPointsOnDifferentFacesOfCenteredUnitSquare<P> {
        *self.thing()
    }
}
// TODO: why is this implemented here?
impl<P: PointReqs> two_different_points::Operations<P> for Shape<P> {
    fn point_by_index(&self, point_index: usize) -> P {
        self.points_on_the_square().point_by_index(point_index) + self.square_center()
    }
}

impl_translate_for_refined_type!(Shape<P: PointReqs>, refinement_base= two_different_points::Shape<P>);

impl_constructors_for_two_different_points_for_refinement!(Shape<P: PointReqs>, unrefined= TwoDifferentPoints<P>);

pub trait Constructors<P: PointReqs>: Sized {
    fn try_new_from_line_and_square<L: directed_float_line::Operations<P>>(
        line: L,
        square: P::OnGrid,
    ) -> Result<Self, String>;
}

impl<P: PointReqs> Constructors<P>
    for Shape<P>
{
    fn try_new_from_line_and_square<L: Directedfloat_line::Operations<P>>(
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

pub trait Operations<P: PointReqs> {
}

impl<P: PointReqs> Shape<P> {

    pub fn square_center(&self) -> P {
        self.square().to_f32()
    }

}
// TODO: why is this implemented here?
impl<P: PointReqs> Operations<P>
    for TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>
{
}
impl<P: PointReqs> Operations<P>
    for Shape<P>
{
}
impl<P: PointReqs> ConstructorsForTwoDifferentPoints<P>
    for Shape<P>
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
