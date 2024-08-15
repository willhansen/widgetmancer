use crate::utility::*;

trait_alias!(pub trait PointReqs = directed_line_cutting_grid_square::PointReqs);

pub type Shape<P: PointReqs> = ThingRelToSquare<HalfPlaneCuttingCenteredUnitSquare<P>, OnGrid<P>>;

impl<P: PointReqs> RefinementOf<HalfPlane<P>> for Shape<P>
where
    P: PointReqs,
{
    fn valid(&self) -> bool {
        todo!()
    }
}

impl<P: PointReqs> From<Shape<P>> for HalfPlane<P> {
    fn from(value: Shape<P>) -> Self {
        Self::from_border_with_inside_on_right(value.dividing_line().into())
    }
}

impl<P: PointReqs> TryFrom<HalfPlane<P>> for Shape<P> {
    type Error = String;

    fn try_from(value: HalfPlane<P>) -> Result<Self, Self::Error> {
        todo!()
    }
}

impl_complement_for_refinement!(Shape<P: PointReqs>, refinement_base= HalfPlane<P>);

impl_half_plane_ops_for_newtype!(Shape<P: PointReqs>, base= DirectedLineCuttingGridSquare<P>);
half_plane::impl_constructors_for_refinement!(Shape<P: PointReqs>, border= DirectedLineCuttingGridSquare<P>, base= HalfPlane<P>);

pub trait Operations<P: PointReqs>: halfplane::Operations<P> {
    // type PointType: FloatCoordinate;
    // TODO: change output to normalized float
    fn fraction_of_square_covered(&self) -> f32 {
        // TODO: tidy this up when halfplane is a trait
        self.to_local()
            .very_approximate_fraction_coverage_of_centered_unit_square()
    }
}

impl<P: PointReqs> Operations<P> for Shape<P> {}
