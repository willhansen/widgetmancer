use crate::utility::*;

trait_alias_macro!(pub trait PointReqsForHalfPlaneCuttingGridSquare = PointReqsForDirectedLineCuttingGridSquare);
trait_alias_macro!(trait Reqs = PointReqsForHalfPlaneCuttingGridSquare);

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct HalfPlaneCuttingGridSquare<P: Reqs>(DirectedLineCuttingGridSquare<P>);

impl<P: Reqs> NewTypeWithKnownBaseType for HalfPlaneCuttingGridSquare<P> {
    type BaseOfNewType = DirectedLineCuttingGridSquare<P>;
}

impl<P: Reqs> Refinement<HalfPlane<P>> for HalfPlaneCuttingGridSquare<P>
where
    P: Reqs,
{
    fn valid_refinement(base: HalfPlane<P>) -> bool {
        todo!()
    }
}
impl<P: Reqs> From<HalfPlaneCuttingGridSquare<P>> for HalfPlane<P> {
    fn from(value: HalfPlaneCuttingGridSquare<P>) -> Self {
        Self::from_border_with_inside_on_right(value.dividing_line().into())
    }
}

impl<P: Reqs> TryFrom<HalfPlane<P>> for HalfPlaneCuttingGridSquare<P> {
    type Error = String;

    fn try_from(value: HalfPlane<P>) -> Result<Self, Self::Error> {
        todo!()
    }
}

impl<P: Reqs> HalfPlaneOps for HalfPlaneCuttingGridSquare<P> {
    type PointType = P;

    type BorderType = DirectedLineCuttingGridSquare<P>;

    fn border_line(&self) -> Self::BorderType {
        todo!()
    }
}

pub trait HalfPlaneCuttingGridSquareOps<P: Reqs>: HalfPlaneOps<PointType = P> {
    // type PointType: FloatCoordinate;
    // fn which_square(&self) -> <P as CoordinateOps>::OnGrid; // TODO: delete
    fn which_square(&self) -> P::OnGrid;
    fn to_local(&self) -> HalfPlaneCuttingCenteredUnitSquare<P>;
    // TODO: change output to normalized float
    fn fraction_of_square_covered(&self) -> f32 {
        // TODO: tidy this up when halfplane is a trait
        self.to_local()
            .very_approximate_fraction_coverage_of_centered_unit_square()
    }
}

impl<P: Reqs> HalfPlaneCuttingGridSquareOps<P> for HalfPlaneCuttingGridSquare<P> {
    fn which_square(&self) -> P::OnGrid {
        self.dividing_line().which_square()
    }

    fn to_local(&self) -> HalfPlaneCuttingCenteredUnitSquare<P> {
        self.dividing_line().points_relative_to_the_square()
    }
}
