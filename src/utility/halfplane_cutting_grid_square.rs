use crate::utility::*;

trait_alias_macro!(pub trait PointReqsForHalfPlaneCuttingGridSquare = PointReqsForDirectedLineCuttingGridSquare);
trait_alias_macro!(trait PointReqs = PointReqsForHalfPlaneCuttingGridSquare);

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct HalfPlaneCuttingGridSquare<P: PointReqs>(DirectedLineCuttingGridSquare<P>);


impl<P: PointReqs> Refinement<HalfPlane<P>> for HalfPlaneCuttingGridSquare<P>
where
    P: PointReqs,
{
    fn valid_refinement(base: HalfPlane<P>) -> bool {
        todo!()
    }
}
impl<P: PointReqs> From<HalfPlaneCuttingGridSquare<P>> for HalfPlane<P> {
    fn from(value: HalfPlaneCuttingGridSquare<P>) -> Self {
        Self::from_border_with_inside_on_right(value.dividing_line().into())
    }
}

impl<P: PointReqs> TryFrom<HalfPlane<P>> for HalfPlaneCuttingGridSquare<P> {
    type Error = String;

    fn try_from(value: HalfPlane<P>) -> Result<Self, Self::Error> {
        todo!()
    }
}

impl_quarter_turn_rotatable_for_impl_half_plane_ops!(HalfPlaneCuttingGridSquare<P: PointReqs>);

impl_complement_for_newtype!(HalfPlaneCuttingGridSquare<P: PointReqs>);

impl<P: PointReqs> HalfPlaneOps<P> for HalfPlaneCuttingGridSquare<P> {
    type BorderType = DirectedLineCuttingGridSquare<P>;

    fn border_line(&self) -> Self::BorderType {
        todo!()
    }
}

pub trait HalfPlaneCuttingGridSquareOps<P: PointReqs>: HalfPlaneOps<P> {
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

impl<P: PointReqs> HalfPlaneCuttingGridSquareOps<P> for HalfPlaneCuttingGridSquare<P> {
    fn which_square(&self) -> P::OnGrid {
        self.dividing_line().which_square()
    }

    fn to_local(&self) -> HalfPlaneCuttingCenteredUnitSquare<P> {
        self.dividing_line().points_relative_to_the_square()
    }
}
