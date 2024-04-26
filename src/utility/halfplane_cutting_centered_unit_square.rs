use crate::utility::*;

trait_alias_macro!(pub trait PointReqsForHalfPlaneCuttingCenteredUnitSquare = PointReqsForDirectedLineCuttingCenteredUnitSquare);
trait_alias_macro!(trait Reqs =  PointReqsForHalfPlaneCuttingCenteredUnitSquare);

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct HalfPlaneCuttingCenteredUnitSquare<P: Reqs>(DirectedLineCuttingCenteredUnitSquare<P>);

impl<P: Reqs> NewType for HalfPlaneCuttingCenteredUnitSquare<P> {}
impl<P: Reqs> NewTypeWithKnownBaseType for HalfPlaneCuttingCenteredUnitSquare<P> {
    type BaseOfNewType = DirectedLineCuttingCenteredUnitSquare<P>;
}

impl<P: Reqs> SemanticNewtype for HalfPlaneCuttingCenteredUnitSquare<P> {}
impl<P: Reqs> TryFrom<HalfPlane<P>> for HalfPlaneCuttingCenteredUnitSquare<P> {
    type Error = ();

    fn try_from(value: HalfPlane<P>) -> Result<Self, Self::Error> {
        Self::from_border_with_inside_on_right(value.try_into()?)
    }
}

impl<P: Reqs> From<HalfPlaneCuttingCenteredUnitSquare<P>> for HalfPlane<P> {
    fn from(value: HalfPlaneCuttingCenteredUnitSquare<P>) -> Self {
        todo!()
    }
}

impl<P: Reqs> Refinement<HalfPlane<P>> for HalfPlaneCuttingCenteredUnitSquare<P> {
    fn valid_refinement(base: HalfPlane<P>) -> bool {
        todo!()
    }
}

pub trait OpsForHalfPlaneCuttingCenteredUnitSqare<P: Reqs> {}

impl<P: Reqs> OpsForHalfPlaneCuttingCenteredUnitSqare<P> for HalfPlaneCuttingCenteredUnitSquare<P> {}
