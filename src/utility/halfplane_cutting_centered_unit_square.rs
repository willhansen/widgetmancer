use crate::utility::*;

trait_alias_macro!(pub trait PointReqsForHalfPlaneCuttingCenteredUnitSquare = PointReqsForDirectedLineCuttingCenteredUnitSquare);

pub struct HalfPlaneCuttingCenteredUnitSquare<P>(DirectedLineCuttingCenteredUnitSquare<P>)
where
    P: PointReqsForHalfPlaneCuttingCenteredUnitSquare;

impl<P> SemanticNewtype<DirectedLineCuttingCenteredUnitSquare>
    for HalfPlaneCuttingCenteredUnitSquare<P>
where
    P: PointReqsForHalfPlaneCuttingCenteredUnitSquare,
{
}

impl<P> Refinement<HalfPlane<P>> for PointReqsForHalfPlaneCuttingCenteredUnitSquare<P>
where
    P: PointReqsForHalfPlaneCuttingCenteredUnitSquare,
{
    fn valid_refinement(base: HalfPlane<P>) -> bool {
        todo!()
    }
}
