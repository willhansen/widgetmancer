use crate::utility::*;

trait_alias_macro!(pub trait PointReqsForHalfPlaneCuttingCenteredUnitSquare = PointReqsForDirectedLineCuttingCenteredUnitSquare);
trait_alias_macro!(trait PointReqs =  PointReqsForHalfPlaneCuttingCenteredUnitSquare);

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct HalfPlaneCuttingCenteredUnitSquare<P: PointReqs>(
    DirectedLineCuttingCenteredUnitSquare<P>,
);

impl<P: PointReqs> HalfPlaneCuttingCenteredUnitSquare<P> {
    fn new(val: DirectedLineCuttingCenteredUnitSquare<P>) -> Self {
        Self(val)
    }
}

impl_parallel_refinement_for_newtype!(HalfPlaneCuttingCenteredUnitSquare<P: PointReqs>, newtype_base= DirectedLineCuttingCenteredUnitSquare<P>, refinement_base= HalfPlane<P>, diagonal_base= DirectedLine<P>);


impl_complement_for_refinement!(HalfPlaneCuttingCenteredUnitSquare<P: PointReqs>);
impl_quarter_turn_rotatable_for_newtype!(HalfPlaneCuttingCenteredUnitSquare<P: PointReqs>);

impl_half_plane_ops_for_newtype!(HalfPlaneCuttingCenteredUnitSquare<P: PointReqs>, base= DirectedLineCuttingCenteredUnitSquare<P>);

impl_constructors_for_half_plane_for_refinement!(HalfPlaneCuttingCenteredUnitSquare<P: PointReqs>, border= DirectedLineCuttingCenteredUnitSquare<P>, base=HalfPlane<P>);


// TODO: This feels like It only exists to explicitly require the base type's constructors.
pub trait ConstructorsForHalfPlaneCuttingCenteredUnitSquare<P: PointReqs>:
    ConstructorsForHalfPlane<P>
{
}

impl<P: PointReqs> ConstructorsForHalfPlaneCuttingCenteredUnitSquare<P>
    for HalfPlaneCuttingCenteredUnitSquare<P>
{
}

pub trait OpsForHalfPlaneCuttingCenteredUnitSqare<P: PointReqs>: HalfPlaneOps<P> {}

impl<P: PointReqs> OpsForHalfPlaneCuttingCenteredUnitSqare<P>
    for HalfPlaneCuttingCenteredUnitSquare<P>
{
}
