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


impl<P: PointReqs> TryFrom<HalfPlane<P>> for HalfPlaneCuttingCenteredUnitSquare<P> {
    type Error = ();

    fn try_from(value: HalfPlane<P>) -> Result<Self, Self::Error> {
        Ok(Self::from_border_with_inside_on_right(value.try_into()?))
    }
}

impl<P: PointReqs> From<HalfPlaneCuttingCenteredUnitSquare<P>> for HalfPlane<P> {
    fn from(value: HalfPlaneCuttingCenteredUnitSquare<P>) -> Self {
        todo!()
    }
}

impl<P: PointReqs> Refinement<HalfPlane<P>> for HalfPlaneCuttingCenteredUnitSquare<P> {
    type Base = HalfPlane<P>;
    fn valid_refinement(base: Self::Base) -> bool {
        todo!()
    }
}

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
