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

impl_complement_for_refinement!(HalfPlaneCuttingCenteredUnitSquare<P: PointReqs>, refinement_base= HalfPlane<P>);
impl_quarter_turn_rotatable_for_newtype!(HalfPlaneCuttingCenteredUnitSquare<P: PointReqs>);

impl_half_plane_ops_for_newtype!(HalfPlaneCuttingCenteredUnitSquare<P: PointReqs>, base= DirectedLineCuttingCenteredUnitSquare<P>);

impl_constructors_for_half_plane_for_refinement!(HalfPlaneCuttingCenteredUnitSquare<P: PointReqs>, border= DirectedLineCuttingCenteredUnitSquare<P>, base=HalfPlane<P>);

impl<P: PointReqs> TryFrom<HalfPlane<P>> for HalfPlaneCuttingCenteredUnitSquare<P> {
    type Error = String;

    fn try_from(value: HalfPlane<P>) -> Result<Self, Self::Error> {
        let unrefined_border = value.border_line();
        let refined_border: DirectedLineCuttingCenteredUnitSquare<P> =
            unrefined_border.try_into()?;
        Ok(Self::from_border_with_inside_on_right(refined_border))
    }
}

impl<P: PointReqs> Into<HalfPlane<P>> for HalfPlaneCuttingCenteredUnitSquare<P> {
    fn into(self) -> HalfPlane<P> {
        HalfPlane::<P>::from_border_with_inside_on_right(self.border_line().into())
    }
}

impl<P: PointReqs> Refinement<HalfPlane<P>> for HalfPlaneCuttingCenteredUnitSquare<P> {
    fn valid(&self) -> bool {
        self.0.valid()
    }
}

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
