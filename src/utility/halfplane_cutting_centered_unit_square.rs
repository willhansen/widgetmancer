use crate::utility::*;

trait_alias_macro!(pub trait PointReqsForHalfPlaneCuttingCenteredUnitSquare = PointReqsForDirectedLineCuttingCenteredUnitSquare);
trait_alias_macro!(trait Reqs =  PointReqsForHalfPlaneCuttingCenteredUnitSquare);

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct HalfPlaneCuttingCenteredUnitSquare<P: Reqs>(DirectedLineCuttingCenteredUnitSquare<P>);

impl<P: Reqs> HalfPlaneCuttingCenteredUnitSquare<P> {
    fn new(val: DirectedLineCuttingCenteredUnitSquare<P>) -> Self {
        Self(val)
    }
}

impl<P: Reqs> NewTypeWithKnownBaseType for HalfPlaneCuttingCenteredUnitSquare<P> {
    type BaseOfNewType = DirectedLineCuttingCenteredUnitSquare<P>;
}

impl<P: Reqs> SemanticNewtype for HalfPlaneCuttingCenteredUnitSquare<P> {}
impl<P: Reqs> TryFrom<HalfPlane<P>> for HalfPlaneCuttingCenteredUnitSquare<P> {
    type Error = ();

    fn try_from(value: HalfPlane<P>) -> Result<Self, Self::Error> {
        Ok(Self::from_border_with_inside_on_right(value.try_into()?))
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

impl_quarter_turn_rotatable_for_newtype!(HalfPlaneCuttingCenteredUnitSquare<P: Reqs>);

impl<P: Reqs> HalfPlaneOps for HalfPlaneCuttingCenteredUnitSquare<P> {
    type PointType = P;
    type BorderType = DirectedLineCuttingCenteredUnitSquare<P>;

    fn border_line(&self) -> Self::BorderType {
        self.0
    }
}

impl<P: Reqs> HalfPlaneConstructors for HalfPlaneCuttingCenteredUnitSquare<P> {
    type PointType = P;

    type BorderType = DirectedLineCuttingCenteredUnitSquare<P>;

    fn from_border_with_inside_on_right(line: Self::BorderType) -> Self {
        todo!()
    }
}

// TODO: This feels like It only exists to explicitly require the base type's constructors.
pub trait ConstructorsForHalfPlaneCuttingCenteredUnitSquare<P: Reqs>:
    HalfPlaneConstructors
{
}

impl<P: Reqs> ConstructorsForHalfPlaneCuttingCenteredUnitSquare<P>
    for HalfPlaneCuttingCenteredUnitSquare<P>
{
}

pub trait OpsForHalfPlaneCuttingCenteredUnitSqare<P: Reqs>: HalfPlaneOps<PointType = P> {}

impl<P: Reqs> OpsForHalfPlaneCuttingCenteredUnitSqare<P> for HalfPlaneCuttingCenteredUnitSquare<P> {}
