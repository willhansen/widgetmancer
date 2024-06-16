use crate::utility::*;

trait_alias!(pub trait PointReqsForDirectedLineCuttingCenteredUnitSquare = PointReqsForDirectedLine + PointReqsForTwoPointsOnDifferentFacesOfCenteredUnitSquare);
trait_alias!(trait PointReqs =PointReqsForDirectedLineCuttingCenteredUnitSquare );

// TODO: create a RefinedDirectedLine trait?
#[derive(PartialEq, Debug, Clone, Copy, Constructor)]
pub struct DirectedLineCuttingCenteredUnitSquare<P>(
    TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>,
)
where
    P: PointReqs;

impl_abstraction_for_newtype!(DirectedLineCuttingCenteredUnitSquare<P: PointReqs>, base= TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>);

impl_quarter_turn_rotatable_for_newtype!(
    DirectedLineCuttingCenteredUnitSquare<P: PointReqs>);
impl_reversible_for_newtype!(
    DirectedLineCuttingCenteredUnitSquare<P: PointReqs>
);

// TODO: Switch to TryTranslate to avoid panics
impl_translate_for_newtype!(DirectedLineCuttingCenteredUnitSquare<P: PointReqs>);

impl_operations_for_directed_line_for_delegate!(DirectedLineCuttingCenteredUnitSquare<P: PointReqs>, accessor=0);

impl<P: PointReqs> TryFrom<DirectedLine<P>> for DirectedLineCuttingCenteredUnitSquare<P> {
    type Error = String;

    fn try_from(value: DirectedLine<P>) -> Result<Self, Self::Error> {
        todo!()
    }
}
impl<P: PointReqs> Into<DirectedLine<P>> for DirectedLineCuttingCenteredUnitSquare<P> {
    fn into(self) -> DirectedLine<P> {
        DirectedLine::<P>::new_from_directed_line(self)
    }
}

impl<P: PointReqs> RefinementOf<DirectedLine<P>> for DirectedLineCuttingCenteredUnitSquare<P> {
    fn valid(&self) -> bool {
        // Non-valid states non-representable
        true
    }
}
