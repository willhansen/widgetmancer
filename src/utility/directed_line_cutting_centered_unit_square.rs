use crate::utility::*;

trait_alias_macro!(pub trait PointReqsForDirectedLineCuttingCenteredUnitSquare = PointReqsForDirectedLine + PointReqsForTwoPointsOnDifferentFaces);
trait_alias_macro!(trait PointReqs =PointReqsForDirectedLineCuttingCenteredUnitSquare );

// TODO: create a RefinedDirectedLine trait?
#[derive(PartialEq, Debug, Clone, Copy, Constructor)]
pub struct DirectedLineCuttingCenteredUnitSquare<P>(
    TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>,
)
where
    P: PointReqs;

impl_abstraction_for_newtype!(DirectedLineCuttingCenteredUnitSquare<P: PointReqs>, base= TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>);

impl_quarter_turn_rotatable_for_newtype!(
    DirectedLineCuttingCenteredUnitSquare<P: PointReqs>
);
impl_reversible_for_newtype!(
    DirectedLineCuttingCenteredUnitSquare<P: PointReqs>
);


// TODO: Switch to TryTranslate to avoid panics
impl_translate_for_newtype!(DirectedLineCuttingCenteredUnitSquare<P: PointReqs>);

impl_operations_for_line_for_newtype!(DirectedLineCuttingCenteredUnitSquare<P: PointReqs>);
impl_constructors_for_line_for_newtype!(DirectedLineCuttingCenteredUnitSquare<P: PointReqs>, base= TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>);

impl_operations_for_directed_line_for_newtype!(DirectedLineCuttingCenteredUnitSquare<P: PointReqs>);




impl<P: PointReqs> TryFrom<DirectedLine<P>>
    for DirectedLineCuttingCenteredUnitSquare<P>
{
    type Error = ();

    fn try_from(value: DirectedLine<P>) -> Result<Self, Self::Error> {
        todo!()
    }
}

impl_constructors_for_directed_line_for_newtype!(DirectedLineCuttingCenteredUnitSquare<P: PointReqs>, base= TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>);
impl_constructors_for_two_different_points_for_abstraction!(DirectedLineCuttingCenteredUnitSquare<P: PointReqs>, base= TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>);

