use crate::utility::*;

// TODO: create a RefinedDirectedLine trait?
#[derive(PartialEq, Debug, Clone, Copy, Constructor)]
pub struct DirectedLineCuttingCenteredUnitSquare<P>(
    TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>,
)
where
    P: PointReqsForTwoPointsOnDifferentFaces;

impl_quarter_turn_rotatable_for_newtype!(
    DirectedLineCuttingCenteredUnitSquare<T: PointReqsForTwoPointsOnDifferentFaces>
);
impl_reversible_for_newtype!(
    DirectedLineCuttingCenteredUnitSquare<T: PointReqsForTwoPointsOnDifferentFaces>
);

// TODO: Switch to TryTranslate to avoid panics
impl_translate_for_newtype!(
    DirectedLineCuttingCenteredUnitSquare<T: PointReqsForTwoPointsOnDifferentFaces>
);

impl<P: PointReqsForTwoPointsOnDifferentFaces> LineOps
    for DirectedLineCuttingCenteredUnitSquare<P>
{
    type PointType = P;

    fn two_different_arbitrary_points_on_line(&self) -> [Self::PointType; 2] {
        todo!()
    }
}

impl<P: PointReqsForTwoPointsOnDifferentFaces> DirectedLineOps
    for DirectedLineCuttingCenteredUnitSquare<P>
{
    fn two_points_on_line_in_order(&self) -> [Self::PointType; 2] {
        self.0.to_array()
    }
}

impl<P: PointReqsForTwoPointsOnDifferentFaces> TryFrom<DirectedLine<P>>
    for DirectedLineCuttingCenteredUnitSquare<P>
{
    type Error = ();

    fn try_from(value: DirectedLine<P>) -> Result<Self, Self::Error> {
        todo!()
    }
}
