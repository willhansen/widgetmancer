use crate::utility::*;

pub struct DirectedLineCuttingLocalSquare<P>(TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>)
where
    P: PointReqsForTwoPointsOnDifferentFaces;

impl<P: PointReqsForTwoPointsOnDifferentFaces> LineOps for DirectedLineCuttingLocalSquare<P> {
    type PointType = P;

    fn two_different_arbitrary_points_on_line(&self) -> [Self::PointType; 2] {
        todo!()
    }
}

impl<P: PointReqsForTwoPointsOnDifferentFaces> DirectedLineOps
    for DirectedLineCuttingLocalSquare<P>
{
    fn two_points_on_line_in_order(&self) -> [Self::PointType; 2] {
        self.0.to_array()
    }
}
