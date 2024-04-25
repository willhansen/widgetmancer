use crate::utility::*;

#[derive(PartialEq, Debug, Clone, Copy, Constructor)]
pub struct LineCuttingGridSquare<P>(DirectedLineCuttingGridSquare<P>)
where
    P: PointReqsForTwoPointsOnDifferentFaces;

impl<P: PointReqsForTwoPointsOnDifferentFaces> LineOps for LineCuttingGridSquare<P> {
    type PointType = P;

    fn two_different_arbitrary_points_on_line(&self) -> [Self::PointType; 2] {
        self.0.two_different_arbitrary_points_on_line()
    }
}

// TODO: Switch to TryTranslate to avoid panics
impl_translate_for_newtype!(LineCuttingGridSquare<T: PointReqsForTwoPointsOnDifferentFaces>);
