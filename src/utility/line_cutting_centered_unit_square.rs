use crate::utility::*;

#[derive(PartialEq, Debug, Clone, Copy, Constructor)]
pub struct LineCuttingCenteredUnitSquare<P>(DirectedLineCuttingCenteredUnitSquare<P>)
where
    P: PointReqsForTwoPointsOnDifferentFaces;

impl<P: PointReqsForTwoPointsOnDifferentFaces> LineOps for LineCuttingCenteredUnitSquare<P> {
    type PointType = P;

    fn two_different_arbitrary_points_on_line(&self) -> [Self::PointType; 2] {
        self.0.two_different_arbitrary_points_on_line()
    }
}

// TODO: Switch to TryTranslate to avoid panics
impl_translate_for_newtype!(
    LineCuttingCenteredUnitSquare<T: PointReqsForTwoPointsOnDifferentFaces>
);
impl_quarter_turn_rotatable_for_newtype!(
    LineCuttingCenteredUnitSquare<T: PointReqsForTwoPointsOnDifferentFaces>
);
