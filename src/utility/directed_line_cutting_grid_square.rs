use crate::utility::*;
use derive_more::Constructor;

trait_alias_macro!(pub trait PointReqsForDirectedLineCuttingGridSquare = PointReqsForTwoPointsOnDifferentFaces);

#[derive(Debug, PartialEq, Clone, Copy, Constructor)]
pub struct DirectedLineCuttingGridSquare<P>(TwoPointsOnDifferentFacesOfGridSquare<P>)
where
    P: PointReqsForDirectedLineCuttingGridSquare;

impl_quarter_turn_rotatable_for_newtype!(
    DirectedLineCuttingGridSquare<T: PointReqsForDirectedLineCuttingGridSquare>
);
impl_reversible_for_newtype!(
    DirectedLineCuttingGridSquare<T: PointReqsForDirectedLineCuttingGridSquare>
);

// TODO Switch to TryAdd and avoid panic?
// TODO: define with a macro?
impl<P: PointReqsForDirectedLineCuttingGridSquare> Add<P> for DirectedLineCuttingGridSquare<P> {
    type Output = Self;

    fn add(self, rhs: P) -> Self::Output {
        Self::new(self.0.add(rhs))
    }
}
// TODO Switch to Try version and avoid panic?
impl<P: PointReqsForDirectedLineCuttingGridSquare> Sub<P> for DirectedLineCuttingGridSquare<P> {
    type Output = Self;

    fn sub(self, rhs: P) -> Self::Output {
        Self::new(self.0.sub(rhs))
    }
}

impl<P: PointReqsForDirectedLineCuttingGridSquare> LineOps for DirectedLineCuttingGridSquare<P> {
    type PointType = P;

    fn two_different_arbitrary_points_on_line(&self) -> [Self::PointType; 2] {
        todo!()
    }
}

impl<P: PointReqsForDirectedLineCuttingGridSquare> DirectedLineOps
    for DirectedLineCuttingGridSquare<P>
{
    fn two_points_on_line_in_order(&self) -> [Self::PointType; 2] {
        self.0.to_array()
    }
}
