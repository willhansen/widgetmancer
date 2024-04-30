use crate::utility::*;
use derive_more::Constructor;

trait_alias_macro!(pub trait PointReqsForDirectedLineCuttingGridSquare = PointReqsForTwoPointsOnDifferentFaces);
trait_alias_macro!(trait PointReqs =PointReqsForDirectedLineCuttingGridSquare);

#[derive(Debug, PartialEq, Clone, Copy, Constructor)]
pub struct DirectedLineCuttingGridSquare<P: PointReqs>(TwoPointsOnDifferentFacesOfGridSquare<P>);

impl_abstraction_for_newtype!(DirectedLineCuttingGridSquare<P: PointReqs>(TwoPointsOnDifferentFacesOfGridSquare<P>));

impl_quarter_turn_rotatable_for_newtype!(
    DirectedLineCuttingGridSquare<T: PointReqs>
);
impl_reversible_for_newtype!(
    DirectedLineCuttingGridSquare<T: PointReqs>
);

// TODO Switch to TryAdd and avoid panic?
// TODO: define with a macro?
impl<P: PointReqs> Add<P> for DirectedLineCuttingGridSquare<P> {
    type Output = Self;

    fn add(self, rhs: P) -> Self::Output {
        Self::new(self.0.add(rhs))
    }
}
// TODO Switch to Try version and avoid panic?
impl<P: PointReqs> Sub<P> for DirectedLineCuttingGridSquare<P> {
    type Output = Self;

    fn sub(self, rhs: P) -> Self::Output {
        Self::new(self.0.sub(rhs))
    }
}

impl<P: PointReqs> LineOps for DirectedLineCuttingGridSquare<P> {
    type PointType = P;

    fn two_different_arbitrary_points_on_line(&self) -> [Self::PointType; 2] {
        todo!()
    }
}

impl<P: PointReqs> DirectedLineOps for DirectedLineCuttingGridSquare<P> {
    fn two_points_on_line_in_order(&self) -> [Self::PointType; 2] {
        self.0.to_array()
    }
}
