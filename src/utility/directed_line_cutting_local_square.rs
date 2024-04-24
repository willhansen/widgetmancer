use crate::utility::*;

#[derive(Debug, Clone, Copy)]
pub struct DirectedLineCuttingLocalSquare<P>(TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>)
where
    P: PointReqsForTwoPointsOnDifferentFaces;

impl<P: PointReqsForTwoPointsOnDifferentFaces> DirectedLineCuttingLocalSquare<P> {
    pub fn new(x: TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>) -> Self {
        Self(x)
    }
}

impl_quarter_turn_rotatable_for_newtype!(
    DirectedLineCuttingLocalSquare<T: PointReqsForTwoPointsOnDifferentFaces>
);

// TODO Switch to TryAdd and avoid panic?
// TODO: define with a macro?
impl<P: PointReqsForTwoPointsOnDifferentFaces> Add<P> for DirectedLineCuttingLocalSquare<P> {
    type Output = Self;

    fn add(self, rhs: P) -> Self::Output {
        Self::new(self.0.add(rhs))
    }
}
// TODO Switch to Try version and avoid panic?
impl<P: PointReqsForTwoPointsOnDifferentFaces> Sub<P> for DirectedLineCuttingLocalSquare<P> {
    type Output = Self;

    fn sub(self, rhs: P) -> Self::Output {
        Self::new(self.0.sub(rhs))
    }
}

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
