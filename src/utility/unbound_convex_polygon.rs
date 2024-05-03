use crate::utility::*;

trait_alias_macro!(pub trait PointReqsForUnboundConvexPolygon = PointReqsForHalfPlane);
trait_alias_macro!(trait PointReqs = PointReqsForUnboundConvexPolygon);

trait HalfPlaneReqs<P: PointReqs>: HalfPlaneOps<P> {}
impl<P: PointReqs, T> HalfPlaneReqs<P> for T where T: HalfPlaneOps<P> {}

/// A polygon without the requirement that the shape be closed
// TODO: enforce order and non-redundancy on creation
#[derive(Debug, PartialEq, Clone)]
pub struct UnboundConvexPolygon<P, H>(Vec<H>);

impl<P: PointReqs, H: HalfPlaneReqs<P>> UnboundConvexPolygon<P, H> {
    pub fn new(sides: Vec<H>) -> Self {
        Self(sides)
    }
    pub fn sides(&self) -> &Vec<H> {
        &self.0
    }
}
impl<P: PointReqs, H: HalfPlaneReqs<P>> QuarterTurnRotatable for UnboundConvexPolygon<P, H> {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        Self::new(self.sides().quarter_rotated_ccw(quarter_turns_ccw))
    }
}

// TODO: define with macro
impl<P: PointReqs, H: HalfPlaneReqs<P>> Complement for UnboundConvexPolygon<P, H> {
    type Output = Self;

    fn complement(&self) -> Self::Output {
        Self::new(self.sides().iter().map(|x| x.complement()).collect())
    }
}

pub trait OpsForUnboundConvexPolygon {}

impl<P: PointReqs, H: HalfPlaneReqs<P>> OpsForUnboundConvexPolygon for UnboundConvexPolygon<P, H> {}
