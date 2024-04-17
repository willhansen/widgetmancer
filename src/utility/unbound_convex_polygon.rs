use crate::utility::*;

/// A polygon without the requirement that the shape be closed
// TODO: enforce order and non-redundancy on creation
#[derive(Debug, PartialEq, Clone)]
pub struct UnboundConvexPolygon<LineType>(Vec<HalfPlane<LineType>>);

impl<L: DirectedFloatLineOps> QuarterTurnRotatable for UnboundConvexPolygon<L> {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        Self(self.sides().quarter_rotated_ccw(quarter_turns_ccw))
    }
}

impl<L> UnboundConvexPolygon<L> {
    pub fn new(sides: Vec<HalfPlane<L>>) -> Self {
        Self(sides)
    }
    pub fn sides(&self) -> &Vec<HalfPlane<L>> {
        &self.0
    }
}

pub trait UnboundPolygonOps {
    type LineType: LineOps;
}

impl<L: LineOps> UnboundPolygonOps for UnboundConvexPolygon<L> {
    type LineType = L;
}
