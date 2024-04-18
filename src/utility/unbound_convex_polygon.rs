use crate::utility::*;

/// A polygon without the requirement that the shape be closed
// TODO: enforce order and non-redundancy on creation
#[derive(Debug, PartialEq, Clone)]
pub struct UnboundConvexPolygon<HalfPlaneType>(Vec<HalfPlaneType>);

impl<H: HalfPlaneOps> UnboundConvexPolygon<H> {
    pub fn new(sides: Vec<H>) -> Self {
        Self(sides)
    }
    pub fn sides(&self) -> &Vec<H> {
        &self.0
    }
}
impl<Hp: HalfPlaneOps> QuarterTurnRotatable for UnboundConvexPolygon<Hp> {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        Self::new(self.sides().quarter_rotated_ccw(quarter_turns_ccw))
    }
}

// TODO: some sort of `impl_for_mappable` macro
impl<H: HalfPlaneOps> Complement for UnboundConvexPolygon<H> {
    type Output = Self;

    fn complement(&self) -> Self::Output {
        Self::new(self.sides().iter().map(|x| x.complement()).collect())
    }
}

pub trait UnboundConvexPolygonOps {
    type LineType: LineOps;
}

impl<L: LineOps> UnboundConvexPolygonOps for UnboundConvexPolygon<L> {
    type LineType = L;
}
