use crate::utility::*;

trait_alias_macro!(pub trait PointReqsForUnboundConvexPolygon = PointReqsForHalfPlaneCuttingCenteredUnitSquare);
trait_alias_macro!(trait PointReqs = PointReqsForUnboundConvexPolygon);

/// A polygon without the requirement that the shape be closed
// TODO: enforce side order and non-redundancy on creation
//      - sides should be going ccw 
//      - some sides may actually not affect the resulting shape
#[derive(Debug, PartialEq, Clone)]
pub struct UnboundConvexPolygonCuttingCenteredUnitSquare<P: PointReqs>(Vec<HalfPlaneCuttingCenteredUnitSquare<P>>);

impl<P: PointReqs> UnboundConvexPolygonCuttingCenteredUnitSquare<P> {
    pub fn new(sides: Vec<P>) -> Self {
        Self(sides)
    }
    pub fn sides(&self) -> &Vec<P> {
        &self.0
    }
}

impl_quarter_turn_rotatable_for_newtype!(UnboundConvexPolygonCuttingCenteredUnitSquare<P: PointReqs>);

// TODO: define with macro
impl<P: PointReqs> Complement for UnboundConvexPolygonCuttingCenteredUnitSquare<P> {
    type Output = Self;

    fn complement(&self) -> Self::Output {
        Self::new(self.sides().iter().map(|x| x.complement()).collect())
    }
}

pub trait OpsForUnboundConvexPolygon<P: PointReqs> {
}

impl<P: PointReqs> OpsForUnboundConvexPolygon<P> for UnboundConvexPolygonCuttingCenteredUnitSquare<P> {
}
