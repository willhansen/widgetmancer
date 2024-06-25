use crate::utility::*;

trait_alias!(pub trait PointReqs = half_plane_cutting_centered_unit_square::PointReqs);

/// A polygon without the requirement that the shape be closed
// TODO: enforce side order and non-redundancy on creation
//      - sides should be going ccw
//      - some sides may actually not affect the resulting shape
#[derive(Debug, PartialEq, Clone)]
pub struct Shape<P: PointReqs>(
    Vec<HalfPlaneCuttingCenteredUnitSquare<P>>,
);

impl<P: PointReqs> Shape<P> {
    pub fn new(sides: Vec<P>) -> Self {
        Self(sides)
    }
    pub fn sides(&self) -> &Vec<P> {
        &self.0
    }
}

impl_quarter_turn_rotatable_for_newtype!(Shape<P: PointReqs>);

// TODO: define with macro
impl<P: PointReqs> Complement for Shape<P> {
    type Output = Self;

    fn complement(&self) -> Self::Output {
        Self::new(self.sides().iter().map(|x| x.complement()).collect())
    }
}

pub trait Operations<P: PointReqs> {}

impl<P: PointReqs> Operations<P>
    for Shape<P>
{
}
