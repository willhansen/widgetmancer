use crate::utility::*;

trait_alias!(pub trait PointReqs = directed_line_cutting_centered_unit_square::PointReqs);

#[derive(PartialEq, Debug, Clone, Copy, Constructor)]
pub struct Shape<P: PointReqs>(directed_line_cutting_centered_unit_square::Shape<P>);

impl_abstraction_for_newtype!(LineCuttingCenteredUnitSquare<P: PointReqs>, base= DirectedLineCuttingCenteredUnitSquare<P>);
impl_abstraction_skip_level!(Shape<P: PointReqs> --> DirectedLineCuttingCenteredUnitSquare<P> --> TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>);

// Is the arrow direction confusing?
impl_skip_level_try_from!(LineCuttingCenteredUnitSquare<P: PointReqs> --> DirectedLineCuttingCenteredUnitSquare<P> --> DirectedLine<P>);

// TODO: Switch to TryTranslate to avoid panics
translate::impl_for_newtype!(
    LineCuttingCenteredUnitSquare<P: PointReqs>
);
impl_quarter_turn_rotatable_for_newtype!(
    LineCuttingCenteredUnitSquare<P: PointReqs>);

line::impl_operations_via_delegate!(LineCuttingCenteredUnitSquare<P: PointReqs>, accessor=0);
line::impl_constructors_via_base!(LineCuttingCenteredUnitSquare<P: PointReqs>, base= DirectedLineCuttingCenteredUnitSquare<P>);

directed_line::impl_constructors_for_newtype!(LineCuttingCenteredUnitSquare<P: PointReqs>, base= DirectedLineCuttingCenteredUnitSquare<P>);

impl<P: PointReqs> Into<Line<P>> for LineCuttingCenteredUnitSquare<P> {
    fn into(self) -> Line<P> {
        Line::<P>::from_line(self)
    }
}
impl<P: PointReqs> TryFrom<Line<P>> for LineCuttingCenteredUnitSquare<P> {
    type Error = String;

    fn try_from(value: Line<P>) -> Result<Self, Self::Error> {
        Ok(TwoPointsOnDifferentFacesOfCenteredUnitSquare::try_from_line(value)?.into())
    }
}

impl<P: PointReqs> RefinementOf<Line<P>> for LineCuttingCenteredUnitSquare<P> {
    fn valid(&self) -> bool {
        // Non-valid states non-representable
        true
    }
}

pub trait Constructors<P: PointReqs>: directed_line_cutting_centered_unit_square::Constructors<P> {

}

impl<P: PointReqs> Constructors<P> for Shape<P> {}
