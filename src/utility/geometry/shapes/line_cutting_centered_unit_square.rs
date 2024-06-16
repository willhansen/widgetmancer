use crate::utility::*;

trait_alias!(pub trait PointReqsForLineCuttingCenteredUnitSquare = PointReqsForDirectedLineCuttingCenteredUnitSquare);
trait_alias!(trait PointReqs = PointReqsForLineCuttingCenteredUnitSquare);

#[derive(PartialEq, Debug, Clone, Copy, Constructor)]
pub struct LineCuttingCenteredUnitSquare<P: PointReqs>(DirectedLineCuttingCenteredUnitSquare<P>);

impl_abstraction_for_newtype!(LineCuttingCenteredUnitSquare<P: PointReqs>, base= DirectedLineCuttingCenteredUnitSquare<P>);
impl_abstraction_skip_level!(LineCuttingCenteredUnitSquare<P: PointReqs> --> DirectedLineCuttingCenteredUnitSquare<P> --> TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>);

// Is the arrow direction confusing?
impl_skip_level_try_from!(LineCuttingCenteredUnitSquare<P: PointReqs> --> DirectedLineCuttingCenteredUnitSquare<P> --> DirectedLine<P>);

// TODO: Switch to TryTranslate to avoid panics
impl_translate_for_newtype!(
    LineCuttingCenteredUnitSquare<P: PointReqs>
);
impl_quarter_turn_rotatable_for_newtype!(
    LineCuttingCenteredUnitSquare<P: PointReqs>);

impl_operations_for_line_for_delegate!(LineCuttingCenteredUnitSquare<P: PointReqs>, accessor=0);
impl_constructors_for_line_for_newtype!(LineCuttingCenteredUnitSquare<P: PointReqs>, base= DirectedLineCuttingCenteredUnitSquare<P>);

impl_constructors_for_directed_line_for_newtype!(LineCuttingCenteredUnitSquare<P: PointReqs>, base= DirectedLineCuttingCenteredUnitSquare<P>);

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
