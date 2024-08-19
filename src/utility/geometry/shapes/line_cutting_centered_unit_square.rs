use crate::utility::*;

trait_alias!(pub trait PointReqs = directed_line_cutting_centered_unit_square::PointReqs);

#[derive(PartialEq, Debug, Clone, Copy, Constructor)]
pub struct Shape<P: PointReqs>(directed_line_cutting_centered_unit_square::Shape<P>);

impl_abstraction_via_newtype!(LineCuttingCenteredUnitSquare<P: PointReqs>, base= DirectedLineCuttingCenteredUnitSquare<P>);

// TODO: this multi-step chaining should not have to be done manually
impl_abstraction_skip_level!(Shape<P: PointReqs> --> DirectedLineCuttingCenteredUnitSquare<P> --> TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>);
impl_abstraction_skip_level!(Shape<P: PointReqs> -->  TwoPointsOnDifferentFacesOfCenteredUnitSquare<P> --> TwoDifferentPoints<P> );

impl_skip_level_try_from!(LineCuttingCenteredUnitSquare<P: PointReqs> --> DirectedLineCuttingCenteredUnitSquare<P> --> DirectedLine<P>);

// TODO: Switch to TryTranslate to avoid panics
translate::impl_via_newtype!(
    LineCuttingCenteredUnitSquare<P: PointReqs>
);
impl_quarter_turn_rotatable_via_newtype!(
    LineCuttingCenteredUnitSquare<P: PointReqs>);

// TODO: is the trait bound on the lambda necessary?
line::impl_operations_via_delegate!(LineCuttingCenteredUnitSquare<P: PointReqs>, accessor=|x: Shape<_>| x.0);
line::impl_constructors_via_base!(LineCuttingCenteredUnitSquare<P: PointReqs>, base= DirectedLineCuttingCenteredUnitSquare<P>);

directed_line::impl_constructors_via_base!(LineCuttingCenteredUnitSquare<P: PointReqs>, base= DirectedLineCuttingCenteredUnitSquare<P>);

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

impl<P: PointReqs> directed_line_cutting_centered_unit_square::Constructors<P> for Shape<P> {}
impl<P: PointReqs> Constructors<P> for Shape<P> {}

two_different_points::impl_constructors_via_base!(Shape<P: PointReqs>, base= two_points_on_different_faces_of_centered_unit_square::Shape<P>);
two_points_on_different_faces_of_centered_unit_square::impl_constructors_via_base!(Shape<P: PointReqs>, base= TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>);
