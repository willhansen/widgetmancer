use crate::utility::*;

trait_alias!(pub trait PointReqs = directed_line::PointReqs + two_points_on_different_faces_of_centered_unit_square::PointReqs);

// TODO: create a RefinedDirectedLine trait?
#[derive(PartialEq, Debug, Clone, Copy, Constructor)]
pub struct Shape<P>(
    TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>,
)
where
    P: PointReqs;

impl_abstraction_for_newtype!(Shape<P: PointReqs>, base= TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>);

impl_quarter_turn_rotatable_for_newtype!(
    Shape<P: PointReqs>);
impl_reversible_for_newtype!(
    Shape<P: PointReqs>
);

// TODO: Switch to TryTranslate to avoid panics
impl_translate_for_newtype!(Shape<P: PointReqs>);

// impl_operations_for_directed_line_for_delegate!(Shape<P: PointReqs>, accessor=0);

impl<P: PointReqs> TryFrom<DirectedLine<P>> for Shape<P> {
    type Error = String;

    fn try_from(value: DirectedLine<P>) -> Result<Self, Self::Error> {
        todo!()
    }
}
impl<P: PointReqs> Into<DirectedLine<P>> for Shape<P> {
    fn into(self) -> DirectedLine<P> {
        DirectedLine::<P>::new_from_directed_line(self)
    }
}

impl<P: PointReqs> RefinementOf<DirectedLine<P>> for Shape<P> {
    fn valid(&self) -> bool {
        // Non-valid states non-representable
        true
    }
}
pub trait Constructors<P: PointReqs>: two_points_on_different_faces_of_centered_unit_square::Constructors<P> {

}

impl<P: PointReqs> Constructors<P> for Shape<P> {}
