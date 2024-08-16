use crate::utility::*;

trait_alias!(pub trait PointReqs = directed_line::PointReqs + two_points_on_different_faces_of_centered_unit_square::PointReqs);

// TODO: create a RefinedDirectedLine trait?
#[derive(PartialEq, Debug, Clone, Copy, Constructor)]
pub struct Shape<P>(
    TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>,
)
where
    P: PointReqs;

impl_abstraction_via_newtype!(Shape<P: PointReqs>, base= TwoPointsOnDifferentFacesOfCenteredUnitSquare<P>);

impl_quarter_turn_rotatable_via_newtype!(
    Shape<P: PointReqs>);
impl_reversible_via_newtype!(
    Shape<P: PointReqs>
);

// TODO: Switch to TryTranslate to avoid panics
translate::impl_via_newtype!(Shape<P: PointReqs>);

// directed_line::impl_operations_via_delegate!(Shape<P: PointReqs>, accessor=0);

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
impl<P: PointReqs> two_different_points::Constructors<P> for Shape<P> {
    fn try_from_two_exact_points(p1: P, p2: P) -> Result<Self, String> {
        todo!()
    }
}

impl<P: PointReqs> two_points_on_different_faces_of_centered_unit_square::Constructors<P> for Shape<P> {}

impl<P: PointReqs> AbstractsTo<LineCuttingCenteredUnitSquare<P>> for Shape<P> {
    fn set_with_abstraction(&self, val: &LineCuttingCenteredUnitSquare<P>) -> Self {
        todo!()
    }
}

impl<P: PointReqs, S: thing_relative_to_square::PointReqs> AbstractsTo<ThingRelToSquare<LineCuttingCenteredUnitSquare<P>, S>> for Shape<P> {
    fn set_with_abstraction(&self, val: &ThingRelToSquare<LineCuttingCenteredUnitSquare<P>, S>) -> Self {
        todo!()
    }
}

