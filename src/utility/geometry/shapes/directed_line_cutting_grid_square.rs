use crate::utility::*;
use derive_more::Constructor;

trait_alias!(pub trait PointReqs = two_points_on_different_faces_of_centered_unit_square::PointReqs);

pub type Shape<P: PointReqs> =
    ThingRelToSquare<DirectedLineCuttingCenteredUnitSquare<P>, OnGrid<P>>;

// pub trait OperationsForDirectedLineCuttingGridSquare<P: PointReqs> {
// }

// not an abstraction of two points on g
// impl_abstraction_for_newtype!(Shape<P: PointReqs>, base=TwoPointsOnDifferentFacesOfGridSquare<P>);
// TODO: maybe instead blanket implement through the wrapper type
// impl<P: PointReqs> AbstractionOf<TwoPointsOnDifferentFacesOfGridSquare<P>> for Shape<P> { }

// TODO Switch to TryTranslate and avoid panic?
translate::impl_for_newtype!(Shape<P: PointReqs>);

impl_operations_for_line_for_delegate!(Shape<P: PointReqs>, accessor=thing());
impl<P: PointReqs> ConstructorsForLine<P> for Shape<P> {}

impl_operations_for_directed_line_for_delegate!(Shape<P: PointReqs>, accessor=thing());
impl_constructors_for_directed_line_for_newtype!(Shape<P: PointReqs>, base=TwoPointsOnDifferentFacesOfGridSquare<P>);
impl_constructors_for_two_different_points_for_abstraction!(Shape<P: PointReqs>, base=TwoPointsOnDifferentFacesOfGridSquare<P>);

impl<P: PointReqs> Into<DirectedLine<P>> for Shape<P> {
    fn into(self) -> DirectedLine<P> {
        DirectedLine::<P>::new_from_directed_line(self)
    }
}
