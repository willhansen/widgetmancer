use crate::utility::*;
use derive_more::Constructor;

trait_alias!(pub trait PointReqs = two_points_on_different_faces_of_centered_unit_square::PointReqs);

pub type Shape<P: PointReqs> =
    ThingRelToSquare<DirectedLineCuttingCenteredUnitSquare<P>, OnGrid<P>>;

// pub trait OperationsForDirectedLineCuttingGridSquare<P: PointReqs> {
// }

// not an abstraction of two points on g
// impl_abstraction_via_newtype!(Shape<P: PointReqs>, base=TwoPointsOnDifferentFacesOfGridSquare<P>);
// TODO: maybe instead blanket implement through the wrapper type
// impl<P: PointReqs> AbstractionOf<TwoPointsOnDifferentFacesOfGridSquare<P>> for Shape<P> { }

// TODO Switch to TryTranslate and avoid panic?
translate::impl_via_newtype!(Shape<P: PointReqs>);

line::impl_operations_via_delegate!(Shape<P: PointReqs>, accessor=|x| x.thing());


directed_line::impl_operations_via_delegate!(Shape<P: PointReqs>, accessor=|x| x.thing());
// TODO: refactor to pass shape module instead of exact struct
directed_line::impl_constructors_via_base!(Shape<P: PointReqs>, base=TwoPointsOnDifferentFacesOfGridSquare<P>);
two_different_points::impl_constructors_via_abstraction!(Shape<P: PointReqs>, base=TwoPointsOnDifferentFacesOfGridSquare<P>);

impl<P: PointReqs> Into<DirectedLine<P>> for Shape<P> {
    fn into(self) -> DirectedLine<P> {
        DirectedLine::<P>::new_from_directed_line(self)
    }
}
