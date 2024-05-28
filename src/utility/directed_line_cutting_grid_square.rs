use crate::utility::*;
use derive_more::Constructor;

trait_alias_macro!(pub trait PointReqsForDirectedLineCuttingGridSquare = PointReqsForTwoPointsOnDifferentFaces);
trait_alias_macro!(trait PointReqs =PointReqsForDirectedLineCuttingGridSquare);

pub type DirectedLineCuttingGridSquare<P: PointReqs> =
    ThingRelToSquare<DirectedLineCuttingCenteredUnitSquare<P>, OnGrid<P>>;

// pub trait OperationsForDirectedLineCuttingGridSquare<P: PointReqs> {
// }

impl_abstraction_for_newtype!(DirectedLineCuttingGridSquare<P: PointReqs>, base=TwoPointsOnDifferentFacesOfGridSquare<P>);

impl_constructors_for_two_different_points_for_abstraction!(DirectedLineCuttingGridSquare<P: PointReqs>, base= TwoPointsOnDifferentFacesOfGridSquare<P>);

// TODO Switch to TryTranslate and avoid panic?
impl_translate_for_newtype!(DirectedLineCuttingGridSquare<P: PointReqs>);

impl_operations_for_line_for_delegate!(DirectedLineCuttingGridSquare<P: PointReqs>, accessor=thing());
impl<P: PointReqs> ConstructorsForLine<P> for DirectedLineCuttingGridSquare<P> {}

impl_operations_for_directed_line_for_delegate!(DirectedLineCuttingGridSquare<P: PointReqs>, accessor=thing());
impl_constructors_for_directed_line_for_newtype!(DirectedLineCuttingGridSquare<P: PointReqs>, base=TwoPointsOnDifferentFacesOfGridSquare<P>);

impl<P: PointReqs> Into<DirectedLine<P>> for DirectedLineCuttingGridSquare<P> {
    fn into(self) -> DirectedLine<P> {
        DirectedLine::<P>::new_from_directed_line(self)
    }
}
