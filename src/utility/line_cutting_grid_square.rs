use crate::utility::*;

trait_alias_macro!(pub trait PointReqsForLineCuttingGridSquare = PointReqsForTwoPointsOnDifferentFaces);
trait_alias_macro!(trait PointReqs = PointReqsForLineCuttingGridSquare);

pub type LineCuttingGridSquare<P: PointReqs> =
    ThingRelToSquare<LineCuttingCenteredUnitSquare<P>, OnGrid<P>>;

impl_abstraction_for_wrapped_thing!(LineCuttingCenteredUnitSquare<P: PointReqs>, abstraction_base= LineCuttingGridSquare<P>);

// TODO: macro for impl_parallel_abstraction_connected_by_wrapper
impl<P: PointReqs> Abstraction<DirectedLineCuttingGridSquare<P>> for LineCuttingGridSquare<P> {}
impl<P: PointReqs> From<DirectedLineCuttingGridSquare<P>> for LineCuttingGridSquare<P> {
    fn from(value: DirectedLineCuttingGridSquare<P>) -> Self {
        Self::new((*value.thing()).into(), value.square())
    }
}

impl_abstraction_skip_level!(LineCuttingGridSquare<P: PointReqs> --> DirectedLineCuttingGridSquare<P> --> TwoPointsOnDifferentFacesOfGridSquare<P>);

impl<P: PointReqs> ConstructorsForTwoPointsOnDifferentFacesOfGridSquare<P>
    for LineCuttingGridSquare<P>
{
    fn try_new_from_line_and_square<L: DirectedFloatLineOps<P>>(
        line: L,
        square: <P>::OnGrid,
    ) -> Result<Self, String> {
        Ok(
            TwoPointsOnDifferentFacesOfGridSquare::try_new_from_line_and_square(line, square)?
                .into(),
        )
    }
}

pub trait ConstructorsForLineCuttingGridSquare<P: PointReqs>:
    ConstructorsForTwoPointsOnDifferentFacesOfGridSquare<P>
{
}
impl<P: PointReqs> ConstructorsForLineCuttingGridSquare<P> for LineCuttingGridSquare<P> {}

pub trait OpsForLineCuttingGridSquare<P: PointReqs> {}

impl<P: PointReqs> OpsForLineCuttingGridSquare<P> for LineCuttingGridSquare<P> {}

impl_operations_for_line_for_delegate!(LineCuttingGridSquare<P: PointReqs>, accessor=thing());
impl_constructors_for_line_for_newtype!(LineCuttingGridSquare<P: PointReqs>, base= DirectedLineCuttingGridSquare<P>);

impl_constructors_for_directed_line_for_newtype!(LineCuttingGridSquare<P: PointReqs>, base= DirectedLineCuttingGridSquare<P>);

impl_constructors_for_two_different_points_for_newtype!(LineCuttingGridSquare<P: PointReqs>, base= DirectedLineCuttingGridSquare<P>);

// TODO: Switch to TryTranslate to avoid panics
impl_translate_for_newtype!(LineCuttingGridSquare<P: PointReqs>);
