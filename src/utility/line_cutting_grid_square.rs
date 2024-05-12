use crate::utility::*;

trait_alias_macro!(pub trait PointReqsForLineCuttingGridSquare = PointReqsForTwoPointsOnDifferentFaces);
trait_alias_macro!(trait PointReqs = PointReqsForLineCuttingGridSquare);

#[derive(PartialEq, Debug, Clone, Copy, Constructor)]
pub struct LineCuttingGridSquare<P: PointReqs>(DirectedLineCuttingGridSquare<P>);

impl_abstraction_for_newtype!(LineCuttingGridSquare<P: PointReqs>, base= DirectedLineCuttingGridSquare<P>);
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

impl_operations_for_line_for_newtype!(LineCuttingGridSquare<P: PointReqs>);
impl_constructors_for_line_for_newtype!(LineCuttingGridSquare<P: PointReqs>, base= DirectedLineCuttingGridSquare<P>);

impl_constructors_for_directed_line_for_newtype!(LineCuttingGridSquare<P: PointReqs>, base= DirectedLineCuttingGridSquare<P>);

impl_constructors_for_two_different_points_for_newtype!(LineCuttingGridSquare<P: PointReqs>, base= DirectedLineCuttingGridSquare<P>);

// TODO: Switch to TryTranslate to avoid panics
impl_translate_for_newtype!(LineCuttingGridSquare<P: PointReqs>);
impl_quarter_turn_rotatable_for_newtype!(
    LineCuttingGridSquare<T: PointReqs>
);
