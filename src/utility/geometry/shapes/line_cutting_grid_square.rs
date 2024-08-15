use crate::utility::*;

trait_alias!(pub trait PointReqs = two_points_on_different_faces_of_centered_unit_square::PointReqs);

pub type Shape<P: PointReqs> = ThingRelToSquare<LineCuttingCenteredUnitSquare<P>, OnGrid<P>>;

impl_abstraction_for_wrapped_thing!(LineCuttingCenteredUnitSquare<P: PointReqs>, abstraction_base= Shape<P>, accessor=thing());

// TODO: macro for impl_parallel_abstraction_connected_by_wrapper
impl<P: PointReqs> AbstractionOf<DirectedLineCuttingGridSquare<P>> for Shape<P> {}
impl<P: PointReqs> From<DirectedLineCuttingGridSquare<P>> for Shape<P> {
    fn from(value: DirectedLineCuttingGridSquare<P>) -> Self {
        Self::new((*value.thing()).into(), value.square())
    }
}

impl_abstraction_skip_level!(Shape<P: PointReqs> --> DirectedLineCuttingGridSquare<P> --> TwoPointsOnDifferentFacesOfGridSquare<P>);

impl<P: PointReqs> two_points_on_different_faces_of_grid_square::Constructors<P> for Shape<P> {
    fn try_new_from_line_and_square<L: directed_float_line::Operations<P>>(
        line: L,
        square: <P>::OnGrid,
    ) -> Result<Self, String> {
        Ok(
            TwoPointsOnDifferentFacesOfGridSquare::try_new_from_line_and_square(line, square)?
                .into(),
        )
    }
}

pub trait Constructors<P: PointReqs>:
    two_points_on_different_faces_of_grid_square::Constructors<P>
{
}
impl<P: PointReqs> Constructors<P> for Shape<P> {}

pub trait Operations<P: PointReqs> {}

impl<P: PointReqs> Operations<P> for Shape<P> {}

impl_operations_for_line_for_delegate!(Shape<P: PointReqs>, accessor=thing());
impl_constructors_for_line_for_newtype!(Shape<P: PointReqs>, base= DirectedLineCuttingGridSquare<P>);

impl_constructors_for_directed_line_for_newtype!(Shape<P: PointReqs>, base= DirectedLineCuttingGridSquare<P>);

// TODO: this could be generally implemented for anything convertible from twodifferentpoints
impl<P: PointReqs> ConstructorsForTwoDifferentPoints<P> for Shape<P> {
    fn try_from_two_exact_points(p1: P, p2: P) -> Result<Self, String> {
        TwoDifferentPoints::try_from_two_exact_points(p1, p2).try_into()
    }
}

// TODO: Switch to TryTranslate to avoid panics
translate::impl_for_newtype!(Shape<P: PointReqs>);
