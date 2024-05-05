use crate::utility::*;

trait_alias_macro!(pub trait PointReqsForLineCuttingCenteredUnitSquare = PointReqsForDirectedLineCuttingCenteredUnitSquare);
trait_alias_macro!(trait PointReqs = PointReqsForLineCuttingCenteredUnitSquare);

#[derive(PartialEq, Debug, Clone, Copy, Constructor)]
pub struct LineCuttingCenteredUnitSquare<P: PointReqs>(DirectedLineCuttingCenteredUnitSquare<P>);

impl<P: PointReqs> LineOps<P> for LineCuttingCenteredUnitSquare<P> {

    fn two_different_arbitrary_points_on_line(&self) -> [P; 2] {
        self.0.two_different_arbitrary_points_on_line()
    }
}

// TODO: Switch to TryTranslate to avoid panics
impl_translate_for_newtype!(
    LineCuttingCenteredUnitSquare<P: PointReqs>
);
impl_quarter_turn_rotatable_for_newtype!(
    LineCuttingCenteredUnitSquare<P: PointReqs>
);
