use crate::utility::*;

trait_alias_macro!(pub trait PointReqsForLineCuttingCenteredUnitSquare = PointReqsForDirectedLineCuttingCenteredUnitSquare);
trait_alias_macro!(trait PointReqs = PointReqsForLineCuttingCenteredUnitSquare);

#[derive(PartialEq, Debug, Clone, Copy, Constructor)]
pub struct LineCuttingCenteredUnitSquare<P: PointReqs>(DirectedLineCuttingCenteredUnitSquare<P>);

impl_abstraction_for_newtype!(LineCuttingCenteredUnitSquare<P: PointReqs>, base= DirectedLineCuttingCenteredUnitSquare<P>);

// TODO: Switch to TryTranslate to avoid panics
impl_translate_for_newtype!(
    LineCuttingCenteredUnitSquare<P: PointReqs>
);
impl_quarter_turn_rotatable_for_newtype!(
    LineCuttingCenteredUnitSquare<P: PointReqs>
);

impl_operations_for_line_for_newtype!(LineCuttingCenteredUnitSquare<P: PointReqs>);
impl_constructors_for_line_for_newtype!(LineCuttingCenteredUnitSquare<P: PointReqs>, base= DirectedLineCuttingCenteredUnitSquare<P>);

impl_constructors_for_directed_line_for_newtype!(LineCuttingCenteredUnitSquare<P: PointReqs>, base= DirectedLineCuttingCenteredUnitSquare<P>);
impl_constructors_for_two_different_points_for_abstraction!(LineCuttingCenteredUnitSquare<P: PointReqs>, base= DirectedLineCuttingCenteredUnitSquare<P>);

