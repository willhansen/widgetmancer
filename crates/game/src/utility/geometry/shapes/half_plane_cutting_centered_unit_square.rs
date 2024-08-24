use crate::utility::*;

trait_alias!(pub trait PointReqs = directed_line_cutting_centered_unit_square::PointReqs);

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct Shape<P: PointReqs>(DirectedLineCuttingCenteredUnitSquare<P>);

impl<P: PointReqs> Shape<P> {
    fn new(val: DirectedLineCuttingCenteredUnitSquare<P>) -> Self {
        Self(val)
    }
}

impl_complement_via_base!(Shape<P: PointReqs>, base= HalfPlane<P>);
impl_quarter_turn_rotatable_via_newtype!(Shape<P: PointReqs>);

half_plane::impl_operations_via_newtype!(Shape<P: PointReqs>, base= DirectedLineCuttingCenteredUnitSquare<P>);

half_plane::impl_constructors_via_base!(Shape<P: PointReqs>, border= DirectedLineCuttingCenteredUnitSquare<P>, base=HalfPlane<P>);

impl<P: PointReqs> TryFrom<HalfPlane<P>> for Shape<P> {
    type Error = String;

    fn try_from(value: HalfPlane<P>) -> Result<Self, Self::Error> {
        let unrefined_border = value.border_line();
        let refined_border: DirectedLineCuttingCenteredUnitSquare<P> =
            unrefined_border.try_into()?;
        Ok(Self::from_border_with_inside_on_right(refined_border))
    }
}

impl<P: PointReqs> Into<HalfPlane<P>> for Shape<P> {
    fn into(self) -> HalfPlane<P> {
        HalfPlane::<P>::from_border_with_inside_on_right(self.border_line().into())
    }
}

impl<P: PointReqs> RefinementOf<HalfPlane<P>> for Shape<P> {
    fn valid(&self) -> bool {
        self.0.valid()
    }
}

// TODO: This feels like It only exists to explicitly require the base type's constructors.
pub trait Constructors<P: PointReqs>: half_plane::Constructors<P> {}

impl<P: PointReqs> Constructors<P> for Shape<P> {}
impl<P: PointReqs> half_plane::Constructors<P> for Shape<P> {
    type BorderType = DirectedLine<P>;

    fn from_border_with_inside_on_right(line: Self::BorderType) -> Self {
        todo!()
    }
}

pub trait Operations<P: PointReqs>: half_plane::Operations<P> {}

impl<P: PointReqs> Operations<P> for Shape<P> {}
