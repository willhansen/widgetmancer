use crate::utility::*;

pub trait Operations:
    coordinates::Operations<DataType = Self::_DataType>
    + Neg<Output = Self>
    // TODO: put on the SignedCoordinateConstructor trait instead
    + From<NormalizedOrthoAngle>
    + From<OrthogonalDirection>
    + From<(Self::_DataType, Self::_DataType)>
{
    type _DataType: num::Signed + Copy + PartialOrd + Debug;
    fn flip_x(&self) -> Self {
        Self::new(-self.x(), self.y())
    }
    fn flip_y(&self) -> Self {
        Self::new(self.x(), -self.y())
    }
    fn right() -> Self {
        Self::new(Self::DataType::one(), Self::DataType::zero())
    }
    fn left() -> Self {
        Self::new(-Self::DataType::one(), Self::DataType::zero())
    }
    fn up() -> Self {
        Self::new(Self::DataType::zero(), Self::DataType::one())
    }
    fn down() -> Self {
        Self::new(Self::DataType::zero(), -Self::DataType::one())
    }
    // TODO: allow non-orthogonal directions
    fn stepped(&self, dir: OrthogonalDirection) -> Self {
        self.moved(dir, Self::DataType::one())
    }
    fn moved(&self, dir: OrthogonalDirection, length: Self::DataType) -> Self {
        *self + dir.to_step::<Self>() * length
    }
    fn position_on_orthogonal_axis(&self, axis: impl Into<OrthogonalDirection>) -> Self::DataType {
        let axis_vector: Self = axis.into().to_step();
        self.dot(axis_vector)
    }
    fn orthogonal_angle(&self) -> Result<NormalizedOrthoAngle, String> {
        <NormalizedOrthoAngle as ortho_angle::Operations>::try_from_coordinate(*self)
    }
}

impl<T> Operations for T
where
    T: coordinates::Operations + Neg<Output = Self> + From<NormalizedOrthoAngle> + From<OrthogonalDirection>,
    T::DataType: num::Signed,
{
    type _DataType = T::DataType;
}

impl<V> QuarterTurnRotatable for V
where
    V: Operations,
{
    fn quarter_rotated_ccw(&self, angle: impl Into<NormalizedOrthoAngle>) -> Self {
        // if self.is_absolute() {
        //     return *self;
        // }
        let angle = angle.into();
        Self::new(
            self.x() * angle.cos() - self.y() * angle.sin(),
            self.x() * angle.sin() + self.y() * angle.cos(),
        )
    }
}
