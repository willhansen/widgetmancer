
pub trait UnsignedCoordinate: coordinate::Operations {}
impl<T> UnsignedCoordinate for T
where
    T: coordinate::Operations,
    T::DataType: num::Unsigned,
{
}
