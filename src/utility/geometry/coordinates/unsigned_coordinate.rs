use crate::utility::*;

pub trait UnsignedCoordinate: coordinates::Operations {}
impl<T> UnsignedCoordinate for T
where
    T: coordinates::Operations,
    T::DataType: num::Unsigned,
{
}
