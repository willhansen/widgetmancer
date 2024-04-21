use crate::utility::*;

pub trait Translatable<MoveType>:
    Add<MoveType, Output = Self> + Sub<MoveType, Output = Self>
{
}
impl<T, MoveType> Translatable<MoveType> for T where
    T: Add<MoveType, Output = Self> + Sub<MoveType, Output = Self>
{
}
