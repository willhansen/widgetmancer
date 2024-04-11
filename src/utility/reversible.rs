use crate::utility::*;

/// Reversing twice should always return the original
pub trait Reversible {
    fn reversed(&self) -> Self;
}
