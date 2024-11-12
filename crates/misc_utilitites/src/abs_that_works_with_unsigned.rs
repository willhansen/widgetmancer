use num::Signed;

// TODO: better name
pub trait AbsThatWorksWithUnsigned {
    fn abs_that_works_with_unsigned(&self) -> Self;
}

macro_rules! impl_abs_that_works_with_unsigned_for {
    (signed: $Type:ty) => {
        impl AbsThatWorksWithUnsigned for $Type {
            fn abs_that_works_with_unsigned(&self) -> Self {
                self.abs()
            }
        }
    };
    (unsigned: $Type:ty) => {
        impl AbsThatWorksWithUnsigned for $Type {
            fn abs_that_works_with_unsigned(&self) -> Self {
                *self
            }
        }
    }
}

impl_abs_that_works_with_unsigned_for!(signed: i32);
impl_abs_that_works_with_unsigned_for!(unsigned: u32);
impl_abs_that_works_with_unsigned_for!(signed: f32);

#[cfg(test)]
mod tests {
    use super::*;


    #[test]
    fn test_i32() {
        assert_eq!(5i32.abs_that_works_with_unsigned(), 5i32);
        assert_eq!((-5i32).abs_that_works_with_unsigned(), 5i32);
    }
    #[test]
    fn test_u32() {
        assert_eq!(5u32.abs_that_works_with_unsigned(), 5u32);
    }
    #[test]
    fn test_f32() {
        assert_eq!(5f32.abs_that_works_with_unsigned(), 5f32);
        assert_eq!((-5f32).abs_that_works_with_unsigned(), 5f32);
    }
}
