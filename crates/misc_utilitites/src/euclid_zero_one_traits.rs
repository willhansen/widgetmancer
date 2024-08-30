// Copied from https://docs.rs/euclid/latest/src/euclid/num.rs.html
use num_traits;

// Euclid has its own Zero and One traits instead of of using the num_traits equivalents.
// Unfortunately, num_traits::Zero requires Add, which opens a bag of sad things:
//  - Most importantly, for Point2D to implement Zero it would need to implement Add<Self> which we
//    don't want (we allow "Point + Vector" and "Vector + Vector" semantics and purposefully disallow
//    "Point + Point".
//  - Some operations that require, say, One and Div (for example Scale::inv) currently return a
//    type parameterized over T::Output which is ambiguous with num_traits::One because it inherits
//    Mul which also has an Output associated type. To fix it need to complicate type signatures
//    by using <T as Trait>::Output which makes the code and documentation harder to read.
//
// On the other hand, euclid::num::Zero/One are automatically implemented for all types that
// implement their num_traits counterpart. Euclid users never need to explicitly use
// euclid::num::Zero/One and can/should only manipulate the num_traits equivalents without risk
// of compatibility issues with euclid.

pub trait Zero {
    fn zero() -> Self;
}

impl<T: num_traits::Zero> Zero for T {
    fn zero() -> T {
        num_traits::Zero::zero()
    }
}

pub trait One {
    fn one() -> Self;
}

impl<T: num_traits::One> One for T {
    fn one() -> T {
        num_traits::One::one()
    }
}
