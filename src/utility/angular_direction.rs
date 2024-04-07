macro_rules! binary_enum {
    ($name:ident, $a:ident, $b:ident) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub enum $name {
            $a,
            $b,
        }

        impl std::ops::Neg for $name {
            type Output = Self;

            fn neg(self) -> Self::Output {
                use $name::*;
                match self {
                    $a => $b,
                    $b => $a,
                }
            }
        }
    };
}

binary_enum!(AngularDirection, CW, CCW);
binary_enum!(Direction1D, POSITIVE, NEGATIVE);
