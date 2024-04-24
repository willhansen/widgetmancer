use crate::utility::*;

// A trait to help keep all the conversion requirements between newtypes straight
pub trait RefinementNewtype: TryFrom<Self::Base> + Into<Self::Base> {
    type Base;
    fn valid_refinement(base: Self::Base) -> bool;
}

// TODO: implementation macros, but the templates may be tricky
// macro_rules! impl_refinement_conversions {
//     ($base:ident, $refinement:ident) => {
//         impl
//     };
// }
