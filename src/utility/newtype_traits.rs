use crate::utility::*;

// Traits to help keep all the conversion requirements between newtypes straight

/// The type is being used as a subset of the base type.  Can be a refinement of multiple other types.
// TODO: is this more of a conceptual refinement than the actual type system version?
pub trait Refinement<RefinementBase>: TryFrom<RefinementBase> + Into<RefinementBase> {
    fn valid_refinement(base: RefinementBase) -> bool;
}

// This trait is needed so newtypes can access the contained type generally, because apparently `Self::0` isn't a type analog to `self.0`
pub trait NewType {
    type BaseOfNewType;
}

// TODO: enforce the newtype sub-traits being mutually exclusive.  Need negative trait bounds?

pub trait RefinementNewType: NewType + Refinement<Self::BaseOfNewType> {}

/// Hides some information of the base type
pub trait AbstractionNewType: NewType + From<Self::BaseOfNewType> {}

/// The type is being used in a new way, and all known semantics are discarded
pub trait SemanticNewtype: NewType {}

// TODO: implement The `From` and `TryFrom` traits with macros
// macro_rules! impl_refinement_conversions {
//     ($base:ident, $refinement:ident) => {
//         impl
//     };
// }
