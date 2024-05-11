use crate::utility::*;

// Traits to help keep all the conversion requirements between newtypes straight

/// The type is being used as a subset of the base type.  Can be a refinement of multiple other types.
// TODO: is this more of a conceptual refinement than the actual type system version?
// TODO: how require that implements OperationsFor<RefinementBase>
pub trait Refinement<Base>: TryFrom<Self::Base> + Into<Self::Base> {
    type Base;
    fn valid_refinement(base: Self::Base) -> bool;
}

/// Indicates that the implementing type has less information visible than the base type.
/// - Can be created with same constructors as base (TODO: enforce)
/// - Operations on this type can be applied to the base type as well (TODO: enforce)
pub trait Abstraction<Base>: From<Self::Base> {
    type Base;
}

macro_rules! impl_abstraction_for_newtype {
    ($abstract_type:ident<P: $point_trait:ident>($base_type:ident<P>)) => {
        impl<PointType: $point_trait> Abstraction<$base_type<PointType>>
            for $abstract_type<PointType>
        {
            type Base = $base_type<PointType>;
        }
        impl<PointType: $point_trait> From<$base_type<PointType>> for $abstract_type<PointType> {
            fn from(value: $base_type<PointType>) -> Self {
                Self(value)
            }
        }
    };
}
pub(crate) use impl_abstraction_for_newtype;

// abstractions chain together
// TODO: analogous chain for refinement
// TODO: adapt macro to arbitrary chain length?
macro_rules! impl_abstraction_skip_level {
    ($abstract_type:ident<P: $point_trait:ident> --> $base_type:ident<P> --> $baser_type:ident<P>) => {
        impl<PointType: $point_trait> Abstraction<$baser_type<PointType>>
            for $abstract_type<PointType>
        where
            Self: Abstraction<$base_type<PointType>>,
            $base_type<PointType>: Abstraction<$baser_type<PointType>>,
        {
            type Base = $baser_type<PointType>;
        }
        impl<PointType: $point_trait> From<$baser_type<PointType>> for $abstract_type<PointType>
        where
            Self: From<$base_type<PointType>>,
            $base_type<PointType>: From<$baser_type<PointType>>,
        {
            fn from(value: $baser_type<PointType>) -> Self {
                Self::from($base_type::<PointType>::from(value))
            }
        }
    };
}
pub(crate) use impl_abstraction_skip_level;
// The type is being used in a new way, and all known semantics are discarded
// TODO: This kind of feels like the default use of a newtype, and maybe does not need a dedicated trait...  Only use I can think of is to block implementation of the other two newtype traits

// TODO: implement The `From` and `TryFrom` traits with macros
// macro_rules! impl_refinement_conversions {
//     ($base:ident, $refinement:ident) => {
//         impl
//     };
// }
