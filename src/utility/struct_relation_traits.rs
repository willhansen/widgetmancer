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
    ($abstract_type:ident<P: $PointTrait:ident>, base= $BaseType:ident<P>) => {
        impl<PointType: $PointTrait> Abstraction<$BaseType<PointType>>
            for $abstract_type<PointType>
        {
            type Base = $BaseType<PointType>;
        }
        impl<PointType: $PointTrait> From<$BaseType<PointType>> for $abstract_type<PointType> {
            fn from(value: $BaseType<PointType>) -> Self {
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
    // TODO: better base indication syntax
    ($abstract_type:ident<P: $PointTrait:ident> --> $BaseType:ident<P> --> $BaserType:ident<P>) => {
        impl<PointType: $PointTrait> Abstraction<$BaserType<PointType>>
            for $abstract_type<PointType>
        where
            Self: Abstraction<$BaseType<PointType>>,
            $BaseType<PointType>: Abstraction<$BaserType<PointType>>,
        {
            type Base = $BaserType<PointType>;
        }
        impl<PointType: $PointTrait> From<$BaserType<PointType>> for $abstract_type<PointType>
        where
            Self: From<$BaseType<PointType>>,
            $BaseType<PointType>: From<$BaserType<PointType>>,
        {
            fn from(value: $BaserType<PointType>) -> Self {
                Self::from($BaseType::<PointType>::from(value))
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
