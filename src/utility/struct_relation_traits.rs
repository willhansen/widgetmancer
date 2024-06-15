use crate::utility::*;

// Traits to help keep all the conversion requirements between newtypes straight

/// The type is being used as a subset of the base type.  Can be a refinement of multiple other types.
// TODO: is this more of a conceptual refinement than the actual type system version?
// TODO: how require that implements OperationsFor<RefinementBase>
pub trait RefinementOf<RawType>: TryFrom<RawType> + Into<RawType> {
    fn valid(&self) -> bool;
}
// For when you want to automatically propagate a refinement relation down a pair of newtypes
// TODO: abstraction rather than newtype?

/// Indicates that the implementing type has less information visible than the base type.
/// - Can be created with same constructors as base (TODO: enforce)
/// - Operations on this type can be applied to the base type as well (TODO: enforce)
pub trait AbstractionOf<BaseType>: From<BaseType> {}

pub trait AbstractsTo<AbstractType>: Into<AbstractType> {
    fn set_with_abstraction(&self, val: &AbstractType) -> Self;
}

macro_rules! impl_abstraction_for_newtype {
    ($abstract_type:ident<P: $PointReqs:ident>, base= $BaseType:ident<P>) => {
        impl<PointType: $PointReqs> AbstractionOf<$BaseType<PointType>>
            for $abstract_type<PointType>
        {
        }
        impl<PointType: $PointReqs> From<$BaseType<PointType>> for $abstract_type<PointType> {
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
// TODO: update notation for consistency with other parameterized impl macros
macro_rules! impl_abstraction_skip_level {
    // TODO: better base indication syntax
    ($abstract_type:ident<P: $PointReqs:ident> --> $BaseType:ident<P> --> $BaserType:ident<P>) => {
        impl<PointType: $PointReqs> AbstractionOf<$BaserType<PointType>>
            for $abstract_type<PointType>
        where
            Self: AbstractionOf<$BaseType<PointType>>,
            $BaseType<PointType>: AbstractionOf<$BaserType<PointType>>,
        {
        }
        impl<PointType: $PointReqs> From<$BaserType<PointType>> for $abstract_type<PointType>
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

macro_rules! impl_skip_level_try_from {
    // TODO: better base indication syntax
    ($EndType:ident<P: $PointReqs:ident> --> $MidType:ident<P> --> $StartType:ident<P>) => {
        impl<P: $PointReqs> TryFrom<$StartType<P>> for $EndType<P>
        where
            Self: TryFrom<$MidType<P>>,
            $MidType<P>: TryFrom<$StartType<P>>,
        {
            // TODO
            type Error = String;
            fn try_from(value: $StartType<P>) -> Result<$EndType<P>, Self::Error> {
                let mid: $MidType<P> = value.try_into()?;
                mid.try_into()
            }
        }
    };
}
pub(crate) use impl_skip_level_try_from;

// The type is being used in a new way, and all known semantics are discarded
// TODO: This kind of feels like the default use of a newtype, and maybe does not need a dedicated trait...  Only use I can think of is to block implementation of the other two newtype traits

// TODO: implement The `From` and `TryFrom` traits with macros
// macro_rules! impl_refinement_conversions {
//     ($base:ident, $refinement:ident) => {
//         impl
//     };
// }
