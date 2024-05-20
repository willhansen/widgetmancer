use crate::utility::*;

// Traits to help keep all the conversion requirements between newtypes straight

/// The type is being used as a subset of the base type.  Can be a refinement of multiple other types.
// TODO: is this more of a conceptual refinement than the actual type system version?
// TODO: how require that implements OperationsFor<RefinementBase>
pub trait Refinement<Base>: TryFrom<Base> + Into<Base> {
    fn valid(&self) -> bool;
}
// For when you want to automatically propagate a refinement relation down a pair of newtypes
// TODO: abstraction rather than newtype?


///     v--refinement
/// A------------------>B
/// |                   |
/// |   abstraction     |   abstraction
/// |<--newtype         |<--newtype
/// |                   |
/// v                   v
/// C- - - - - - - - - >D
///      ^--new refinement
///
/// A = DiagonalBase
/// B = NewtypeBase
/// C = RefinementBase
/// D = Self
macro_rules! impl_parallel_refinement_for_newtype {
    ($Self:ident<P: $PointReqs:ident>, newtype_base= $NewtypeBase:ident<P>, refinement_base= $RefinementBase:ident<P>, diagonal_base= $DiagonalBase:ident<P>) => {

        impl<P: PointReqs> Refinement<$RefinementBase<P>> for $Self<P> 
        where 
            $NewtypeBase<P>: Refinement<$DiagonalBase<P>>,
            $RefinementBase<P>: Abstraction<$DiagonalBase<P>>,
            Self: Abstraction<$NewtypeBase<P>>

        {
            fn valid(&self) -> bool {
                self.0.valid()
            }
        }

        impl<P: $PointReqs> TryFrom<$RefinementBase<P>> for $Self<P> {
            type Error = String;

            fn try_from(value: $RefinementBase<P>) -> Result<Self, Self::Error> {
            
                let maybe_valid = Self(value.0.try_into()?);
                if !maybe_valid.valid() {
                    Err(format!("NOT VALID: {:?}", maybe_valid))
                }
                else {
                    Ok(maybe_valid)
                }
            
            }
        }

        impl<P: PointReqs> From<$Self<P>> for $RefinementBase<P> {
            fn from(value: $Self<P>) -> Self {
                // up abstraction
                let newtype_base: $NewtypeBase<P> = value.0;
                let diagonal_base: $DiagonalBase<P> = newtype_base.into();
                let refinement_base: $RefinementBase<P> = diagonal_base.into();
                refinement_base
                
            }
        }



    }
}
pub(crate) use impl_parallel_refinement_for_newtype;

/// Indicates that the implementing type has less information visible than the base type.
/// - Can be created with same constructors as base (TODO: enforce)
/// - Operations on this type can be applied to the base type as well (TODO: enforce)
pub trait Abstraction<Base>: From<Base> {
}

macro_rules! impl_abstraction_for_newtype {
    ($abstract_type:ident<P: $PointReqs:ident>, base= $BaseType:ident<P>) => {
        impl<PointType: $PointReqs> Abstraction<$BaseType<PointType>>
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
macro_rules! impl_abstraction_skip_level {
    // TODO: better base indication syntax
    ($abstract_type:ident<P: $PointReqs:ident> --> $BaseType:ident<P> --> $BaserType:ident<P>) => {
        impl<PointType: $PointReqs> Abstraction<$BaserType<PointType>>
            for $abstract_type<PointType>
        where
            Self: Abstraction<$BaseType<PointType>>,
            $BaseType<PointType>: Abstraction<$BaserType<PointType>>,
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
// The type is being used in a new way, and all known semantics are discarded
// TODO: This kind of feels like the default use of a newtype, and maybe does not need a dedicated trait...  Only use I can think of is to block implementation of the other two newtype traits

// TODO: implement The `From` and `TryFrom` traits with macros
// macro_rules! impl_refinement_conversions {
//     ($base:ident, $refinement:ident) => {
//         impl
//     };
// }
