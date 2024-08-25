mod shapes;
use crate::shapes::*;
mod trait_alias_macro;
use crate::trait_alias_macro::trait_alias;

use std::ops::{Add, Div, Mul, Neg, Sub};
use std::fmt::Debug;

pub type FloatPoint = euclid::Vector2D<f32, euclid::UnknownUnit>;

trait_alias!(pub trait FloatPointReqs =  Neg<Output = Self>
    + Copy
    + PartialEq
    + Add<Self, Output = Self>
    + Sub<Self, Output = Self>
    // + Mul<i32, Output = Self>
    + Mul<f32, Output = Self>
    + Div<f32, Output = Self>
    + euclid::num::Zero
    + Sized
    + Debug
);

static_assertions::assert_impl_all!(FloatPoint: FloatPointReqs);




macro_rules! verify_abstraction_relation {
    (concrete: $concrete_module:ident, abstract: $abstract_module:ident, point type: $point_type:ty) => {
        // Confirm the concrete type knows about the abstract type
        static_assertions::assert_impl_all!(
            $concrete_module::Shape<$point_type>:
            AbstractsTo<$abstract_module::Shape<$point_type>>
        );
        // Confirm the abstracted type knows about the concrete type
        static_assertions::assert_impl_all!($abstract_module::Shape<$point_type>: AbstractionOf <$concrete_module::Shape<$point_type>>);

        // Confirm the concrete type's operations can use the abstract type's operations
        static_assertions::assert_trait_sub_all!($concrete_module::Operations<$point_type>: $abstract_module::Operations<$point_type>);
        // Confirm the abstraction can be constructed with the new module's constructors
        static_assertions::assert_trait_sub_all!($abstract_module::Constructors<$point_type>: $concrete_module::Constructors<$point_type>);
    };
}

macro_rules! verify_refinement_relation {
    (raw: $raw_module:ident, refined: $refined_module:ident, point type: $point_type:ty) => {
        // Confirm the new module knows about the raw type it is a refinement of
        static_assertions::assert_impl_all!($refined_module::Shape<$point_type>: RefinementOf<$raw_module::Shape<$point_type>>);
        // Confirm the new module can use the operations and constructors of the raw type
        static_assertions::assert_trait_sub_all!($refined_module::Operations<$point_type>: $raw_module::Operations<$point_type>);
        static_assertions::assert_trait_sub_all!($refined_module::Constructors<$point_type>: $raw_module::Constructors<$point_type>);

    };
}

macro_rules! validate_new_shape_for_specific_point_type {
	($new_module:ident,
    $shape_name:ident,
		// rename to abstracted_type?
		$(abstracts_to: $($abstracts_to:ident,)+;)?
		// rename to concrete_type?
		$(abstraction_of: $($abstraction_of:ident,)+;)?
		// rename to raw_type?
		$(refinement_of: $($refinement_of:ident,)+;)?
    point type: $point_type:ty
    ) => {

        static_assertions::assert_impl_all!($new_module::Shape<$point_type>: $new_module::Operations<$point_type>, $new_module::Constructors<$point_type>);
        static_assertions::assert_trait_sub_all!($new_module::Operations<$point_type>: $new_module::Constructors<$point_type>);
        // abstracts to
        $($(
            verify_abstraction_relation!(concrete: $new_module, abstract: $abstracts_to, point type: $point_type);
        )+)?
        // abstraction of
        $($(
            verify_abstraction_relation!(concrete: $abstraction_of, abstract: $new_module, point type: $point_type);
        )+)?
        // refinement of
        $($(
            verify_refinement_relation!(raw: $refinement_of, refined: $new_module, point type: $point_type);
        )+)?
	}
}
macro_rules! validate_shape {
	($new_module:ident,
    $shape_name:ident,
		// rename to abstracted_type?
		$(abstracts_to: $($abstracts_to:ident,)+;)?
		// rename to concrete_type?
		$(abstraction_of: $($abstraction_of:ident,)+;)?
		// rename to raw_type?
		$(refinement_of: $($refinement_of:ident,)+;)?
    ) => {

        pub type $shape_name<P> = $new_module::Shape<P>;

        validate_new_shape_for_specific_point_type!(
            $new_module,
            $shape_name,
            // rename to abstracted_type?
            $(abstracts_to: $($abstracts_to,)+;)?
            // rename to concrete_type?
            $(abstraction_of: $($abstraction_of,)+;)?
            // rename to raw_type?
            $(refinement_of: $($refinement_of,)+;)?
            point type: FloatPoint // TODO: should not need to pick a point type to verify.  Ideally would validate schema generally)
        );
    }
}

// These function calls are the code representation of the `struct_and_trait_diagram` in the docs

validate_shape!(two_different_points, TwoDifferentPoints, abstracts_to: directed_line,; );
validate_shape!(directed_line, DirectedLine, abstraction_of: two_different_points,; );
// validate_shape!(two_different_points, TwoDifferentPoints, abstracts_to: directed_line, ray, directed_line_segment,; );

// Traits to help keep all the conversion requirements between newtypes straight

/// The type is being used as a subset of the base type.  Can be a refinement of multiple other types.
// TODO: is this more of a conceptual refinement than the actual type system version?
// TODO: how require that implements OperationsFor<RefinementBase>
pub trait RefinementOf<RawType>: TryFrom<RawType> + Into<RawType> {
    fn valid(&self) -> bool;
}

/// Indicates that the implementing type has less information visible than the base type.
/// - Can be created with same constructors as base (TODO: enforce)
/// - Operations on this type can be applied to the base type as well (TODO: enforce)
pub trait AbstractionOf<BaseType>: From<BaseType> {}

pub trait AbstractsTo<AbstractType>: Into<AbstractType> {
    fn set_with_abstraction(&self, val: &AbstractType) -> Self;
}

macro_rules! impl_abstraction_via_newtype {
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
pub(crate) use impl_abstraction_via_newtype;

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
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_make_line() {
        // DirectedLine::from_point_and_direction(p(1.0, 2.0), UP);
        println!("tested");
    }
}
