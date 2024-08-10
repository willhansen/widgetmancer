// TODO: re-export specific names for each type (maybe in the top level for external use only?)
// pub use float_line_segment::Shape as FloatLineSegment;
// pub use float_line_segment::Operations as OperationsForFloatLineSegment;
// pub use float_line_segment::Constructors as ConstructorsForFloatLineSegment;
automod::dir!(pub "src/utility/geometry");

pub_use!(
    angle,
    angle_interval,
    angular_direction,
    angular_edge_of_centered_arc,
    categorized_partial_angle_interval,
    circular_interval,
    complement,
    coordinates,
    has_origin_pose,
    orthogonal_facing_int_pose,
    partial_angle_interval, // TODO: make private and contained within angle_interval?
    poses,
    quadrant,
    quarter_turn_rotatable,
    reversible,
    shapes,
    size_2d,
    thing_relative_to_square,
    translate,
);

pub fn floating_point_step<P: float_coordinate::Operations>(start: P, angle: Angle<f32>, length: f32) -> P {
    start + P::unit_vector_from_angle(angle) * length
}
