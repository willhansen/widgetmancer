// TODO: re-export specific names for each type (maybe in the top level for external use only?)
// pub use float_line_segment::Shape as FloatLineSegment;
// pub use float_line_segment::Operations as OperationsForFloatLineSegment;
// pub use float_line_segment::Constructors as ConstructorsForFloatLineSegment;
automod::dir!("src/utility/geometry");

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
    size_2d,
    translate,
);
