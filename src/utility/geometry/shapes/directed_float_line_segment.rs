use crate::utility::*;

trait_alias!(pub trait PointReqs = float_coordinate::Operations);

impl<T, P: PointReqs> float_line_segment::Operations<P> for T where T: float_line::Operations<P> + line_segment::Operations<P> {}

pub trait Operations<P: PointReqs>:
    directed_line::Operations<P> + float_line_segment::Operations<P>
{
    fn lerp(&self, t: f32) -> P {
        self.start().lerp2d(self.end(), t)
    }
}
impl<T, P: PointReqs> directed_float_line_segment::Operations<P> for T where
    T: directed_line::Operations<P> + float_line_segment::Operations<P>
{
}
