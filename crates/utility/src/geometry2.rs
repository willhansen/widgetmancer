use itertools::all;

use crate::BoolIterExt;

pub type IPoint = [i32; 2];
pub type UPoint = [u32; 2];
pub type FPoint = [f32; 2];
pub type OrthoDir = i32;
pub type SquareEdge = (IPoint, OrthoDir);
// 1-indexed
pub type ScreenRowColCharPos = [u16; 2];

// minimum square and maximum square
pub type IRect = [[i32; 2]; 2];

pub const DIR_RIGHT: i32 = 0;
pub const DIR_UP: i32 = 1;
pub const DIR_LEFT: i32 = 2;
pub const DIR_DOWN: i32 = 3;
pub const ALL_ORTHODIRS: [OrthoDir; 4] = [0, 1, 2, 3];

pub const STEP_RIGHT: IPoint = [1, 0];
pub const STEP_UP: IPoint = [0, 1];
pub const STEP_LEFT: IPoint = [-1, 0];
pub const STEP_DOWN: IPoint = [0, -1];

pub trait PointExt<T> {
    fn x(&self) -> T;
    fn y(&self) -> T;
    fn new(x: T, y: T) -> Self;
}
impl<T: Copy> PointExt<T> for [T; 2] {
    fn x(&self) -> T {
        self[0]
    }
    fn y(&self) -> T {
        self[1]
    }
    fn new(x: T, y: T) -> Self {
        [x, y]
    }
}

pub trait SignedPointExt<T> {
    fn negative(&self) -> Self;
}
impl<T: std::ops::Neg> SignedPointExt<T> for IPoint {
    fn negative(&self) -> Self {
        self.map(|x| -x)
    }
}

pub trait IPointExt: Sized + PointExt<i32> {
    fn add(&self, rhs: Self) -> Self {
        Self::new(self.x() + rhs.x(), self.y() + rhs.y())
    }
    fn mul(&self, rhs: i32) -> Self {
        Self::new(self.x() * rhs, self.y() * rhs)
    }
    fn div(&self, rhs: i32) -> Self {
        Self::new(self.x() / rhs, self.y() / rhs)
    }
    fn neg(&self) -> Self {
        Self::new(-self.x(), -self.y())
    }
    fn sub(&self, rhs: Self) -> Self {
        self.add(rhs.neg())
    }
    fn dot(&self, rhs: IPoint) -> i32 {
        self.x() * rhs.x() + self.y() * rhs.y()
    }
    fn has_component_in_direction(&self, dir: OrthoDir) -> bool {
        self.dot(step_in_direction(dir)) > 0
    }
    fn has_component_against_direction(&self, dir: OrthoDir) -> bool {
        self.dot(step_in_direction(dir)) < 0
    }
    fn to_float(&self) -> FPoint {
        FPoint::new(self.x() as f32, self.y() as f32)
    }
    fn grid_square_center(&self) -> FPoint {
        self.to_float().add([0.5; 2])
    }
    fn to_string(&self) -> String {
        format!("[{}, {}]", self.x(), self.y())
    }
    fn squared_length(&self) -> i32 {
        self.x().pow(2) + self.y().pow(2)
    }
    fn abs(&self) -> UPoint;
    fn absmax(&self) -> u32 {
        *self.abs().iter().max().unwrap()
    }
    fn to_unsigned(&self) -> UPoint;
    fn to_usize(&self) -> USizePoint;
}

impl IPointExt for IPoint {
    fn abs(&self) -> UPoint {
        self.map(|x| x.abs() as u32)
    }
    fn to_unsigned(&self) -> UPoint {
        self.map(|x| {
            assert!(x >= 0);
            x as u32
        })
    }
    fn to_usize(&self) -> USizePoint {
        self.map(|x| {
            assert!(x >= 0);
            x as usize
        })
    }
}
pub trait UPointExt {
    fn to_signed(&self) -> IPoint;
    fn to_float(&self) -> FPoint;
}
impl UPointExt for UPoint {
    fn to_signed(&self) -> IPoint {
        self.map(|x| x as i32)
    }
    fn to_float(&self) -> FPoint {
        self.map(|x| x as f32)
    }
}
impl UPointExt for [u16; 2] {
    fn to_signed(&self) -> IPoint {
        self.map(|x| x as i32)
    }
    fn to_float(&self) -> FPoint {
        self.map(|x| x as f32)
    }
}
impl UPointExt for [usize; 2] {
    fn to_signed(&self) -> IPoint {
        self.map(|x| x as i32)
    }
    fn to_float(&self) -> FPoint {
        self.map(|x| x as f32)
    }
}

pub trait FPointExt: PointExt<f32> + Sized + Clone {
    fn add(&self, rhs: Self) -> Self {
        Self::new(self.x() + rhs.x(), self.y() + rhs.y())
    }
    fn mul(&self, rhs: f32) -> Self {
        Self::new(self.x() * rhs, self.y() * rhs)
    }
    fn div(&self, rhs: f32) -> Self {
        self.mul(1.0 / rhs)
    }
    fn neg(&self) -> Self {
        Self::new(-self.x(), -self.y())
    }
    fn sub(&self, rhs: Self) -> Self {
        self.add(rhs.neg())
    }
    fn dot(&self, rhs: FPoint) -> f32 {
        self.x() * rhs.x() + self.y() * rhs.y()
    }
    fn rounded(&self) -> IPoint {
        [self.x().round() as i32, self.y().round() as i32]
    }
    fn floor(&self) -> IPoint {
        [self.x().floor() as i32, self.y().floor() as i32]
    }
    fn snap_to_grid(&self) -> IPoint {
        self.floor()
    }
    fn length(&self) -> f32 {
        (self.x().powi(2) + self.y().powi(2)).sqrt()
    }
    fn dist(&self, other: Self) -> f32 {
        other.sub(self.clone()).length()
    }
    fn lerp(&self, other: Self, t: f32) -> Self {
        if t == 0.0 {
            return self.clone();
        }
        if t == 1.0 {
            return other;
        }
        let dp = other.sub(self.clone());
        self.add(dp.mul(t))
    }
    fn normalized(&self) -> Self {
        self.div(self.length())
    }
}
impl FPointExt for FPoint {}

pub type USizePoint = [usize; 2];
pub trait USizePointExt {
    fn to_int(&self) -> IPoint;
}
impl USizePointExt for [usize; 2] {
    fn to_int(&self) -> IPoint {
        self.map(|x| x as i32)
    }
}

pub trait OrthoPoseExt {
    fn rotate(&self, quarter_turns_ccw: i32) -> Self;
    fn reversed(&self) -> Self;
    fn stepped(&self) -> Self;
}
impl OrthoPoseExt for ([i32; 2], i32) {
    fn rotate(&self, quarter_turns_ccw: i32) -> Self {
        (self.0, (self.1 + quarter_turns_ccw).rem_euclid(4))
    }
    fn reversed(&self) -> Self {
        (self.0, (self.1 + 2) % 4)
    }
    fn stepped(&self) -> Self {
        (self.0.add(step_in_direction(self.1)), self.1)
    }
}

pub fn step_in_direction(dir: OrthoDir) -> IPoint {
    match dir {
        0 => [1, 0],
        1 => [0, 1],
        2 => [-1, 0],
        3 => [0, -1],
        _ => panic!("invalid direction: {dir}"),
    }
}
#[allow(dead_code)]
pub fn closest_ortho_dir(square: IPoint) -> Option<OrthoDir> {
    if square[0].abs() == square[1].abs() {
        return None;
    }

    Some(if square[0].abs() > square[1].abs() {
        if square[0] > 0 {
            0
        } else {
            2
        }
    } else {
        if square[1] > 0 {
            1
        } else {
            3
        }
    })
}

pub fn rotate_quarter_turns<T: std::ops::Neg<Output = T> + Copy>(v: [T; 2], turns: i32) -> [T; 2] {
    match turns.rem_euclid(4) {
        0 => v,
        1 => [-v[1], v[0]],
        2 => [-v[0], v[1]],
        3 => [v[1], -v[0]],
        _ => unreachable!(),
    }
}

pub fn other_side_of_edge(edge: SquareEdge) -> SquareEdge {
    let step = step_in_direction(edge.1);
    let reverse_dir = (edge.1 + 2).rem_euclid(4);
    ([edge.0[0] + step[0], edge.0[1] + step[1]], reverse_dir)
}

pub trait IRectExt: Sized {
    fn min_square(&self) -> IPoint;
    fn max_square(&self) -> IPoint;
    fn size(&self) -> USizePoint;
    fn width(&self) -> usize {
        self.size()[0]
    }
    fn height(&self) -> usize {
        self.size()[1]
    }
    fn from_min_and_size(min: IPoint, size: USizePoint) -> Self {
        let max = min.add(size.to_signed().sub([1; 2]));
        Self::from_min_and_max(min, max)
    }
    fn from_min_and_max(min: IPoint, max: IPoint) -> Self;
    fn contains_square(&self, square: IPoint) -> bool {
        let min = self.min_square();
        let max = self.max_square();
        let a = [0, 1].map(|i| square[i] >= min[i] && square[i] <= max[i]);
        all(a, |x| x)
    }
    fn contains_rect(&self, other: IRect) -> bool {
        self.contains_square(other.min_square()) && self.contains_square(other.max_square())
    }
    fn valid(&self) -> bool;
    fn from_center_and_radius(center: IPoint, radius: u32) -> Self {
        let min = center.sub([radius; 2].to_signed());
        let max = center.add([radius; 2].to_signed());
        Self::from_min_and_max(min, max)
    }
    // quadrants start top-right and go counter-clockwise
    fn corner_by_quadrant(&self, nth_quadrant: i32) -> IPoint {
        let [x0, y0] = self.min_square();
        let [x1, y1] = self.max_square();
        match nth_quadrant.rem_euclid(4) {
            0 => [x1, y1],
            1 => [x0, y1],
            2 => [x0, y0],
            3 => [x1, y0],
            _ => unreachable!("rem_euclid fail"),
        }
    }
    fn top_right_corner(&self) -> IPoint {
        self.corner_by_quadrant(0)
    }
    fn top_left_corner(&self) -> IPoint {
        self.corner_by_quadrant(1)
    }
    fn bottom_left_corner(&self) -> IPoint {
        self.corner_by_quadrant(2)
    }
    fn bottom_right_corner(&self) -> IPoint {
        self.corner_by_quadrant(3)
    }
    // Only provides a center if the rectangle has odd width and height
    fn center(&self) -> Option<IPoint> {
        if self.size().map(|x| x % 2 == 0).any_true() {
            return None;
        }
        let half_diag = self.size().to_signed().div(2);
        Some(self.min_square().add(half_diag))
    }
    fn border_squares(self) -> impl Iterator<Item = IPoint> {
        let [x1, y1] = self.max_square();
        (0..x1)
            .map(move |dx| [dx, 0])
            .chain((0..y1).map(move |dy| [x1, dy]))
            .chain((0..x1).map(move |dx| [x1 - dx, y1]))
            .chain((0..y1).map(move |dy| [0, y1 - dy]))
            .map(move |x| self.min_square().add(x))
    }
    fn covered_squares(self) -> impl Iterator<Item = IPoint> {
        let [w, h] = self.size().to_signed();
        let [x0, y0] = self.min_square();
        (0..w)
            .map(move |x| (0..h).map(move |y| [x0 + x, y0 + y]))
            .flatten()
    }
    fn to_string(&self) -> String;
    fn add(&self, rhs: IPoint) -> Self {
        Self::from_min_and_size(self.min_square().add(rhs), self.size())
    }
}
impl IRectExt for IRect {
    fn min_square(&self) -> IPoint {
        self[0]
    }
    fn max_square(&self) -> IPoint {
        self[1]
    }
    // width, height
    fn size(&self) -> USizePoint {
        self[1].sub(self[0]).add([1; 2]).to_usize()
    }
    fn from_min_and_max(min: IPoint, max: IPoint) -> Self {
        [min, max]
    }
    fn valid(&self) -> bool {
        [0, 1].map(|i| self[0][i] <= self[1][i]).all_true()
    }
    fn to_string(&self) -> String {
        format!("{:?}", self)
    }
}

#[cfg(test)]
mod ui_handler_tests {
    use super::*;
    #[test]
    fn test_rect_inside() {
        let rect = IRect::from_min_and_size([0, 0], [3, 4]);

        assert!(rect.contains_square([1, 1]));
        assert!(rect.contains_square([0, 1]));
        assert!(!rect.contains_square([-1, 1]));
    }
    #[test]
    fn test_rect_creation() {
        let rect = IRect::from_min_and_max([0, 0], [3, 4]);
        assert_eq!(rect.min_square(), [0, 0]);
        assert_eq!(rect.max_square(), [3, 4]);
        assert_eq!(rect.size(), [4, 5]);
        assert!(rect.contains_square([3, 4]));
        assert!(rect.contains_square([3, 3]));
        assert!(rect.contains_square([2, 4]));
        assert!(!rect.contains_square([4, 4]));
        assert!(!rect.contains_square([3, 5]));

        let rect = IRect::from_min_and_size([0, 0], [3, 4]);
        assert_eq!(rect.max_square(), [2, 3]);
    }
    #[test]
    fn test_rect_center() {
        assert_eq!(
            IRect::from_min_and_size([0, 0], [3, 3]).center(),
            Some([1, 1])
        );
        assert_eq!(
            IRect::from_min_and_size([0, 1], [3, 5]).center(),
            Some([1, 3])
        );
        assert_eq!(
            IRect::from_min_and_size([0, -2], [3, 3]).center(),
            Some([1, -1])
        );
        assert_eq!(IRect::from_min_and_size([0, 0], [4, 5]).center(), None);
    }
}
