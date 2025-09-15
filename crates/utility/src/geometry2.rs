pub type IPoint = [i32; 2];
pub type UPoint = [u32; 2];
pub type FPoint = [f32; 2];
pub type OrthoDir = i32;
pub type SquareEdge = (IPoint, OrthoDir);
// 1-indexed
pub type ScreenRowColCharPos = [u16; 2];

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
impl<T: Copy> PointExt<T> for [T;2] {

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
}

impl IPointExt for IPoint {
    fn abs(&self) -> UPoint {
        self.map(|x| x.abs() as u32)
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
impl UPointExt for [u16;2] {
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
impl FPointExt for FPoint {
}

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
