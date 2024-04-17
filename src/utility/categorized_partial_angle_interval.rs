use crate::utility::*;
use derive_getters::Getters;
use derive_more::Constructor;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum CategorizedPartialAngleInterval {
    Ray(RayAngleInterval),               // width == 0, one angle
    Cone(Cone),                          // width in (0,180)
    HalfPlane(HalfPlaneAngleInterval),   // width == 180
    InverseCone(InverseCone),            // width in (180,360)
    InverseRay(InverseRayAngleInterval), // width == 360, one angle
}
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct HalfPlaneAngleInterval(PartialAngleInterval);
impl HalfPlaneAngleInterval {
    pub fn from_center_angle(a: FAngle) -> Self {
        Self::new(PartialAngleInterval::from_angles(
            a.turned_right(),
            a.turned_left(),
        ))
    }
    pub fn new(a: PartialAngleInterval) -> Self {
        // TODO: add tolerance
        assert!(a.width() == FAngle::pi());
        Self(a)
    }
    pub fn get(&self) -> PartialAngleInterval {
        self.0
    }
    pub fn complement(&self) -> Self {
        Self::new(self.0.complement())
    }
}

/// angle is less than 180 degrees only
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Cone(PartialAngleInterval);
impl Cone {
    pub fn new(a: PartialAngleInterval) -> Self {
        assert!(a.width() > FAngle::zero());
        assert!(a.width() < FAngle::pi());
        Self(a)
    }
    pub fn get(&self) -> PartialAngleInterval {
        self.0
    }
    pub fn complement(&self) -> InverseCone {
        InverseCone::new(self.0.complement())
    }
}

// Angle is between 180 and 360.  Complement of a Cone
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct InverseCone(PartialAngleInterval);
impl InverseCone {
    pub fn new(a: PartialAngleInterval) -> Self {
        assert!(a.width() > FAngle::pi());
        assert!(a.width() < FAngle::two_pi());
        Self(a)
    }
    pub fn get(&self) -> PartialAngleInterval {
        self.0
    }
    pub fn complement(&self) -> Cone {
        Cone::new(self.0.complement())
    }
}
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct RayAngleInterval(FAngle);
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct InverseRayAngleInterval(FAngle);

#[cfg(test)]
mod tests {
    use ntest::assert_about_eq;

    use super::*;
    #[test]
    fn test_cone() {
        let c: Cone = Cone::new(PartialAngleInterval::from_degrees(40.0, 80.0));
        let ic: InverseCone = c.complement();
        assert_about_eq!(ic.get().width().to_degrees(), 320.0);
        assert_about_eq!(
            ic.get().center_angle().radians,
            (FAngle::degrees(60.0) + FAngle::pi()).radians
        );
    }
}
