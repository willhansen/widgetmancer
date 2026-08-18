use ambassador::{delegatable_trait, Delegate};
use derive_more::From;
use euclid::*;
use getset::{CopyGetters, Setters};

use utility::*;

/// Stable identity of a floating entity, assigned by `Game` at spawn.
/// Exists so subsystems outside the model (the renderer's per-entity
/// caches) can track an entity across frames without the entity itself
/// knowing what is cached.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FloatingEntityId(pub u64);

#[delegatable_trait]
pub trait FloatingEntityTrait {
    fn position(&self) -> WorldPoint;
    fn set_position(&mut self, position: WorldPoint);
    fn velocity(&self) -> WorldMove;
    fn set_velocity(&mut self, velocity: WorldMove);
}

#[derive(Clone, PartialEq, Debug, Copy, From, Delegate)]
#[delegate(FloatingEntityTrait)]
pub(crate) enum FloatingEntityEnum {
    DeathCube(DeathCube),
    FloatingHunterDrone(FloatingHunterDrone),
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct DeathCube {
    pub(crate) id: FloatingEntityId,
    pub(crate) position: WorldPoint,
    pub(crate) velocity: WorldMove,
}
impl DeathCube {
    pub fn new(id: FloatingEntityId, position: WorldPoint, velocity: WorldMove) -> Self {
        DeathCube {
            id,
            position,
            velocity,
        }
    }
}

impl FloatingEntityTrait for DeathCube {
    fn position(&self) -> WorldPoint {
        self.position
    }
    fn set_position(&mut self, position: WorldPoint) {
        self.position = position;
    }
    fn velocity(&self) -> WorldMove {
        self.velocity
    }
    fn set_velocity(&mut self, velocity: WorldMove) {
        self.velocity = velocity;
    }
}

pub const HUNTER_DRONE_SIGHT_RANGE: f32 = 5.0;

#[derive(PartialEq, Debug, Copy, Clone, Setters, CopyGetters)]
pub struct FloatingHunterDrone {
    pub(crate) id: FloatingEntityId,
    pub(crate) position: WorldPoint,
    pub(crate) velocity: WorldMove,
    #[getset(get_copy = "pub", set = "pub")]
    pub(crate) sight_direction: Angle<f32>,
}

impl FloatingEntityTrait for FloatingHunterDrone {
    fn position(&self) -> WorldPoint {
        self.position
    }
    fn set_position(&mut self, position: WorldPoint) {
        self.position = position;
    }
    fn velocity(&self) -> WorldMove {
        self.velocity
    }
    fn set_velocity(&mut self, velocity: WorldMove) {
        self.velocity = velocity;
    }
}

impl FloatingHunterDrone {
    pub fn new(
        id: FloatingEntityId,
        position: WorldPoint,
        velocity: WorldMove,
        sight_direction: Angle<f32>,
    ) -> Self {
        FloatingHunterDrone {
            id,
            position,
            velocity,
            sight_direction,
        }
    }
}
