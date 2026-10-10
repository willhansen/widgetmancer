//! Self-contained "placed world features": conveyor belts and upgrades. Solid
//! blocks live in the terrain voxel set (see `terrain.rs`); this module owns the
//! non-solid floor features plus the pure accessors. The orchestrating `Game`
//! methods delegate to this via thin shims.

use std::collections::HashMap;
use std::time::Duration;

use crate::piece::Upgrade;
use utility::*;

pub const CONVEYOR_BELT_MOVEMENT_PERIOD: Duration = Duration::new(2, 0);
pub const CONVEYOR_BELT_VISUAL_PERIOD: Duration = CONVEYOR_BELT_MOVEMENT_PERIOD.saturating_mul(2);

#[derive(Clone, Eq, PartialEq, Debug, Copy)]
pub enum FloorFeature {
    PushArrow(OrthogonalWorldStep),
    ConveyorBelt(OrthogonalWorldStep),
}

/// One conveyor-belt square. `movement_period` is how long it takes to carry an
/// entity one square, so a shorter period is a faster belt; the visual phase
/// cycles over twice the period. Per-square periods let a single run ramp
/// (slow ends, fast middle) and let separate belts run at different speeds
/// (issue 0016).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ConveyorBelt {
    pub direction: OrthogonalWorldStep,
    pub movement_period: Duration,
}

impl ConveyorBelt {
    pub fn new(direction: OrthogonalWorldStep, movement_period: Duration) -> Self {
        ConveyorBelt {
            direction,
            movement_period,
        }
    }

    pub fn with_default_period(direction: OrthogonalWorldStep) -> Self {
        ConveyorBelt::new(direction, CONVEYOR_BELT_MOVEMENT_PERIOD)
    }

    /// Squares per second this square carries an entity.
    pub fn speed(&self) -> f32 {
        1.0 / self.movement_period.as_secs_f32()
    }

    pub fn visual_period(&self) -> Duration {
        self.movement_period.saturating_mul(2)
    }
}

pub fn conveyor_belt_speed() -> f32 {
    1.0 / CONVEYOR_BELT_MOVEMENT_PERIOD.as_secs_f32()
}

/// True if a full movement-period boundary was crossed between
/// `prev_time_since_start` and `prev_time_since_start + delta`.
pub fn conveyor_period_just_elapsed(
    prev_time_since_start: Duration,
    delta: Duration,
    movement_period: Duration,
) -> bool {
    let period = movement_period.as_secs_f32();
    let prev_conveyor_periods_since_start = prev_time_since_start.as_secs_f32() / period;
    let new_conveyor_periods_since_start =
        delta.as_secs_f32() / period + prev_conveyor_periods_since_start;

    new_conveyor_periods_since_start.floor() > prev_conveyor_periods_since_start.floor()
}

#[derive(Clone, Debug, Default)]
pub struct FloorFeatures {
    pub upgrades: HashMap<WorldSquare, Upgrade>,
    pub conveyor_belts: HashMap<WorldSquare, ConveyorBelt>,
}

impl FloorFeatures {
    pub fn new() -> Self {
        Self::default()
    }

    /// Place a belt at the default speed.
    pub fn place_conveyor_belt(&mut self, square: WorldSquare, dir: WorldStep) {
        self.place_conveyor_belt_with_speed(square, dir, 1.0);
    }

    /// Place a belt at `speed_multiplier` times the default speed (1.0 = the
    /// original belt). The multiplier is clamped positive.
    pub fn place_conveyor_belt_with_speed(
        &mut self,
        square: WorldSquare,
        dir: WorldStep,
        speed_multiplier: f32,
    ) {
        let speed_multiplier = if speed_multiplier.is_finite() && speed_multiplier > 0.0 {
            speed_multiplier
        } else {
            1.0
        };
        let period = CONVEYOR_BELT_MOVEMENT_PERIOD.div_f32(speed_multiplier);
        self.conveyor_belts
            .insert(square, ConveyorBelt::new(dir.into(), period));
    }

    pub fn place_upgrade(&mut self, upgrade_type: Upgrade, square: WorldSquare) {
        self.upgrades.insert(square, upgrade_type);
    }

    pub fn is_upgrade_at(&self, square: WorldSquare) -> bool {
        self.upgrades.contains_key(&square)
    }
}
