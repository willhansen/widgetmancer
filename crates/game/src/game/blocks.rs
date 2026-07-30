//! Self-contained "placed world features": solid blocks, conveyor belts, and
//! upgrades. Owns the three storage collections and the pure accessors; the
//! orchestrating `Game` methods delegate to this via thin shims.

use std::collections::{HashMap, HashSet};
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

pub fn conveyor_belt_speed() -> f32 {
    1.0 / CONVEYOR_BELT_MOVEMENT_PERIOD.as_secs_f32()
}

/// True if a full conveyor-belt movement period boundary was crossed between
/// `prev_time_since_start` and `prev_time_since_start + delta`.
pub fn conveyor_period_just_elapsed(prev_time_since_start: Duration, delta: Duration) -> bool {
    let prev_conveyor_periods_since_start =
        prev_time_since_start.as_secs_f32() / CONVEYOR_BELT_MOVEMENT_PERIOD.as_secs_f32();
    let new_conveyor_periods_since_start = delta.as_secs_f32()
        / CONVEYOR_BELT_MOVEMENT_PERIOD.as_secs_f32()
        + prev_conveyor_periods_since_start;

    new_conveyor_periods_since_start.floor() > prev_conveyor_periods_since_start.floor()
}

#[derive(Clone, Debug, Default)]
pub struct Blocks {
    pub upgrades: HashMap<WorldSquare, Upgrade>,
    pub blocks: HashSet<WorldSquare>,
    pub conveyor_belts: HashMap<WorldSquare, OrthogonalWorldStep>,
}

impl Blocks {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn place_block(&mut self, square: WorldSquare) {
        self.blocks.insert(square);
    }

    pub fn is_block_at(&self, square: WorldSquare) -> bool {
        self.blocks.contains(&square)
    }

    pub fn place_conveyor_belt(&mut self, square: WorldSquare, dir: WorldStep) {
        self.conveyor_belts.insert(square, dir.into());
    }

    pub fn place_upgrade(&mut self, upgrade_type: Upgrade, square: WorldSquare) {
        self.upgrades.insert(square, upgrade_type);
    }

    pub fn is_upgrade_at(&self, square: WorldSquare) -> bool {
        self.upgrades.contains_key(&square)
    }
}
