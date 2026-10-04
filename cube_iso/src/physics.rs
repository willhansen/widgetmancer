//! Player state and the three swappable control/physics schemes.
//!
//! All three share one continuous `(x, y, z)` position; they differ only in
//! how intent and gravity are applied:
//!
//! 1. [`PhysicsMode::SmoothRealtime`] — continuous horizontal velocity, gravity
//!    on the realtime clock, and (visually) a single-character-wide body.
//! 2. [`PhysicsMode::GridRealtimeGravity`] — integer horizontal steps, gravity
//!    still integrated on the realtime clock.
//! 3. [`PhysicsMode::GridMoveGated`] (default) — fully discrete: gravity is
//!    resolved immediately after an intentional move and the resolved fall is
//!    recorded as a [`Player::trail`] of altitudes to draw.

use crate::world::World;

pub const GRAVITY: f32 = 14.0;
pub const PLAYER_SPEED: f32 = 7.0;
/// Below this altitude in open space the player is considered lost and respawns.
pub const FALL_LIMIT: f32 = -12.0;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PhysicsMode {
    SmoothRealtime,
    GridRealtimeGravity,
    GridMoveGated,
}

impl PhysicsMode {
    pub fn label(self) -> &'static str {
        match self {
            PhysicsMode::SmoothRealtime => "1 smooth + realtime gravity",
            PhysicsMode::GridRealtimeGravity => "2 grid + realtime gravity",
            PhysicsMode::GridMoveGated => "3 grid + move-gated gravity",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Intent {
    North,
    South,
    East,
    West,
}

impl Intent {
    /// `(dx, dy)` in world squares. North is `+y`, east is `+x`.
    pub fn delta(self) -> (i32, i32) {
        match self {
            Intent::North => (0, 1),
            Intent::South => (0, -1),
            Intent::East => (1, 0),
            Intent::West => (-1, 0),
        }
    }
}

/// One altitude step recorded while falling under move-gated gravity.
pub type TrailPoint = (f32, f32, f32);

#[derive(Clone, Debug)]
pub struct Player {
    pub x: f32,
    pub y: f32,
    pub z: f32,
    pub vx: f32,
    pub vy: f32,
    pub vz: f32,
    pub mode: PhysicsMode,
    pub trail: Vec<TrailPoint>,
    /// Set when the player has fallen past [`FALL_LIMIT`] into open space and
    /// is waiting to be respawned; the fall trail is kept so it stays visible.
    pub lost: bool,
}

impl Player {
    pub fn new(mode: PhysicsMode) -> Self {
        let mut player = Player {
            x: 0.0,
            y: 0.0,
            z: 0.0,
            vx: 0.0,
            vy: 0.0,
            vz: 0.0,
            mode,
            trail: Vec::new(),
            lost: false,
        };
        player.respawn();
        player
    }

    /// Top surface under the player's rounded column, if any.
    pub fn support(&self, world: &World) -> Option<i32> {
        world.top_height(self.x.round() as i32, self.y.round() as i32)
    }

    pub fn is_smooth(&self) -> bool {
        self.mode == PhysicsMode::SmoothRealtime
    }

    pub fn respawn(&mut self) {
        let half = (crate::world::CUBE_SIZE as f32 - 1.0) / 2.0;
        self.x = half;
        self.y = half;
        self.z = crate::world::CUBE_HEIGHT as f32;
        self.vx = 0.0;
        self.vy = 0.0;
        self.vz = 0.0;
        self.trail.clear();
        self.lost = false;
    }

    pub fn set_mode(&mut self, mode: PhysicsMode) {
        self.mode = mode;
        self.vx = 0.0;
        self.vy = 0.0;
        self.vz = 0.0;
        self.trail.clear();
        self.lost = false;
    }

    pub fn apply_intent(&mut self, world: &World, intent: Intent) {
        if self.lost {
            return;
        }
        let (dx, dy) = intent.delta();
        if dx == 0 && dy == 0 {
            return;
        }
        match self.mode {
            PhysicsMode::SmoothRealtime => {
                self.vx = dx as f32 * PLAYER_SPEED;
                self.vy = dy as f32 * PLAYER_SPEED;
            }
            PhysicsMode::GridRealtimeGravity | PhysicsMode::GridMoveGated => {
                self.x += dx as f32;
                self.y += dy as f32;
                if self.mode == PhysicsMode::GridMoveGated {
                    self.resolve_gated_gravity(world);
                }
            }
        }
    }

    pub fn tick(&mut self, world: &World, dt: f32) {
        match self.mode {
            PhysicsMode::GridMoveGated => {}
            PhysicsMode::SmoothRealtime => {
                self.x += self.vx * dt;
                self.y += self.vy * dt;
                let damp = (1.0 - 8.0 * dt).max(0.0);
                self.vx *= damp;
                self.vy *= damp;
                self.integrate_fall(world, dt);
            }
            PhysicsMode::GridRealtimeGravity => self.integrate_fall(world, dt),
        }
    }

    fn integrate_fall(&mut self, world: &World, dt: f32) {
        self.vz -= GRAVITY * dt;
        self.z += self.vz * dt;
        match self.support(world) {
            Some(surface) => {
                if self.z <= surface as f32 {
                    self.z = surface as f32;
                    self.vz = 0.0;
                }
            }
            None => {
                if self.z <= FALL_LIMIT {
                    self.z = FALL_LIMIT;
                    self.vz = 0.0;
                    self.lost = true;
                }
            }
        }
    }

    /// Mode 3: after an intentional step, drop one altitude step at a time
    /// until supported, recording the path so the renderer can draw the fall.
    fn resolve_gated_gravity(&mut self, world: &World) {
        self.trail.clear();
        loop {
            match self.support(world) {
                Some(surface) if self.z > surface as f32 => {
                    self.trail.push((self.x, self.y, self.z));
                    self.z -= 1.0;
                }
                Some(surface) => {
                    self.z = surface as f32;
                    break;
                }
                None => {
                    if self.z <= FALL_LIMIT {
                        self.lost = true;
                        break;
                    }
                    self.trail.push((self.x, self.y, self.z));
                    self.z -= 1.0;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn starts_on_top_of_a_cube() {
        let world = World::four_cubes();
        let player = Player::new(PhysicsMode::GridMoveGated);
        assert_eq!(player.support(&world), Some(crate::world::CUBE_HEIGHT));
        assert_eq!(player.z, crate::world::CUBE_HEIGHT as f32);
    }

    #[test]
    fn move_gated_gravity_resolves_and_records_a_trail_when_leaving_the_edge() {
        let world = World::four_cubes();
        let mut player = Player::new(PhysicsMode::GridMoveGated);
        // The cube occupies x in 0..CUBE_SIZE; stepping west repeatedly reaches
        // the edge and then the void.
        for _ in 0..crate::world::CUBE_SIZE {
            player.apply_intent(&world, Intent::West);
        }
        assert_eq!(
            player.support(&world),
            None,
            "should have stepped into void"
        );
        assert!(player.z < crate::world::CUBE_HEIGHT as f32);
        assert!(
            !player.trail.is_empty(),
            "fall should leave a visible trail"
        );
    }

    #[test]
    fn grid_realtime_gravity_lands_on_the_surface() {
        let world = World::four_cubes();
        let mut player = Player::new(PhysicsMode::GridRealtimeGravity);
        player.z = crate::world::CUBE_HEIGHT as f32 + 5.0;
        for _ in 0..200 {
            player.tick(&world, 1.0 / 60.0);
        }
        assert_eq!(player.z, crate::world::CUBE_HEIGHT as f32);
        assert_eq!(player.vz, 0.0);
    }

    #[test]
    fn falling_into_void_marks_lost_and_respawn_recovers() {
        let world = World::four_cubes();
        let mut player = Player::new(PhysicsMode::GridRealtimeGravity);
        // Put the player in the gap column and let gravity run.
        let gap = crate::world::CUBE_SIZE + crate::world::CUBE_GAP / 2;
        player.x = gap as f32;
        player.y = gap as f32;
        player.z = crate::world::CUBE_HEIGHT as f32;
        for _ in 0..600 {
            player.tick(&world, 1.0 / 60.0);
        }
        assert!(
            player.lost,
            "falling into open space should mark the player lost"
        );
        assert_eq!(player.z, FALL_LIMIT);
        player.respawn();
        assert!(!player.lost);
        assert_eq!(player.support(&world), Some(crate::world::CUBE_HEIGHT));
    }
}
