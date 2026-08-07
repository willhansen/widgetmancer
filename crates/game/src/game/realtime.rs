//! Real-time (`Duration`-based) effects, extracted from the `game` god module
//! (ROADMAP.md item 1, step 7). Implemented as an `impl Game` block in a
//! submodule; private methods are `pub(crate)` where `mod.rs` orchestration
//! or tests still call them.

use std::collections::{HashMap, HashSet};
use std::time::Duration;

use euclid::Angle;
use num::clamp;

use crate::piece::PieceType::*;
use crate::*;

use super::{conveyor_period_just_elapsed, FloatingEntityTrait, Game, GridEntity, DeathCube, FloatingHunterDrone, HUNTER_DRONE_SIGHT_RANGE};

impl Game {

    pub fn tick_realtime_effects(&mut self, delta: Duration) {
        self.tick_death_cubes(delta);
        self.tick_hunter_drones(delta);
        self.tick_realtime_turrets(delta);
        self.tick_conveyor_belts(delta);
        self.world_time += delta;
    }

    pub(crate) fn world_time_since_start(&self) -> Duration {
        self.world_time.duration_since(self.world_start_time)
    }

    pub fn tick_realtime_turrets(&mut self, delta: Duration) {
        let turret_squares: Vec<WorldSquare> = self
            .pieces
            .iter()
            .filter(|(_, piece)| piece.piece_type == DeathCubeTurret)
            .map(|(&square, _)| square)
            .collect();

        let CUBES_PER_SECOND = 1.0;
        let CUBE_SPEED = 4.0;

        let chance_to_fire_this_tick = clamp(CUBES_PER_SECOND * delta.as_secs_f32(), 0.0, 1.0);

        turret_squares.iter().for_each(|square| {
            let should_fire = random_event(chance_to_fire_this_tick);
            if should_fire {
                let direction = random_unit_vector();
                self.place_linear_death_cube(square.to_f32(), (direction * CUBE_SPEED).cast_unit());
            }
        });
    }
    pub fn tick_death_cubes(&mut self, duration: Duration) {
        let mut kill_lines: Vec<(WorldSquare, WorldSquare)> = vec![];
        for cube in &mut self.death_cubes {
            let start_pos = cube.position;
            cube.position += cube.velocity * duration.as_secs_f32();
            let end_pos = cube.position;

            let start_square = world_point_to_world_square(start_pos);
            let end_square = world_point_to_world_square(end_pos);
            kill_lines.push((start_square, end_square));
        }
        kill_lines.iter().for_each(|(start_square, end_square)| {
            self.kill_along_line(*start_square, *end_square);
        });
        self.remove_death_cubes_that_are_off_board();
    }

    fn kill_along_line(&mut self, start_square: WorldSquare, end_square: WorldSquare) {
        let mut kill_squares = HashSet::new();
        for (x, y) in line_drawing::Bresenham::new(start_square.to_tuple(), end_square.to_tuple()) {
            kill_squares.insert(point2(x, y));
        }
        kill_squares.into_iter().for_each(|square| {
            if self
                .get_piece_at(square)
                .is_some_and(|piece| piece.faction != self.death_cube_faction)
            {
                self.capture_piece_at(square);
            } else if self.is_player_at(square) {
                self.capture_piece_at(square);
            }
        });
    }


    fn tick_hunter_drones(&mut self, duration: Duration) {
        self.floating_hunter_drones = self
            .floating_hunter_drones
            .iter()
            .map(|drone: &FloatingHunterDrone| {
                let mut clone_drone = drone.clone();

                let raycast_result = self.raycast(
                    clone_drone.position,
                    clone_drone.sight_direction,
                    HUNTER_DRONE_SIGHT_RANGE,
                );

                let maybe_relative_player_square = raycast_result
                    .grid_entities
                    .iter()
                    .find(|(_rel_square, grid_entity)| matches!(grid_entity, GridEntity::Player))
                    .map(|(rel_square, _grid_entity)| rel_square);

                if let Some(relative_player_square) = maybe_relative_player_square {
                    let drone_position_in_its_square =
                        clone_drone.position - clone_drone.position.round();
                    let vec_to_player_center =
                        relative_player_square.to_f32() - drone_position_in_its_square;
                    clone_drone.velocity =
                        vec_to_player_center.normalize() * clone_drone.velocity.length();
                }

                clone_drone = self.slide_floating_entity_with_portal_awareness(
                    &clone_drone,
                    drone.velocity * duration.as_secs_f32(),
                );
                clone_drone.velocity =
                    self.reflect_off_board_edges(clone_drone.position, clone_drone.velocity);

                clone_drone.sight_direction += Angle::degrees(90.0) * duration.as_secs_f32();
                clone_drone
            })
            .collect();
    }

    pub(crate) fn slide_floating_entity_with_portal_awareness<T: FloatingEntityTrait + Clone>(
        &self,
        floating_entity: &T,
        movement: WorldMove,
    ) -> T {
        // TODO: portal awareness
        let mut clone_drone = floating_entity.clone();
        clone_drone.set_position(clone_drone.position() + movement);
        clone_drone
    }

    pub(crate) fn reflect_off_board_edges(&self, pos: WorldPoint, vel: WorldMove) -> WorldMove {
        let mut out_vel = vel;

        let xmin = -0.5;
        let xmax = self.board_size().width as f32 - 0.5;
        let ymin = -0.5;
        let ymax = self.board_size().height as f32 - 0.5;

        if (pos.x < xmin && vel.x < 0.0) || (pos.x > xmax && vel.x > 0.0) {
            out_vel.x *= -1.0;
        }
        if (pos.y < ymin && vel.y < 0.0) || (pos.y > ymax && vel.y > 0.0) {
            out_vel.y *= -1.0;
        }
        out_vel
    }

    fn tick_conveyor_belts(&mut self, delta: Duration) {
        let just_finished_full_movement_period =
            conveyor_period_just_elapsed(self.world_time_since_start(), delta);

        let push_directions: HashMap<WorldSquare, KingWorldStep> = self
            .blocks
            .conveyor_belts
            .iter()
            .map(|(&start_square, &push_direction)| (start_square, push_direction.into()))
            .collect();
        if just_finished_full_movement_period {
            self.simultaneously_push_several_grid_entities(&push_directions);
        }

        let conveyor_distance = Game::conveyor_belt_speed() * delta.as_secs_f32();
        self.simultaneously_push_floating_entities_at_several_squares(
            &push_directions,
            conveyor_distance,
        );
    }

    pub fn remove_death_cubes_that_are_off_board(&mut self) {
        let cubes_on_board = self
            .death_cubes
            .iter()
            .cloned()
            .filter(|death_cube: &DeathCube| {
                let square = world_point_to_world_square(death_cube.position);
                self.square_is_on_board(square)
            })
            .collect();
        self.death_cubes = cubes_on_board;
    }
}
