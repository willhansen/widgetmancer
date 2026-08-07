//! Combat: player attacks, captures, and upgrades, extracted from the
//! `game` god module (ROADMAP.md item 1, step 7). Implemented as an
//! `impl Game` block in a submodule; the public `Game` API is unchanged.

use euclid::{vec2, Angle};

use crate::piece::PieceType::*;
use crate::piece::Upgrade::BlinkRange;
use crate::piece::*;
use crate::*;

use super::Game;

impl Game {

    pub fn do_player_radial_attack(&mut self) {
        assert!(self.player_is_alive());

        let kill_radius = 1;
        let circle_radius = 1.5;

        (-kill_radius..=kill_radius).for_each(|dx| {
            (-kill_radius..=kill_radius).for_each(|dy| {
                let step: WorldStep = vec2(dx, dy);

                if step.square_length() != 0 {
                    self.try_capture_piece_at(self.player_square() + step).ok();
                }
            })
        });

        self.graphics
            .start_circle_attack_animation(self.player_square(), circle_radius);
    }

    pub fn do_player_spear_attack(&mut self) {
        assert!(self.player_is_alive());

        let spear_length = 5;

        for i in 1..=spear_length {
            if let Ok(target_pose) = self.multiple_portal_aware_steps(self.player_pose(), i) {
                let target_square = target_pose.square();
                if !self.square_is_on_board(target_square) || self.is_block_at(target_square) {
                    break;
                }
                self.try_capture_piece_at(target_square).ok();
            } else {
                break;
            }
        }

        self.graphics.start_spear_attack_animation(
            self.player_square(),
            self.player_faced_direction(),
            spear_length as u32,
        );
    }

    pub fn do_player_shoot_arrow(&mut self) {
        assert!(self.player_is_alive());
        let square_in_front_of_player = self.player_square() + self.player_faced_direction().step();
        if !self.square_is_empty(square_in_front_of_player) {
            return;
        }
        self.place_arrow(square_in_front_of_player, self.player_faced_direction());
    }

    pub fn do_player_shoot_shotgun(&mut self) {
        let num_lasers = 10;
        let range = 5.0;
        let spread_radians = 1.0;
        let random_spread_radius = 1.0;
        for i in 0..num_lasers {
            let line_start: WorldSquare = self.player_square();
            let rotation_if_uniform = lerp(
                -spread_radians / 2.0,
                spread_radians / 2.0,
                i as f32 / num_lasers as f32,
            );
            let line_end: WorldPoint = line_start.to_f32()
                + rotate_vect(
                    self.player_faced_direction().step().to_f32() * range,
                    Angle::radians(rotation_if_uniform),
                )
                .cast_unit()
                + rand_radial_offset(random_spread_radius).cast_unit();
            let line = WorldLine::new(line_start.to_f32(), line_end);

            for square in line.touched_squares() {
                if self.is_non_player_piece_at(square) {
                    self.capture_piece_at(square);
                }
            }

            self.graphics
                .add_simple_laser(line_start.to_f32(), line_end);
        }
        self.graphics
            .start_recoil_animation(self.board_size, self.player_faced_direction().step());
    }

    pub fn do_player_shoot_sniper(&mut self) {
        let graphical_laser_end: WorldSquare;
        if let Some(square) = self.selected_square {
            if self.pieces.contains_key(&square) {
                self.capture_piece_at(square);
            }
            graphical_laser_end = square;
        } else {
            graphical_laser_end = self.player_square() + self.player_faced_direction().step() * 300;
        }
        // laser should start at edge of player square, where player is facing
        let graphical_laser_start =
            self.player_square().to_f32() + self.player_faced_direction().step().to_f32() * 0.5;
        self.graphics
            .add_floaty_laser(graphical_laser_start, graphical_laser_end.to_f32());
    }

    pub fn smite_selected_square(&mut self) {
        assert!(self.player_is_alive());
        if let Some(target_square) = self.selected_square {
            self.smite(target_square);
        }
    }

    fn smite(&mut self, square: WorldSquare) {
        self.try_capture_piece_at(square).ok();
        self.graphics.start_burst_explosion(square.to_f32());
        self.graphics.do_smite_animation(square);
    }

    pub fn apply_upgrade(&mut self, upgrade: Upgrade) {
        assert!(self.player_is_alive());
        match upgrade {
            BlinkRange => {
                self.player().blink_range += 1;
            }
        }
    }

    pub fn capture_piece_at(&mut self, square: WorldSquare) {
        let result = self.try_capture_piece_at(square);
        if let Some(err_str) = result.err() {
            panic!("{}", err_str);
        }
    }

    pub fn try_capture_piece_at(&mut self, square: WorldSquare) -> Result<(), String> {
        if !self.square_is_on_board(square) {
            return Err(format!(
                "Tried to capture piece off board at {}",
                point_to_string(square)
            ));
        }
        if self.try_get_player_square() == Some(square) {
            self.kill_player();
            Ok(())
        } else if let Some(piece) = self.pieces.remove(&square) {
            if piece.piece_type == King {
                self.place_upgrade(BlinkRange, square);
            }

            self.graphics.start_piece_death_animation_at(square);

            Ok(())
        } else {
            Err(format!(
                "Tried to capture an empty square at {}",
                point_to_string(square)
            ))
        }
    }
}
