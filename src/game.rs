use std::cmp::{max, min, Ordering};
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::io::Write;
use std::ops::Mul;
use std::time::{Duration, Instant};

use ::num::clamp;
use ambassador::{delegatable_trait, delegate_to_methods, Delegate};
use derive_more::{Constructor, From};
use getset::{CopyGetters, Setters};
use itertools::Itertools;
use line_drawing::Point;
use ntest::assert_false;
use ordered_float::OrderedFloat;
use priority_queue::DoublePriorityQueue;
use rand::rngs::StdRng;
use rand::seq::{IteratorRandom, SliceRandom};
use rand::{thread_rng, Rng, SeedableRng};
use rgb::RGB8;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::animations::selector_animation::SelectorAnimation;
use crate::fov_stuff::rasterized_field_of_view::{
    RasterizedFieldOfView, RasterizedFieldOfViewFunctions,
};
use crate::fov_stuff::square_visibility::{
    RelativeSquareVisibilityFunctions, SquareVisibilityFunctions,
};
use crate::fov_stuff::{portal_aware_field_of_view_from_square, square_visibility, FieldOfView};
use crate::glyph::glyph_constants::{
    BLACK, DARK_CYAN, ENEMY_PIECE_COLOR, RED_PAWN_COLOR, SPACE, WHITE,
};
use crate::graphics::drawable::{DrawableEnum, TextDrawable};
use crate::graphics::screen::ScreenBufferStep;
use crate::graphics::Graphics;
use crate::piece::PieceType::*;
use crate::piece::Upgrade::BlinkRange;
use crate::piece::*;
use crate::portal_geometry::PortalGeometry;
use crate::utility::*;
use crate::{lerp, rand_radial_offset, round_to_king_step, Glyph, IPoint, IVector, LEFT_I};

use self::size_2d::Size2D;

const TURNS_TO_SPAWN_PAWN: u32 = 10;
const PLAYER_SIGHT_RADIUS: u32 = 16;

#[delegatable_trait]
pub trait FloatingEntityTrait {
    fn position(&self) -> WorldPoint;
    fn set_position(&mut self, position: WorldPoint);
    fn velocity(&self) -> WorldMove;
    fn set_velocity(&mut self, velocity: WorldMove);
    fn in_square(&self, square: WorldSquare) -> bool {
        world_point_to_world_square(self.position()) == square
    }
}

#[derive(Clone, PartialEq, Debug, Copy, From, Delegate)]
#[delegate(FloatingEntityTrait)]
enum FloatingEntityEnum {
    DeathCube(DeathCube),
    FloatingHunterDrone(FloatingHunterDrone),
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct DeathCube {
    position: WorldPoint,
    velocity: WorldMove,
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
    position: WorldPoint,
    velocity: WorldMove,
    #[getset(get_copy = "pub", set = "pub")]
    sight_direction: Angle<f32>,
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
    pub fn new(position: WorldPoint, velocity: WorldMove, sight_direction: Angle<f32>) -> Self {
        FloatingHunterDrone {
            position,
            velocity,
            sight_direction,
        }
    }
}

pub const CONVEYOR_BELT_MOVEMENT_PERIOD_S: f32 = 2.0;
pub const CONVEYOR_BELT_VISUAL_PERIOD_S: f32 = CONVEYOR_BELT_MOVEMENT_PERIOD_S * 2.0;

#[derive(Clone, Eq, PartialEq, Debug, Copy)]
enum GridEntity {
    Player,
    Widget(Widget),
    Block,
}

#[derive(Clone, Eq, PartialEq, Debug, Copy)]
enum FloorFeature {
    PushArrow(OrthogonalDirection),
    ConveyorBelt(OrthogonalDirection),
}

pub struct Player {
    pub position: WorldSquare,
    pub faced_direction: KingWorldStep,
    pub blink_range: u32,
}

#[derive(Clone, Eq, PartialEq, Debug, Copy)]
pub struct IncubatingPawn {
    pub age_in_turns: u32,
    pub faction: Faction,
}

#[derive(Clone, PartialEq, Debug)]
struct RaycastResult {
    grid_entities: Vec<(WorldStep, GridEntity)>,
    //floating_entities: Vec<FloatingEntity>,
    endpoint: WorldPoint,
    //end_direction: Angle<f32>,
}

#[derive(Clone, Eq, PartialEq, Debug, Copy, CopyGetters)]
#[get_copy = "pub"]
pub struct Widget {
    val: u32,
    character: char,
}

impl Widget {
    pub fn new(val: u32) -> Self {
        let character = char::from_u32(match val {
            0 => 0x24EA,
            1..=20 => 0x2460 - 1 + val,
            _ => panic!("invalid widget value: {}", val),
        })
        .unwrap();
        Widget { val, character }
    }
    pub fn drawable(&self) -> TextDrawable {
        TextDrawable::new(&(self.character.to_string() + " "), BLACK, BLACK, true)
    }
}

pub struct Game {
    board_size: BoardSize,
    // (x,y), left to right, top to bottom
    //step_foes: Vec<StepFoe>,
    running: bool,
    // set false to quit
    player_optional: Option<Player>,
    graphics: Graphics,
    pieces: HashMap<WorldSquare, Piece>,
    upgrades: HashMap<WorldSquare, Upgrade>,
    blocks: HashSet<WorldSquare>,
    widgets: HashMap<WorldSquare, Widget>,
    floor_push_arrows: HashMap<WorldSquare, OrthogonalDirection>,
    conveyor_belts: HashMap<WorldSquare, OrthogonalDirection>,
    turn_count: u32,
    selectors: Vec<SelectorAnimation>,
    selected_square: Option<WorldSquare>,
    incubating_pawns: HashMap<WorldSquare, IncubatingPawn>,
    faction_factory: FactionFactory,
    red_pawn_faction: Faction,
    default_enemy_faction: Faction,
    death_cubes: Vec<DeathCube>,
    death_cube_faction: Faction,
    portal_geometry: PortalGeometry,
    floating_hunter_drones: Vec<FloatingHunterDrone>,
    world_start_time: Instant,
    world_time: Instant,
}

impl Game {
    pub fn new(terminal_width: u16, terminal_height: u16, start_time: Instant) -> Game {
        let board_size = BoardSize::new(terminal_width as u32 / 2, terminal_height as u32);
        let mut game = Game {
            board_size,
            running: true,
            player_optional: None,
            graphics: Graphics::new(terminal_width, terminal_height, start_time),
            pieces: HashMap::new(),
            upgrades: HashMap::new(),
            blocks: HashSet::new(),
            widgets: HashMap::new(),
            floor_push_arrows: HashMap::new(),
            conveyor_belts: HashMap::new(),
            turn_count: 0,
            selectors: vec![],
            selected_square: None,
            incubating_pawns: Default::default(),
            faction_factory: FactionFactory::new(),
            red_pawn_faction: Faction::RedPawn,
            default_enemy_faction: Faction::default(),
            death_cubes: vec![],
            death_cube_faction: Faction::DeathCube,
            portal_geometry: PortalGeometry::default(),
            floating_hunter_drones: vec![],
            world_start_time: Instant::now(),
            world_time: Instant::now(),
        };
        game.default_enemy_faction = game.get_new_faction();
        assert_eq!(game.default_enemy_faction, Faction::default());

        game.graphics.set_empty_board_animation();
        game
    }
    pub fn board_size(&self) -> BoardSize {
        self.board_size
    }

    pub fn player_is_alive(&self) -> bool {
        self.player_optional.is_some()
    }

    pub fn turn_count(&self) -> u32 {
        self.turn_count
    }

    pub fn place_player(&mut self, square: WorldSquare) {
        self.player_optional = Some(Player {
            position: square,
            faced_direction: LEFT_I.cast_unit().into(),
            blink_range: 5,
        });
    }
    pub fn place_player_with_direction(&mut self, square: WorldSquare, direction: WorldStep) {
        self.player_optional = Some(Player {
            position: square,
            faced_direction: KingWorldStep::new(direction),
            blink_range: 5,
        });
    }

    pub fn mid_square(&self) -> WorldSquare {
        point2(
            self.board_size().width() as i32 / 2,
            self.board_size().height() as i32 / 2,
        )
    }

    fn square_is_on_board(&self, pos: WorldSquare) -> bool {
        pos.x >= 0
            && pos.x < self.board_size().width() as i32
            && pos.y >= 0
            && pos.y < self.board_size().height() as i32
    }

    // TODO: test

    fn point_is_on_board(&self, point: WorldPoint) -> bool {
        point.x >= -0.5
            && point.x < self.board_size().width() as f32 - 0.5
            && point.y >= -0.5
            && point.y < self.board_size().height() as f32 - 0.5
    }

    pub fn quit(&mut self) {
        self.running = false;
    }
    pub fn running(&self) -> bool {
        self.running
    }

    pub fn try_slide_player(&mut self, movement: WorldStep) -> Result<(), ()> {
        assert!(movement.is_orthodiagonal());
        let movement_direction = round_to_king_step(movement);
        let movement_length = king_step_distance(movement);
        self.try_slide_player_by_direction(movement_direction.into(), movement_length)
    }

    pub fn try_slide_player_relative_to_screen(
        &mut self,
        screen_step: ScreenBufferStep,
    ) -> Result<(), ()> {
        let world_step = self.graphics.screen.screen_step_to_world_step(screen_step);
        self.try_slide_player(world_step)
    }

    pub fn try_slide_player_by_direction(
        &mut self,
        direction: KingWorldStep,
        num_squares: u32,
    ) -> Result<(), ()> {
        let (new_pos, new_dir) = self
            .multiple_portal_aware_steps(
                SquareWithKingDir::new(self.player_square(), direction),
                num_squares,
            )?
            .tuple();
        //self.raw_set_player_faced_direction(round_to_king_step(movement));
        self.raw_set_player_faced_direction(new_dir);
        let new_square_is_threatened = self
            .squares_threatened_by_any_piece(false)
            .contains(&new_pos);
        if new_square_is_threatened {
            return Err(());
        }

        let widget_is_at_destination = self.widgets.contains_key(&new_pos);
        if num_squares == 1 && widget_is_at_destination {
            self.try_push_grid_entity(new_pos, new_dir)?;
        }

        self.try_set_player_position(new_pos)?;

        let rotation_from_portals =
            NormalizedOrthoAngle::from_start_and_end_directions(direction.step(), new_dir.step());
        self.graphics.screen.rotate(rotation_from_portals);

        Ok(())
    }

    fn get_grid_entity_at_square(&self, square: WorldSquare) -> Option<GridEntity> {
        if self.try_get_player_square() == Some(square) {
            Some(GridEntity::Player)
        } else if self.blocks.contains(&square) {
            Some(GridEntity::Block)
        } else if let Some(&widget) = self.widgets.get(&square) {
            Some(GridEntity::Widget(widget))
        } else {
            None
        }
    }
    fn square_has_grid_entity(&self, square: WorldSquare) -> bool {
        self.get_grid_entity_at_square(square).is_some()
    }

    fn try_push_grid_entity(
        &mut self,
        start_square: WorldSquare,
        push_direction: KingWorldStep,
    ) -> Result<SquareWithKingDir, ()> {
        let pushee = self.get_grid_entity_at_square(start_square);
        if pushee.is_none() || pushee == Some(GridEntity::Block) {
            return Err(());
        }
        let end_pose =
            self.portal_aware_single_step(SquareWithKingDir::new(start_square, push_direction))?;
        let (end_square, end_dir) = end_pose.tuple();
        if self.square_has_grid_entity(end_square) {
            self.try_push_grid_entity(end_square, end_dir)?;
        }

        // do the movement
        match pushee.unwrap() {
            GridEntity::Player => self.try_slide_player(push_direction.step())?,
            GridEntity::Widget(_) => self.move_widget(start_square, end_square),
            GridEntity::Block => panic!("Can't push a block at: {:?}", start_square),
        };
        Ok(end_pose)
    }

    fn move_widget(&mut self, start: WorldSquare, end: WorldSquare) {
        assert!(self.widgets.contains_key(&start));
        assert!(!self.widgets.contains_key(&end));

        let widget = self.widgets.remove(&start).unwrap();
        self.widgets.insert(end, widget);
    }

    fn push_floating_entities_that_are_in_square_in_king_direction(
        &mut self,
        start_square: WorldSquare,
        push_direction: KingWorldStep,
        push_length: f32,
    ) {
        let floating_entities_at_start: Vec<FloatingEntityEnum> =
            self.take_floating_entities_from_square(start_square);
        let mut moved_drones = floating_entities_at_start
            .iter()
            .map(|e| {
                self.slide_floating_entity_with_portal_awareness(
                    e,
                    push_direction.step().to_f32() * push_length,
                )
            })
            .collect_vec();
        moved_drones
            .into_iter()
            .for_each(
                |floating_entity: FloatingEntityEnum| match floating_entity {
                    FloatingEntityEnum::DeathCube(e) => self.death_cubes.push(e),
                    FloatingEntityEnum::FloatingHunterDrone(e) => {
                        self.floating_hunter_drones.push(e)
                    }
                },
            );
    }

    fn take_floating_entities_from_square(
        &mut self,
        square: WorldSquare,
    ) -> Vec<FloatingEntityEnum> {
        let hunter_drones_from_square = self
            .floating_hunter_drones
            .iter()
            .filter(|drone| drone.in_square(square))
            .map(|drone| FloatingEntityEnum::FloatingHunterDrone(*drone))
            .collect_vec();
        let death_cubes_from_square = self
            .death_cubes
            .iter()
            .filter(|cube| world_point_to_world_square(cube.position) == square)
            .map(|cube| FloatingEntityEnum::DeathCube(*cube))
            .collect_vec();

        // and delete
        self.floating_hunter_drones.retain(|f| !f.in_square(square));
        self.death_cubes.retain(|f| !f.in_square(square));

        [hunter_drones_from_square, death_cubes_from_square].concat()
    }

    pub fn move_player_to(&mut self, square: WorldSquare) {
        self.try_set_player_position(square)
            .expect(&("failed move player to ".to_owned() + &square.to_string()));
    }

    fn floor_color_at_square(&self, square: WorldSquare) -> RGB8 {
        self.graphics.floor_color_at_square(square)
    }

    pub fn player_blink_relative_to_screen(&mut self, screen_step: ScreenBufferStep) {
        let world_step = self.graphics.screen.screen_step_to_world_step(screen_step);
        self.player_blink(world_step);
    }

    pub fn player_blink(&mut self, direction: WorldStep) {
        assert!(direction.is_king_step());
        let start_square = self.player_square();
        let blink_range = self.player().blink_range as i32;
        let ideal_end_square = start_square + direction * blink_range;

        let mut candidate_square = start_square;
        for (x, y) in
            line_drawing::Bresenham::new(start_square.to_tuple(), ideal_end_square.to_tuple())
        {
            let next_square = point2(x, y);
            if next_square == start_square {
                continue;
            }
            if !self.square_is_on_board(next_square) || !self.square_is_empty(next_square) {
                break;
            }
            candidate_square = next_square;
        }
        if candidate_square == start_square {
            return;
        }
        self.move_player_to(candidate_square);
        self.graphics
            .do_blink_animation(start_square, candidate_square);
    }

    pub fn try_get_player_square(&self) -> Option<WorldSquare> {
        if let Some(player) = &self.player_optional {
            Some(player.position)
        } else {
            None
        }
    }
    pub fn player_square(&self) -> WorldSquare {
        if let Some(square) = self.try_get_player_square() {
            square
        } else {
            panic!("player is dead")
        }
    }

    pub fn arrows(&self) -> HashMap<WorldSquare, KingWorldStep> {
        self.pieces
            .iter()
            .filter(|(&square, &piece)| piece.piece_type == Arrow)
            .map(|(&square, &piece)| (square, piece.faced_direction()))
            .collect()
    }

    pub fn player(&mut self) -> &mut Player {
        self.player_optional.as_mut().unwrap()
    }

    pub fn try_set_player_position(&mut self, square: WorldSquare) -> Result<(), ()> {
        if self.is_non_player_piece_at(square) {
            self.capture_piece_at(square);
        }

        if !self.square_is_on_board(square) || self.is_block_at(square) {
            return Err(());
        }

        if let Some(&upgrade) = self.upgrades.get(&square) {
            self.apply_upgrade(upgrade);
            self.upgrades.remove(&square);
        }

        self.raw_set_player_position(square);

        Ok(())
    }

    fn raw_set_player_position(&mut self, square: WorldSquare) {
        if let Some(player) = &mut self.player_optional {
            player.position = square
        } else {
            panic!("Player is too dead to move")
        }
    }

    pub fn player_faced_direction(&self) -> KingWorldStep {
        if let Some(player) = &self.player_optional {
            player.faced_direction
        } else {
            panic!("player is dead")
        }
    }
    pub fn player_faced_direction_on_screen(&self) -> ScreenBufferStep {
        if let Some(player) = &self.player_optional {
            self.graphics
                .screen
                .world_step_to_screen_step(player.faced_direction.into())
        } else {
            panic!("player is dead")
        }
    }

    pub fn player_pose(&self) -> SquareWithKingDir {
        SquareWithKingDir::new(self.player_square(), self.player_faced_direction())
    }

    pub fn raw_set_player_faced_direction(&mut self, new_dir: KingWorldStep) {
        if let Some(player) = &mut self.player_optional {
            player.faced_direction = new_dir
        } else {
            panic!("Player is too dead to rotate")
        }
    }

    pub fn borrow_graphics_mut(&mut self) -> &mut Graphics {
        &mut self.graphics
    }
    pub fn graphics(&self) -> &Graphics {
        &self.graphics
    }
    pub fn pieces(&mut self) -> &mut HashMap<WorldSquare, Piece> {
        &mut self.pieces
    }

    fn find_pieces(&self, target_piece: Piece) -> SquareSet {
        self.pieces
            .iter()
            .filter(|(&square, &piece)| piece == target_piece)
            .map(|(&square, &piece)| square)
            .collect()
    }

    pub fn draw_headless_at_duration_from_start(&mut self, delta: Duration) {
        let draw_time = self.graphics.start_time() + delta;
        self.draw(&mut None, draw_time);
    }
    pub fn draw_headless_now(&mut self) {
        self.draw(&mut None, Instant::now());
    }

    pub fn draw(&mut self, mut writer: &mut Option<Box<dyn Write>>, time: Instant) {
        self.populate_draw_buffer(time);
        self.update_screen_from_draw_buffer(&mut writer);
    }

    pub fn populate_draw_buffer(&mut self, time: Instant) {
        self.graphics.clear_draw_buffer();
        self.graphics.draw_static_board(self.board_size);
        self.graphics.draw_board_animation(time);

        // TODO: fix redundant calculation
        // TODO: make redundant calculation actually produce the same path every time
        //if let Some(player_square) = self.try_get_player_square() {
        //let king_squares = self.find_pieces(Piece::king());
        //let king_paths = king_squares .iter() .filter_map(|&king_square| self.find_king_path(king_square, player_square)) .collect();
        //self.graphics.draw_paths(king_paths);
        //}
        self.graphics
            .draw_floor_push_arrows(&self.floor_push_arrows);

        let global_phase_offset: f32 =
            self.world_time_since_start().as_secs_f32() / CONVEYOR_BELT_VISUAL_PERIOD_S;
        self.graphics
            .draw_conveyor_belts(&self.conveyor_belts, global_phase_offset);

        self.graphics.draw_move_marker_squares(
            self.move_squares_for_all_pieces(false),
            self.squares_threatened_by_any_piece(false),
            self.move_squares_for_all_pieces(true),
            self.squares_threatened_by_any_piece(true),
        );

        self.graphics.draw_blocks(&self.blocks);
        for (&square, &piece) in &self.pieces {
            if piece.piece_type == Arrow {
                self.graphics.draw_arrow(square, piece.faced_direction());
                continue;
            }
            let color = if piece.faction == self.red_pawn_faction {
                RED_PAWN_COLOR
            } else {
                ENEMY_PIECE_COLOR
            };
            self.graphics
                .draw_piece_with_color(square, piece.piece_type, color)
        }
        self.upgrades
            .iter()
            .for_each(|(&square, &upgrade)| self.graphics.draw_upgrade(square, upgrade));
        self.death_cubes
            .iter()
            .for_each(|death_cube| self.graphics.draw_death_cube(*death_cube));
        self.floating_hunter_drones.iter().for_each(|drone| {
            let sight_line_segments = self.portal_geometry.ray_to_naive_line_segments(
                drone.position,
                drone.sight_direction,
                HUNTER_DRONE_SIGHT_RANGE,
            );
            self.graphics
                .draw_floating_hunter_drone(drone, &sight_line_segments);
        });
        self.widgets.iter().for_each(|(&square, pushable)| {
            self.graphics
                .draw_drawable_to_draw_buffer(square, &pushable.drawable())
        });
        self.graphics.remove_finished_animations(time);
        self.graphics.draw_non_board_animations(time);
        if self.player_is_alive() {
            self.graphics
                .draw_player(self.player_square(), self.player_faced_direction());
        }
    }

    pub fn update_screen_from_draw_buffer_headless(&mut self) {
        self.update_screen_from_draw_buffer(&mut None);
    }

    fn update_screen_from_draw_buffer(&mut self, mut writer: &mut Option<Box<dyn Write>>) {
        self.graphics.screen.fill_screen_buffer(BLACK);
        if self.player_is_alive() {
            self.graphics
                .screen
                .set_screen_center_by_world_square(self.player_square());
            self.graphics
                .load_screen_buffer_from_fov(&self.rasterized_player_field_of_view());
        } else {
            self.graphics
                .load_screen_buffer_from_absolute_positions_in_draw_buffer();
        }

        self.graphics.display(&mut writer);
    }

    fn is_player_at(&self, square: WorldSquare) -> bool {
        self.player_is_alive() && self.try_get_player_square() == Some(square)
    }

    fn square_is_empty(&self, square: WorldSquare) -> bool {
        !self.is_player_at(square)
            && !self.is_non_player_piece_at(square)
            && !self.is_block_at(square)
            && !self.is_upgrade_at(square)
    }

    pub fn place_new_king_pawn_faction(&mut self, king_square: WorldSquare) {
        let faction = self.get_new_faction();
        self.place_piece(Piece::new(King, faction), king_square);
        for x in -1..=1 {
            for y in -1..=1 {
                let pawn_square = king_square + vec2(x, y);
                if pawn_square == king_square {
                    continue;
                }
                self.place_piece(Piece::new(OmniDirectionalPawn, faction), pawn_square);
            }
        }
    }

    pub fn place_random_3x3_faction(&mut self, king_square: WorldSquare) {
        let faction = self.get_new_faction();
        self.place_piece(Piece::new(King, faction), king_square);
        for x in -1..=1 {
            for y in -1..=1 {
                let square = king_square + vec2(x, y);
                if square == king_square {
                    continue;
                }
                self.place_piece(
                    Piece::new(Piece::random_subordinate_type(), faction),
                    square,
                );
            }
        }
    }

    pub fn place_linear_death_cube(&mut self, position: WorldPoint, velocity: WorldMove) {
        self.death_cubes.push(DeathCube { position, velocity });
    }

    pub fn tick_realtime_effects(&mut self, delta: Duration) {
        self.tick_death_cubes(delta);
        self.tick_hunter_drones(delta);
        self.tick_realtime_turrets(delta);
        self.tick_conveyor_belts(delta);
        self.world_time += delta;
    }

    fn world_time_since_start(&self) -> Duration {
        self.world_time.duration_since(self.world_start_time)
    }

    pub fn tick_realtime_turrets(&mut self, delta: Duration) {
        let turret_squares: Vec<WorldSquare> = self
            .pieces
            .iter()
            .filter(|(_, piece)| piece.piece_type == DeathCubeTurret)
            .map(|(&square, _)| square)
            .collect();

        let CUBES_PER_SECOND = 10.0;
        let CUBE_SPEED = 5.0;

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
            let maybe_piece = self.get_piece_at(square);
            if maybe_piece.is_some() && maybe_piece.unwrap().faction != self.death_cube_faction {
                self.capture_piece_at(square);
            } else if self.is_player_at(square) {
                self.capture_piece_at(square);
            }
        });
    }

    fn raycast(&self, start_point: WorldPoint, direction: Angle<f32>, range: f32) -> RaycastResult {
        let naive_line =
            TwoDifferentWorldPoints::new_from_point_and_radial(start_point, direction, range);

        let line_segments_after_portal_awareness: Vec<TwoDifferentWorldPoints> = self
            .portal_geometry
            .ray_to_naive_line_segments(start_point, direction, range);

        let mut result = RaycastResult {
            grid_entities: vec![],
            endpoint: line_segments_after_portal_awareness.last().unwrap().p2(),
        };

        let squares_on_ray_path = line_segments_after_portal_awareness
            .iter()
            .flat_map(|line| line.touched_squares())
            .collect_vec();

        let start_square = world_point_to_world_square(start_point);

        let relative_squares_on_naive_line = naive_line
            .touched_squares()
            .into_iter()
            .map(|square| square - start_square)
            .collect_vec();

        assert_eq!(
            squares_on_ray_path.len(),
            relative_squares_on_naive_line.len(),
            "RAYCAST SQUARE MISCOUNT\n\
            real path: {:?}\n\
            naive_path: {:?}",
            squares_on_ray_path
                .iter()
                .cloned()
                .map(|x| x.to_string())
                .collect_vec(),
            relative_squares_on_naive_line
                .iter()
                .cloned()
                .map(|x| x.to_string())
                .collect_vec()
        );

        result.grid_entities = squares_on_ray_path
            .iter()
            .zip(relative_squares_on_naive_line.iter())
            .filter_map(|(&square, &rel_square)| {
                self.get_grid_entity_at_square(square)
                    .map(|entity| (rel_square, entity))
            })
            .collect_vec();
        result
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

    fn slide_floating_entity_with_portal_awareness<T: FloatingEntityTrait + Clone>(
        &self,
        floating_entity: &T,
        movement: WorldMove,
    ) -> T {
        // TODO: portal awareness
        let mut clone_drone = floating_entity.clone();
        clone_drone.set_position(clone_drone.position() + movement);
        clone_drone
    }

    fn reflect_off_board_edges(&self, pos: WorldPoint, vel: WorldMove) -> WorldMove {
        let mut out_vel = vel;

        let xmin = -0.5;
        let xmax = self.board_size().width() as f32 - 0.5;
        let ymin = -0.5;
        let ymax = self.board_size().height() as f32 - 0.5;

        if (pos.x < xmin && vel.x < 0.0) || (pos.x > xmax && vel.x > 0.0) {
            out_vel.x *= -1.0;
        }
        if (pos.y < ymin && vel.y < 0.0) || (pos.y > ymax && vel.y > 0.0) {
            out_vel.y *= -1.0;
        }
        out_vel
    }

    fn drain_arrows(&mut self) -> HashMap<WorldSquare, KingWorldStep> {
        let old_arrows = self.arrows();
        old_arrows.iter().for_each(|(square, _)| {
            self.pieces.remove(&square);
        });
        old_arrows
    }

    fn set_arrows(&mut self, new_arrows: HashMap<WorldSquare, KingWorldStep>) {
        new_arrows.into_iter().for_each(|(square, dir)| {
            self.pieces.insert(square, Piece::arrow(dir));
        });
    }

    pub fn tick_game_logic(&mut self) {
        self.move_non_arrow_factions();
        self.tick_projectile_arrows();
        self.tick_floor_push_arrows();

        self.on_turn_end();
    }

    fn tick_floor_push_arrows(&mut self) {
        let push_directions: HashMap<WorldSquare, KingWorldStep> = self
            .floor_push_arrows
            .iter()
            .map(|(&start_square, &push_direction)| (start_square, push_direction.into()))
            .collect();
        self.simultaneously_push_several_grid_entities(&push_directions);
        self.simultaneously_push_floating_entities_at_several_squares(&push_directions, 1.0);
    }

    fn tick_conveyor_belts(&mut self, delta: Duration) {
        let prev_conveyor_periods_since_start =
            self.world_time_since_start().as_secs_f32() / CONVEYOR_BELT_MOVEMENT_PERIOD_S;
        let new_conveyor_periods_since_start = delta.as_secs_f32()
            / CONVEYOR_BELT_MOVEMENT_PERIOD_S
            + prev_conveyor_periods_since_start;

        let just_finished_full_movement_period =
            new_conveyor_periods_since_start.floor() > prev_conveyor_periods_since_start.floor();

        let push_directions: HashMap<WorldSquare, KingWorldStep> = self
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

    fn simultaneously_push_several_grid_entities(
        &mut self,
        push_directions: &HashMap<WorldSquare, KingWorldStep>,
    ) {
        let mut push_end_squares = HashSet::new();
        push_directions
            .iter()
            .for_each(|(&start_square, &push_direction)| {
                let already_pushed_something_to_here = push_end_squares.contains(&start_square);
                if !already_pushed_something_to_here {
                    let push_end_pose = self.portal_aware_single_step(SquareWithKingDir::new(
                        start_square,
                        push_direction.into(),
                    ));
                    if let Ok((end_square, end_dir)) = push_end_pose.map(|x| x.tuple()) {
                        self.try_push_grid_entity(start_square, push_direction.into())
                            .ok();
                        push_end_squares.insert(end_square);
                    }
                }
            });
    }
    fn simultaneously_push_floating_entities_at_several_squares(
        &mut self,
        push_directions: &HashMap<WorldSquare, KingWorldStep>,
        push_distance: f32,
    ) {
        let mut push_end_squares = HashSet::new();
        push_directions
            .clone()
            .iter()
            .for_each(|(&start_square, &push_direction)| {
                let already_pushed_something_to_here = push_end_squares.contains(&start_square);
                if !already_pushed_something_to_here {
                    let push_end_pose = self.portal_aware_single_step(SquareWithKingDir::new(
                        start_square,
                        push_direction.into(),
                    ));
                    if let Ok((end_square, end_dir)) = push_end_pose.map(|x| x.tuple()) {
                        self.push_floating_entities_that_are_in_square_in_king_direction(
                            start_square,
                            push_direction.into(),
                            push_distance,
                        );
                        push_end_squares.insert(end_square);
                    }
                }
            });
    }

    pub fn tick_projectile_arrows(&mut self) {
        let old_arrows = self.drain_arrows();

        // arrows that hit arrows, blocks, or board edges disappear
        let mut next_arrows = HashMap::<WorldSquare, KingWorldStep>::new();
        let mut arrow_midair_collisions = SquareSet::new();
        let mut capture_squares = SquareSet::new();
        old_arrows
            .iter()
            .for_each(|(&square, &dir): (&WorldSquare, &KingWorldStep)| {
                if let Ok(next_pose) =
                    self.portal_aware_single_step(SquareWithKingDir::new(square, dir))
                {
                    let (next_square, next_dir) = next_pose.tuple();
                    if self.is_piece_at(next_square) {
                        capture_squares.insert(next_square);
                    }
                    if !self.is_block_at(next_square)
                        && self.square_is_on_board(next_square)
                        && !arrow_midair_collisions.contains(&next_square)
                    {
                        let is_new_midair_collision = next_arrows.contains_key(&next_square);
                        if is_new_midair_collision {
                            next_arrows.remove(&next_square);
                            arrow_midair_collisions.insert(next_square);
                        } else {
                            next_arrows.insert(next_square, next_dir);
                        }
                    }
                }
            });
        // apply captures
        capture_squares.into_iter().for_each(|square| {
            self.try_capture_piece_at(square).ok();
        });

        self.set_arrows(next_arrows);
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

    pub fn place_piece(&mut self, piece: Piece, square: WorldSquare) {
        if !self.square_is_on_board(square) {
            panic!("Tried to place piece off board at {}", square.to_string());
        }
        if !self.square_is_empty(square) {
            panic!("Tried to overwrite piece at {}", square.to_string());
        }
        self.pieces.insert(square, piece);
    }

    pub fn place_red_pawn(&mut self, square: WorldSquare) {
        self.place_piece(
            Piece::new(OmniDirectionalPawn, self.red_pawn_faction),
            square,
        )
    }

    pub fn place_death_turret(&mut self, square: WorldSquare) {
        self.place_piece(Piece::new(DeathCubeTurret, self.death_cube_faction), square);
    }

    pub fn place_floating_hunter_drone(
        &mut self,
        point: WorldPoint,
        velocity: WorldMove,
        sight_angle: Angle<f32>,
    ) {
        self.floating_hunter_drones
            .push(FloatingHunterDrone::new(point, velocity, sight_angle));
    }

    pub fn place_upgrade(&mut self, upgrade_type: Upgrade, square: WorldSquare) {
        assert!(self.square_is_empty(square));
        self.upgrades.insert(square, upgrade_type);
    }

    pub fn tick_pawn_incubation(&mut self) {
        let found_incubation_squares: SquareSet =
            self.empty_squares_surrounded_by_pawns_of_one_faction();

        self.incubating_pawns
            .retain(|old_square, _| found_incubation_squares.contains(old_square));

        for square in found_incubation_squares {
            let faction = self.get_piece_at(square + STEP_UP).unwrap().faction;
            let maybe_incubation = self.incubating_pawns.get_mut(&square);
            if maybe_incubation
                .as_ref()
                .is_some_and(|existing_incubation| existing_incubation.faction == faction)
            {
                let existing_incubation = maybe_incubation.unwrap();
                existing_incubation.age_in_turns += 1;
                if existing_incubation.age_in_turns >= TURNS_TO_SPAWN_PAWN {
                    self.place_piece(Piece::new(OmniDirectionalPawn, faction), square);
                }
            } else {
                let new_incubation = IncubatingPawn {
                    age_in_turns: 0,
                    faction,
                };
                self.incubating_pawns.insert(square, new_incubation);
            }
        }
    }

    pub fn empty_squares_surrounded_by_pawns_of_one_faction(&self) -> SquareSet {
        let mut pawn_adjacency_counter = HashMap::<(WorldSquare, Faction), u32>::new();
        self.pieces
            .iter()
            .cartesian_product(ORTHOGONAL_STEPS)
            .map(|((&pawn_square, piece), orthogonal_step)| (pawn_square + orthogonal_step, piece))
            .filter(|(adjacent_square, _)| self.square_is_empty(*adjacent_square))
            .for_each(|(adjacent_square, piece)| {
                *pawn_adjacency_counter
                    .entry((adjacent_square, piece.faction))
                    .or_insert(0) += 1;
            });
        pawn_adjacency_counter
            .into_iter()
            .filter(|(_, count)| *count == 4)
            .map(|((square, _), _)| square)
            .collect()
    }

    pub fn random_empty_square(&self, rng: &mut StdRng) -> Result<WorldSquare, ()> {
        let num_attempts = 40;
        for _ in 0..num_attempts {
            let rand_pos = WorldSquare::new(
                rng.gen_range(0..self.board_size().width() as i32),
                rng.gen_range(0..self.board_size().height() as i32),
            );
            if self.square_is_empty(rand_pos) {
                return Ok(rand_pos);
            }
        }
        Err(())
    }

    pub fn place_piece_randomly(&mut self, piece: Piece, rng: &mut StdRng) -> WorldSquare {
        let rand_pos = self
            .random_empty_square(rng)
            .expect("failed to get random square");
        self.place_piece(piece, rand_pos);
        rand_pos
    }

    pub fn place_block_randomly(&mut self, rng: &mut StdRng) {
        let rand_pos = self
            .random_empty_square(rng)
            .expect("failed to get random square");
        self.place_block(rand_pos);
    }

    pub fn get_piece_at(&self, square: WorldSquare) -> Option<&Piece> {
        self.pieces.get(&square)
    }
    pub fn get_mut_piece_at(&mut self, square: WorldSquare) -> Option<&mut Piece> {
        self.pieces.get_mut(&square)
    }

    pub fn is_non_player_piece_at(&self, square: WorldSquare) -> bool {
        self.get_piece_at(square).is_some()
    }

    pub fn is_piece_at(&self, square: WorldSquare) -> bool {
        self.get_piece_at(square).is_some()
            || self.try_get_player_square().is_some_and(|s| s == square)
    }

    pub fn is_upgrade_at(&self, square: WorldSquare) -> bool {
        self.upgrades.contains_key(&square)
    }
    pub fn is_arrow_at(&self, square: WorldSquare) -> bool {
        self.pieces
            .get(&square)
            .is_some_and(|piece| piece.piece_type == Arrow)
    }

    pub fn piece_type_count(&self, piece_type: PieceType) -> i32 {
        self.pieces
            .values()
            .filter(|&&piece| piece.piece_type == piece_type)
            .count() as i32
    }

    pub fn select_all_pieces(&mut self) {
        self.graphics
            .select_squares(self.pieces.keys().cloned().collect());
    }
    pub fn select_closest_piece(&mut self) {
        let closest_piece_square: Option<WorldSquare> = self.square_of_closest_piece_to_player();

        self.selected_square = closest_piece_square;
        if let Some(square) = closest_piece_square {
            self.select_square(square);
        } else {
            self.clear_selectors();
        }
    }

    pub fn select_square(&mut self, square: WorldSquare) {
        self.graphics.select_squares(vec![square]);
    }
    pub fn clear_selectors(&mut self) {
        self.graphics.select_squares(vec![]);
    }

    fn square_of_closest_piece_to_player(&self) -> Option<WorldSquare> {
        let slightly_right_of_player_position: WorldPoint =
            self.player_square().to_f32() + WorldMove::new(0.01, 0.0);

        self.pieces
            .keys()
            .min_by_key(|square| {
                OrderedFloat((square.to_f32() - slightly_right_of_player_position).length())
            })
            .cloned()
    }

    pub fn on_turn_end(&mut self) {
        self.tick_pawn_incubation();
        self.convert_orphaned_pieces();
        if self.player_is_alive() {
            self.select_closest_piece();
        }
    }

    pub fn convert_orphaned_pieces(&mut self) {
        for faction in self.get_enemy_factions() {
            let mut pieces_in_faction: Vec<&mut Piece> = self
                .pieces
                .iter_mut()
                .map(|(_, piece)| piece)
                .filter(|piece| piece.faction == faction)
                .collect();
            let all_same_piece_type: bool = pieces_in_faction.iter().all_equal();
            let faction_has_a_pawn =
                pieces_in_faction.iter().next().unwrap().piece_type == OmniDirectionalPawn;
            let faction_has_only_pawns = all_same_piece_type && faction_has_a_pawn;
            if faction_has_only_pawns {
                pieces_in_faction
                    .iter_mut()
                    .for_each(|piece| piece.faction = self.red_pawn_faction);
            }
        }
    }

    pub fn move_all_pieces(&mut self) {
        self.move_non_arrow_factions();
        self.tick_projectile_arrows();
        self.turn_count += 1;
    }

    fn non_arrow_piece_squares(&self) -> SquareSet {
        self.pieces
            .iter()
            .filter(|(&_square, &piece)| piece.piece_type != Arrow)
            .map(|(&square, &_piece)| square)
            .collect()
    }

    pub fn move_non_arrow_factions(&mut self) {
        for faction in self.get_enemy_factions() {
            self.move_faction(faction);
        }
    }

    fn get_enemy_factions(&self) -> HashSet<Faction> {
        self.pieces
            .values()
            .map(|piece| piece.faction)
            .unique()
            .filter(|&faction| matches!(faction, Faction::Enemy(_) | Faction::RedPawn))
            .collect()
    }

    pub fn get_new_faction(&mut self) -> Faction {
        let new_faction = self.faction_factory.get_new_faction();
        //self.faction_info.insert(new_faction, Default::default());
        new_faction
    }

    fn squares_of_pieces_in_faction(&self, faction: Faction) -> Vec<WorldSquare> {
        self.pieces
            .iter()
            .filter(|(square, piece)| piece.faction == faction)
            .map(|(&square, piece)| square)
            .collect()
    }

    fn move_faction(&mut self, faction: Faction) {
        let faction_squares = self.squares_of_pieces_in_faction(faction);

        if faction == self.red_pawn_faction {
            // all pieces move
            faction_squares.iter().for_each(|&square| {
                self.move_piece_at_square_and_return_end_position_if_moved(square);
            });
        } else if self.player_is_alive() {
            self.move_piece_at_square_and_return_end_position_if_moved(
                self.square_of_closest_piece_to_player_in_faction(faction),
            );
        } else {
            // select one non-randomly
            let square_of_piece_to_move = faction_squares
                .into_iter()
                .min_by_key(|&square| OrderedFloat(square.x as f32 + 0.1 + square.y as f32))
                .unwrap();
            self.move_piece_at_square_and_return_end_position_if_moved(square_of_piece_to_move);
        }
    }

    fn square_of_closest_piece_to_player_in_faction(&self, faction: Faction) -> WorldSquare {
        self.squares_of_pieces_in_faction(faction)
            .into_iter()
            .min_by_key(|&square| (square - self.player_square()).square_length())
            .unwrap()
    }
    fn kill_player(&mut self) {
        // TODO: less abrupt game-over
        self.player_optional = None;
        self.quit();
    }

    fn move_piece(&mut self, start: WorldSquare, end: WorldSquare) {
        // capture player
        if !self.is_non_player_piece_at(start) {
            panic!("No piece to move at {}", start.to_string());
        }
        if self.is_player_at(end) {
            self.kill_player();
        }
        if self.is_non_player_piece_at(end) {
            let target_piece = self.pieces.get(&end).unwrap();
            let this_piece = self.pieces.get(&start).unwrap();
            if this_piece.faction == target_piece.faction {
                panic!("Tried to capture allied piece at {}", end.to_string());
            }
            self.capture_piece_at(end);
        }
        let piece = self.pieces.remove(&start).unwrap();
        self.pieces.insert(end, piece);
    }

    fn slide_cast(
        &self,
        start_square: WorldSquare,
        repeating_step: NStep,
        pass_through_pieces: bool,
    ) -> SquareList {
        let mut valid_squares: SquareList = vec![];
        let range_cap: u32 = repeating_step.n().unwrap_or(MAX_PIECE_RANGE);
        for i in 0..range_cap {
            let distance = i + 1;
            // TODO: Allow knights to step through portals (probably by line-of-sight between start and end squares)
            let square = if repeating_step.stepp().is_king_step() {
                if let Ok(end_pose) = self.multiple_portal_aware_steps(
                    SquareWithKingDir::from_square_and_step(
                        start_square,
                        repeating_step.stepp().into(),
                    ),
                    distance,
                ) {
                    end_pose.square()
                } else {
                    break;
                }
            } else {
                start_square + repeating_step.stepp() * distance as i32
            };
            if !self.square_is_on_board(square) {
                break;
            }
            valid_squares.push(square);
            if pass_through_pieces && self.is_non_player_piece_at(square) {
                // keep going
            } else {
                if !self.square_is_empty(square) {
                    break;
                }
            }
        }
        valid_squares
    }

    pub fn square_to_move_toward_player_for_piece_at(
        &self,
        piece_square: WorldSquare,
    ) -> Option<WorldSquare> {
        if !self.player_is_alive() {
            return None;
        }
        let current_square_distance_to_player =
            (self.player_square() - piece_square).square_length();
        let closest_move_option_to_player = self
            .move_options_for_piece_at(piece_square)
            .into_iter()
            .filter(|&square| self.square_is_empty(square) && self.square_is_on_board(square))
            .min_by_key(|&square| (square - self.player_square()).square_length());
        if let Some(end_square) = closest_move_option_to_player {
            let possible_square_distance = (end_square - self.player_square()).square_length();
            if possible_square_distance < current_square_distance_to_player {
                return closest_move_option_to_player;
            }
        }
        None
    }

    pub fn piece_can_capture_player(&self, piece_square: WorldSquare) -> bool {
        self.player_is_alive()
            && self
                .capture_options_for_piece_at(piece_square)
                .into_iter()
                .contains(&self.player_square())
    }
    pub fn highest_priority_capture_square_for_piece_at(
        &self,
        piece_square: WorldSquare,
    ) -> Option<WorldSquare> {
        let friendly_faction = self.get_piece_at(piece_square).unwrap().faction;
        // TODO: choose randomly rather than first
        self.capture_options_for_piece_at(piece_square)
            .into_iter()
            .filter(|&world_square| {
                self.get_piece_at(world_square)
                    .is_some_and(|piece| piece.faction != friendly_faction)
            })
            .next()
    }

    pub fn allies_within_radius_excluding_center(
        &self,
        center_square: WorldSquare,
        radius: u32,
        faction: Faction,
    ) -> SquareSet {
        let mut nearby_ally_squares = SquareSet::new();
        // intentional shadow
        let radius = radius as i32;
        (-radius..=radius).for_each(|y_offset| {
            (-radius..=radius).for_each(|x_offset| {
                let square = center_square + STEP_UP * y_offset + STEP_RIGHT * x_offset;
                if square != center_square
                    && self
                        .pieces
                        .get(&square)
                        .is_some_and(|other_piece| other_piece.faction == faction)
                {
                    nearby_ally_squares.insert(square);
                }
            });
        });
        nearby_ally_squares
    }

    pub fn protection_strengths_from_given_pawns(
        &self,
        pawn_squares: SquareSet,
    ) -> HashMap<WorldSquare, u32> {
        let steps = HashSet::from_iter(DIAGONAL_STEPS);
        cross_correlate_squares_with_steps(pawn_squares, steps)
    }

    pub fn orthogonal_adjacency_from_given_squares(
        &self,
        squares: SquareSet,
    ) -> HashMap<WorldSquare, u32> {
        let steps = HashSet::from(ORTHOGONAL_STEPS);
        cross_correlate_squares_with_steps(squares, steps)
    }

    fn move_red_pawn_at(&mut self, piece_square: WorldSquare) -> Option<WorldSquare> {
        let mut end_square: Option<WorldSquare>;
        let piece = self.get_piece_at(piece_square).unwrap().clone();
        assert_eq!(piece.faction, self.red_pawn_faction);
        // Look at surrounding 5x5 square
        let mut nearby_ally_squares =
            self.allies_within_radius_excluding_center(piece_square, 2, piece.faction);
        let nearby_protection_strengths =
            self.protection_strengths_from_given_pawns(nearby_ally_squares.clone());
        let nearby_ally_crowdedness =
            self.orthogonal_adjacency_from_given_squares(nearby_ally_squares.clone());
        let mut best_case_move_steps = Vec::from(ORTHOGONAL_STEPS);
        best_case_move_steps.push(vec2(0, 0));

        let viable_move_squares: HashSet<WorldSquare> = best_case_move_steps
            .iter()
            .map(|&step| piece_square + step)
            .filter(|&square| square == piece_square || self.square_is_empty(square))
            .collect();

        let neutral_goodness_at_viable_move_squares = viable_move_squares
            .iter()
            .map(|&square| (square, 0.0))
            .collect();

        let protection_at_movable_squares: HashMap<WorldSquare, u32> = nearby_protection_strengths
            .into_iter()
            .filter(|(protected_square, strength)| viable_move_squares.contains(protected_square))
            .collect();

        let ally_crowdedness_at_movable_squares: HashMap<WorldSquare, u32> =
            nearby_ally_crowdedness
                .into_iter()
                .filter(|(protected_square, strength)| {
                    viable_move_squares.contains(protected_square)
                })
                .collect();

        let mut goodness_metric_at_move_options = map_sum(
            neutral_goodness_at_viable_move_squares,
            map_to_float(map_sum(
                map_to_signed(protection_at_movable_squares),
                map_neg(map_to_signed(ally_crowdedness_at_movable_squares)),
            )),
        );

        // slight preference for motion
        *goodness_metric_at_move_options
            .entry(piece_square)
            .or_default() -= 1.5;

        let current_goodness: f32 = goodness_metric_at_move_options
            .get(&piece_square)
            .cloned()
            .unwrap_or_default();
        let most_goodness_available: f32 = goodness_metric_at_move_options
            .values()
            .max_by_key(|&&x| OrderedFloat(x))
            .cloned()
            .unwrap_or_default();
        if most_goodness_available > current_goodness {
            end_square = Some(
                goodness_metric_at_move_options
                    .iter()
                    .max_by_key(|(&square, &goodness)| OrderedFloat(goodness))
                    .unwrap()
                    .0
                    .clone(),
            );
        } else {
            end_square = None
        }

        if let Some(move_square) = end_square {
            self.move_piece(piece_square, move_square);
        }
        end_square
    }

    // returns where the piece moves to, if applicable
    pub fn move_piece_at_square_and_return_end_position_if_moved(
        &mut self,
        piece_square: WorldSquare,
    ) -> Option<WorldSquare> {
        if !self.is_non_player_piece_at(piece_square) {
            return None;
        }

        let piece = self.get_piece_at(piece_square).unwrap().clone();

        let mut end_square: Option<WorldSquare>;

        if piece.faction == self.red_pawn_faction {
            return self.move_red_pawn_at(piece_square);
        }
        if self.player_is_alive() {
            if piece.piece_type == King {
                if let Some(path_to_player) =
                    self.find_king_path(piece_square, self.player_square())
                {
                    let first_step_square = *path_to_player.get(1).unwrap();
                    end_square = Some(first_step_square);
                } else {
                    end_square = None;
                }
            } else if self.piece_can_capture_player(piece_square) {
                end_square = Some(self.player_square());
            } else if let Some(square) =
                self.square_to_move_toward_player_for_piece_at(piece_square)
            {
                end_square = Some(square);
            } else {
                if piece.can_turn() {
                    self.turn_piece_toward_player(piece_square);
                    return Some(piece_square);
                }
                end_square = None;
            }
        } else if let optional_square =
            self.highest_priority_capture_square_for_piece_at(piece_square)
        {
            end_square = optional_square;
        } else {
            end_square = None;
        }

        if let Some(move_square) = end_square {
            self.move_piece(piece_square, move_square);
        }
        end_square
    }

    fn turn_piece_toward_player(&mut self, piece_square: WorldSquare) {
        assert!(self.is_non_player_piece_at(piece_square));
        let piece = self.get_piece_at(piece_square).unwrap();
        assert!(piece.can_turn());
        assert!(self.player_is_alive());

        let vector_to_player = self.player_square() - piece_square;
        let angle_to_player = |p: &Piece| -> Angle<f32> {
            p.faced_direction()
                .step()
                .to_f32()
                .angle_to(vector_to_player.to_f32())
        };
        let mut best_angle_to_player_yet = angle_to_player(piece);
        let mut best_rotation_yet = piece.clone();
        for turned_piece in piece.turned_versions() {
            let possible_angle_to_player = angle_to_player(&turned_piece);
            if possible_angle_to_player.radians.abs() < best_angle_to_player_yet.radians.abs() {
                best_rotation_yet = turned_piece;
                best_angle_to_player_yet = possible_angle_to_player;
            }
        }

        if best_rotation_yet != *piece {
            self.pieces.remove(&piece_square);
            self.pieces.insert(piece_square, best_rotation_yet);
        }
    }

    fn move_options_for_piece_at(&self, piece_square: WorldSquare) -> SquareList {
        self.on_board_move_squares_for_piece_at(piece_square, false)
            .into_iter()
            .filter(|&square| self.square_is_empty(square))
            .collect()
    }

    fn on_board_move_or_capture_squares_for_piece_at(
        &self,
        piece_square: WorldSquare,
        capture_instead_of_move: bool,
        pass_through_pieces: bool,
    ) -> SquareSet {
        assert!(self.is_non_player_piece_at(piece_square));
        let mut squares = SquareSet::new();
        let piece = self.get_piece_at(piece_square).unwrap();

        let move_function = if capture_instead_of_move {
            Piece::relative_captures
        } else {
            Piece::relative_moves
        };

        for move_direction in move_function(piece) {
            let mut squares_to_collision =
                self.slide_cast(piece_square, move_direction, pass_through_pieces);
            squares.extend(squares_to_collision);
        }
        squares
    }

    fn on_board_capture_squares_for_piece_at(
        &self,
        piece_square: WorldSquare,
        pass_through_pieces: bool,
    ) -> SquareSet {
        self.on_board_move_or_capture_squares_for_piece_at(piece_square, true, pass_through_pieces)
    }
    fn on_board_move_squares_for_piece_at(
        &self,
        piece_square: WorldSquare,
        pass_through_pieces: bool,
    ) -> SquareSet {
        self.on_board_move_or_capture_squares_for_piece_at(piece_square, false, pass_through_pieces)
    }

    fn move_squares_for_all_pieces(&self, pass_through_pieces: bool) -> SquareSet {
        self.pieces
            .keys()
            .map(|&square| self.on_board_move_squares_for_piece_at(square, pass_through_pieces))
            .flatten()
            .collect()
    }
    fn squares_threatened_by_any_piece(&self, pass_through_pieces: bool) -> SquareSet {
        self.pieces
            .keys()
            .map(|&square| self.on_board_capture_squares_for_piece_at(square, pass_through_pieces))
            .flatten()
            .collect()
    }

    fn guarded_squares_for_piece_at(&self, piece_square: WorldSquare) -> SquareSet {
        self.on_board_capture_squares_for_piece_at(piece_square, false)
    }

    fn find_king_path(
        &self,
        start_square: WorldSquare,
        target_square: WorldSquare,
    ) -> Option<Vec<WorldSquare>> {
        fn cost_heuristic(a: WorldSquare, b: WorldSquare) -> u32 {
            king_step_distance(a - b)
        }
        let relative_steps = KING_STEPS;
        let mut recorded_step_start_squares_by_step_end_squares =
            HashMap::<WorldSquare, WorldSquare>::new();
        let mut squares_to_check = DoublePriorityQueue::<WorldSquare, u32>::new();
        squares_to_check.push(start_square, cost_heuristic(start_square, target_square));
        while let Some((square_to_check, cost)) = squares_to_check.pop_min() {
            let next_squares: SquareList = relative_steps
                .clone()
                .into_iter()
                .map(|step_to_next_square| square_to_check + step_to_next_square)
                .filter(|&next_square| {
                    !recorded_step_start_squares_by_step_end_squares.contains_key(&next_square)
                        && (self.square_is_empty(next_square) || self.is_player_at(next_square))
                })
                .collect();
            next_squares.clone().into_iter().for_each(|next_square| {
                let new_cost = cost + cost_heuristic(next_square, target_square);
                squares_to_check.push(next_square, new_cost);
                recorded_step_start_squares_by_step_end_squares
                    .insert(next_square, square_to_check);
            });
            if next_squares.contains(&target_square) {
                break;
            }
        }
        if !recorded_step_start_squares_by_step_end_squares.contains_key(&target_square) {
            return None;
        }
        let mut reverse_full_path = vec![target_square];
        while *reverse_full_path.last().unwrap() != start_square {
            reverse_full_path.push(
                *recorded_step_start_squares_by_step_end_squares
                    .get(reverse_full_path.last().unwrap())
                    .unwrap(),
            );
        }
        Some(reversed(reverse_full_path))
    }

    fn capture_options_for_piece_at(&self, piece_square: WorldSquare) -> SquareList {
        assert!(self.is_non_player_piece_at(piece_square));

        let mut capture_squares: SquareList = vec![];

        for square in self.guarded_squares_for_piece_at(piece_square) {
            if !self.square_is_empty(square) {
                capture_squares.push(square);
            }
        }
        capture_squares
    }

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
                + (self.player_faced_direction().step().to_f32() * range)
                    .rotate_vect(Angle::radians(rotation_if_uniform))
                    .cast_unit()
                + rand_radial_offset(random_spread_radius).cast_unit();
            let line =
                TwoDifferentWorldPoints::new_from_two_points_on_line(line_start.to_f32(), line_end);

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
        let mut graphical_laser_end: WorldSquare;
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
                square.to_string()
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
                square.to_string()
            ))
        }
    }

    pub fn place_arrow(&mut self, square: WorldSquare, direction: KingWorldStep) {
        self.place_piece(Piece::arrow(direction), square);
    }

    pub fn place_single_sided_one_way_portal(
        &mut self,
        entrance_step: impl Into<WorldSquareWithOrthogonalDir>,
        exit_step: impl Into<WorldSquareWithOrthogonalDir>,
    ) {
        self.portal_geometry.create_portal(entrance_step, exit_step);
    }
    pub fn place_double_sided_one_way_portal(
        &mut self,
        entrance_step: WorldSquareWithOrthogonalDir,
        exit_step: WorldSquareWithOrthogonalDir,
    ) {
        self.portal_geometry
            .create_double_sided_one_way_portal(entrance_step, exit_step);
    }
    pub fn place_single_sided_two_way_portal(
        &mut self,
        entrance_step: WorldSquareWithOrthogonalDir,
        exit_step: WorldSquareWithOrthogonalDir,
    ) {
        self.portal_geometry
            .create_single_sided_two_way_portal(entrance_step, exit_step);
    }
    pub fn place_double_sided_two_way_portal(
        &mut self,
        entrance_step: WorldSquareWithOrthogonalDir,
        exit_step: WorldSquareWithOrthogonalDir,
    ) {
        self.portal_geometry
            .create_double_sided_two_way_portal(entrance_step, exit_step);
    }
    pub fn place_dense_horizontal_portals(
        &mut self,
        top_left: WorldSquare,
        portal_rows: u32,
        portal_cols: u32,
    ) {
        (0..portal_rows).for_each(|row| {
            (0..portal_cols).for_each(|col| {
                let entrance_square =
                    top_left + STEP_RIGHT * col as i32 * 2 + STEP_DOWN * row as i32;
                self.place_offset_rightward_double_sided_two_way_portal(
                    entrance_square,
                    STEP_RIGHT,
                );
            });
        });
    }
    fn place_wide_portal(
        &mut self,
        left_entrance: WorldSquareWithOrthogonalDir,
        left_exit: WorldSquareWithOrthogonalDir,
        width: u32,
    ) {
        (0..width as i32).for_each(|i| {
            self.place_double_sided_two_way_portal(
                left_entrance.strafed_right_n(i),
                left_exit.strafed_right_n(i),
            )
        });
    }

    pub fn place_offset_rightward_double_sided_two_way_portal(
        &mut self,
        start_square: WorldSquare,
        offset: WorldStep,
    ) {
        let exit_square = start_square + STEP_RIGHT + offset;
        let entrance = WorldSquareWithOrthogonalDir::from_square_and_step(start_square, STEP_RIGHT);
        let exit = WorldSquareWithOrthogonalDir::from_square_and_step(exit_square, STEP_RIGHT);
        self.place_double_sided_two_way_portal(entrance, exit);
    }

    pub fn place_widget(&mut self, pushable: Widget, square: WorldSquare) {
        self.widgets.insert(square, pushable);
    }
    pub fn place_floor_push_arrow(&mut self, square: WorldSquare, dir: OrthogonalDirection) {
        self.floor_push_arrows.insert(square, dir);
    }
    pub fn place_conveyor_belt(&mut self, square: WorldSquare, dir: OrthogonalDirection) {
        self.conveyor_belts.insert(square, dir);
    }
    pub fn conveyor_belt_speed() -> f32 {
        1.0 / CONVEYOR_BELT_MOVEMENT_PERIOD_S
    }

    pub fn place_block(&mut self, square: WorldSquare) {
        self.blocks.insert(square);
    }
    pub fn is_block_at(&self, square: WorldSquare) -> bool {
        self.blocks.contains(&square)
    }
    pub fn set_up_vs_arrows(&mut self) {
        (0..10).for_each(|i| {
            self.place_arrow(point2(0, 1 + i), STEP_RIGHT.into());
            self.place_arrow(point2(1 + i, 0), STEP_UP.into());
        });
    }

    pub fn set_up_vs_red_pawns(&mut self) {
        let distance = 4;
        let width = 9;
        let depth = 5;

        let start_square = self.player_square() + STEP_UP * distance + STEP_LEFT * width / 2;
        for dx in 0..width {
            for dy in 0..depth {
                let vec = vec2(dx, dy);
                self.place_red_pawn(start_square + vec);
            }
        }
    }

    pub fn set_up_vs_mini_factions(&mut self) {
        let distance = 5;
        self.place_new_king_pawn_faction(self.player_square() + STEP_UP_LEFT * distance);
        self.place_new_king_pawn_faction(self.player_square() + STEP_UP * distance);
        self.place_random_3x3_faction(self.player_square() + STEP_UP * distance * 2);
        self.place_new_king_pawn_faction(self.player_square() + STEP_UP_RIGHT * distance);
    }
    pub fn set_up_upgrades_galore(&mut self) {
        for i in 0..8 {
            self.place_upgrade(
                BlinkRange,
                self.player_square() + STEP_UP * 5 + STEP_RIGHT * i,
            );
        }
    }

    pub fn set_up_vs_weak_with_pillars_and_turret_and_upgrades(&mut self) {
        self.set_up_columns();
        for x in 0..8 {
            let piece_type = match x % 4 {
                0 => TurningPawn,
                1 => TurningSoldier,
                2 => OmniDirectionalPawn,
                3 => OmniDirectionalSoldier,
                _ => panic!("bad math"),
            };

            self.place_piece(
                Piece::from_type(piece_type),
                self.player_square() + STEP_UP * 5 + STEP_RIGHT * (x - 3),
            );
            self.place_upgrade(
                BlinkRange,
                self.player_square() + STEP_UP * 7 + STEP_RIGHT * (x - 3),
            )
        }
    }
    pub fn set_up_homogeneous_army(&mut self, piece_type: PieceType) {
        for y in 0..3 {
            for x in 0..8 {
                self.place_piece(
                    Piece::from_type(piece_type),
                    self.player_square() + STEP_UP * (5 + y) + STEP_RIGHT * (x - 3),
                );
            }
        }
    }
    pub fn set_up_n_pillars(&mut self, n: u32) {
        (0..n).for_each(|i| self.place_block(self.player_square() + STEP_RIGHT * (i as i32 + 4)));
    }
    pub fn set_up_simple_portal_map(&mut self) {
        let entrance_square = self.player_square() + STEP_RIGHT * 2;
        let exit_square = entrance_square + STEP_RIGHT * 4;

        self.place_block(entrance_square + STEP_UP_RIGHT);
        self.place_block(entrance_square + STEP_DOWN_RIGHT);
        self.place_block(exit_square + STEP_UP_LEFT);
        self.place_block(exit_square + STEP_DOWN_LEFT);
        let entrance =
            WorldSquareWithOrthogonalDir::from_square_and_step(entrance_square, STEP_RIGHT);
        let exit = WorldSquareWithOrthogonalDir::from_square_and_step(exit_square, STEP_RIGHT);

        self.place_double_sided_two_way_portal(entrance, exit);
    }
    pub fn set_up_simple_freestanding_portal(&mut self) {
        let entrance_square = self.player_square() + STEP_RIGHT * 8;
        let exit_square = entrance_square + STEP_RIGHT * 3 + STEP_UP * 5;

        let entrance =
            WorldSquareWithOrthogonalDir::from_square_and_step(entrance_square, STEP_RIGHT);
        let exit = WorldSquareWithOrthogonalDir::from_square_and_step(exit_square, STEP_RIGHT);

        self.place_double_sided_two_way_portal(entrance, exit);
    }
    pub fn set_up_portal_across_wall_map(&mut self, width: u32, height_radius: u32) {
        let entrance_square = self.player_square() + STEP_RIGHT * 2;
        let exit_square = entrance_square + STEP_RIGHT * (width as i32 + 1);

        let n: i32 = (height_radius * 2 + 1) as i32;
        (0..width as i32).for_each(|x| {
            (0..n).for_each(|i| {
                self.place_block(
                    entrance_square + STEP_RIGHT * (x + 1) + STEP_UP * i + STEP_DOWN * n / 2,
                )
            });
        });
        let entrance =
            WorldSquareWithOrthogonalDir::from_square_and_step(entrance_square, STEP_RIGHT);
        let exit = WorldSquareWithOrthogonalDir::from_square_and_step(exit_square, STEP_RIGHT);

        self.place_double_sided_two_way_portal(entrance, exit);
    }
    pub fn set_up_simple_test_map(&mut self) {
        let width = 2;
        let spacing = width * 2 + 1;
        let left_entrance = WorldSquareWithOrthogonalDir::from_square_and_step(
            self.player_square() + STEP_RIGHT * 5,
            STEP_RIGHT,
        );
        let left_exit = WorldSquareWithOrthogonalDir::from_square_and_step(
            left_entrance.square() + STEP_UP + STEP_RIGHT * 3,
            STEP_UP,
        );
        (0..width).for_each(|i| {
            self.place_double_sided_two_way_portal(
                left_entrance.strafed_right_n(i),
                left_exit.strafed_right_n(i),
            )
        });
    }

    pub fn set_up_test_map(&mut self) {
        // self.set_up_simple_test_map();
        // return;

        let base_square = self.player_square();

        self.place_widget(Widget::new(5), base_square + STEP_UP * 4);
        self.place_widget(Widget::new(13), base_square + STEP_UP * 5);
        for i in 0..3 {
            self.place_floor_push_arrow(base_square + STEP_UP * 6 + STEP_RIGHT * i, RIGHT);
            self.place_conveyor_belt(base_square + STEP_UP * 8 + STEP_RIGHT * i, LEFT);
        }

        for i in 0..4 {
            self.place_floating_hunter_drone(
                (base_square + STEP_DOWN * (5 + i)).to_f32(),
                STEP_RIGHT.to_f32() * i as f32,
                Angle::degrees(0.0),
            );
        }

        let left_entrance = WorldSquareWithOrthogonalDir::from_square_and_step(
            base_square + STEP_RIGHT * 3 + STEP_UP * 3,
            STEP_RIGHT,
        );
        let left_exit = WorldSquareWithOrthogonalDir::from_square_and_step(
            left_entrance.square() + STEP_UP * 2 + STEP_RIGHT * 2,
            STEP_UP,
        );
        (0..6).for_each(|i| {
            let entrance = left_entrance.strafed_right_n(i);
            let exit = left_exit.strafed_right_n(i);
            self.place_double_sided_two_way_portal(entrance, exit);
        });

        let block_square = left_entrance.square() + STEP_RIGHT * 2 + STEP_DOWN_RIGHT * 3;
        self.place_block(block_square);

        self.place_dense_horizontal_portals(base_square + STEP_RIGHT * 20, 1, 10);

        let entrance = WorldSquareWithOrthogonalDir::from_square_and_step(
            base_square + STEP_LEFT * 7 + STEP_UP * 3,
            STEP_RIGHT,
        );
        let exit = entrance.stepped_n(5).strafed_right();
        self.place_wide_portal(entrance, exit, 5);

        let bars_top_right_root_square = base_square + STEP_LEFT * 20 + STEP_UP * 5;
        self.place_dotted_thin_walls(bars_top_right_root_square);
    }

    fn place_dotted_thin_walls(&mut self, bars_top_left_root_square: WorldSquare) {
        let block_square = bars_top_left_root_square + STEP_DOWN_LEFT * 3;
        self.place_block(block_square);

        self.place_line_of_portals_to_one_exit(
            (bars_top_left_root_square, STEP_UP).into(),
            STEP_RIGHT * 2,
            5,
            (block_square, STEP_DOWN).into(),
        );
        self.place_line_of_portals_to_one_exit(
            (bars_top_left_root_square + STEP_DOWN * 8, STEP_RIGHT).into(),
            STEP_RIGHT,
            10,
            (block_square, STEP_DOWN).into(),
        );
        self.place_line_of_portals_to_one_exit(
            (bars_top_left_root_square + STEP_LEFT * 5, STEP_LEFT).into(),
            STEP_DOWN * 2,
            5,
            (block_square, STEP_DOWN).into(),
        );
        self.place_line_of_portals_to_one_exit(
            (bars_top_left_root_square + STEP_LEFT * 13, STEP_DOWN).into(),
            STEP_DOWN,
            10,
            (block_square, STEP_DOWN).into(),
        );
    }

    fn place_line_of_portals_to_one_exit(
        &mut self,
        first_entrance: WorldSquareWithOrthogonalDir,
        step: WorldStep,
        num_portals: u32,
        common_exit: WorldSquareWithOrthogonalDir,
    ) {
        (0..num_portals).for_each(|i| {
            let entrance: WorldSquareWithOrthogonalDir = (
                first_entrance.square() + step * i as i32,
                first_entrance.direction(),
            )
                .into();
            self.place_single_sided_one_way_portal(entrance, common_exit);
            self.place_single_sided_one_way_portal(entrance.stepped().turned_back(), common_exit);
        });
    }

    // TODO: fix

    fn place_rotation_portal_square(&mut self, top_left: WorldSquare, side_length: u32) {
        let mut entrance_step =
            WorldSquareWithOrthogonalDir::from_square_and_step(top_left + STEP_LEFT, STEP_RIGHT);
        let mut exit_step =
            WorldSquareWithOrthogonalDir::from_square_and_step(top_left + STEP_DOWN, STEP_DOWN);

        (0..4).for_each(|_| {
            (0..side_length).for_each(|i| {
                self.place_single_sided_one_way_portal(entrance_step, exit_step);
                entrance_step = entrance_step.strafed_right();
                exit_step = exit_step.strafed_right();
            });
            entrance_step = exit_step.turned_back();
            exit_step = exit_step
                .turned_left()
                .stepped_n(side_length as i32 + 1)
                .turned_left()
                .stepped_n(side_length as i32)
                .turned_right();
        })
    }

    pub fn set_up_columns(&mut self) {
        let block_square = self.player_square() + STEP_RIGHT * 4;
        self.place_block(block_square);
        self.place_block(block_square + STEP_RIGHT * 3);
        self.place_block(block_square + STEP_UP * 3);
        self.place_block(block_square + STEP_UP_RIGHT * 3);
        self.place_linear_death_cube(
            self.player_square().to_f32() - vec2(5.0, 3.0),
            vec2(0.1, 0.3),
        );
        self.place_double_sided_two_way_portal(
            WorldSquareWithOrthogonalDir::from_square_and_step(
                block_square + STEP_UP_RIGHT,
                STEP_DOWN,
            ),
            WorldSquareWithOrthogonalDir::from_square_and_step(
                block_square + STEP_DOWN_RIGHT * 4,
                STEP_LEFT,
            ),
        );
        //self.place_death_turret(self.player_square() + STEP_LEFT * 14);
        self.set_up_n_pillars(3);
    }

    pub fn set_up_labyrinth(&mut self, rng: &mut StdRng) {
        let board_squares_total = self.board_size().width() * self.board_size().height();
        let num_blocks = board_squares_total / 3;
        for _ in 0..num_blocks {
            self.place_block_randomly(rng);
        }
    }

    pub fn set_up_labyrinth_hunt(&mut self, rng: &mut StdRng) {
        self.set_up_labyrinth(rng);
        for piece_type in PieceType::iter() {
            for _ in 0..2 {
                self.place_piece_randomly(Piece::from_type(piece_type), rng);
            }
        }
    }
    pub fn set_up_labyrinth_kings(&mut self, rng: &mut StdRng) {
        self.set_up_labyrinth(rng);
        for _ in 0..8 {
            self.place_piece_randomly(Piece::king(), rng);
        }
    }
    pub fn square_is_fully_visible_to_player(&self, square: WorldSquare) -> bool {
        let target_square_relative_to_player = square - self.player_square();
        self.rasterized_player_field_of_view()
            .relative_square_is_fully_visible(target_square_relative_to_player)
    }
    pub fn square_is_visible_to_player(&self, square: WorldSquare) -> bool {
        let target_square_relative_to_player = square - self.player_square();
        self.rasterized_player_field_of_view()
            .relative_square_is_visible(target_square_relative_to_player)
    }
    fn player_field_of_view(&self) -> FieldOfView {
        let start_square = self.player_square();
        portal_aware_field_of_view_from_square(
            start_square,
            PLAYER_SIGHT_RADIUS,
            &self.blocks,
            &self.portal_geometry,
        )
    }
    fn rasterized_player_field_of_view(&self) -> RasterizedFieldOfView {
        self.player_field_of_view().rasterized()
    }

    pub fn get_color_for_faction(&self, faction: Faction) -> RGB8 {
        if faction == self.red_pawn_faction {
            RED_PAWN_COLOR
        } else {
            WHITE
        }
    }

    pub fn portal_aware_single_step(
        &self,
        start: SquareWithKingDir,
    ) -> Result<SquareWithKingDir, ()> {
        self.portal_geometry.portal_aware_single_step(start)
    }

    pub fn multiple_portal_aware_steps(
        &self,
        start: SquareWithKingDir,
        num_steps: u32,
    ) -> Result<SquareWithKingDir, ()> {
        self.portal_geometry
            .multiple_portal_aware_steps(start, num_steps)
    }
}

#[cfg(test)]
mod tests {
    use crate::fov_stuff::debug_print_fov_as_absolute;
    use crate::fov_stuff::rasterized_field_of_view::TopDownPortal;
    use crate::fov_stuff::square_visibility::SquareVisibility;
    use ::num::integer::Roots;
    use ntest::{assert_about_eq, assert_false, timeout};
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::game;
    use crate::glyph::braille::{char_is_braille, EMPTY_BRAILLE};
    use crate::glyph::glyph_constants::{
        BLINK_EFFECT_COLOR, BLOCK_FG, BLUE, DANGER_SQUARE_COLOR, FULL_BLOCK, GREY,
        HUNTER_DRONE_COLOR, LEFT_HALF_BLOCK, OUT_OF_SIGHT_COLOR, RED, RED_PAWN_COLOR,
        RIGHT_HALF_BLOCK, SIGHT_LINE_SEEKING_COLOR,
    };
    use crate::glyph::{DoubleGlyph, DoubleGlyphFunctions};
    use crate::graphics::drawable::{Drawable, DrawableEnum};
    use crate::graphics::screen::{
        Screen, SCREEN_STEP_DOWN, SCREEN_STEP_DOWN_RIGHT, SCREEN_STEP_RIGHT, SCREEN_STEP_UP,
        SCREEN_STEP_UP_RIGHT, SCREEN_STEP_ZERO,
    };
    use crate::piece::PieceType::Rook;
    use crate::piece::Upgrade;
    use crate::utility::*;
    use crate::utils_for_tests::*;

    use super::*;

    #[test]

    fn test_try_set_player_on_block_is_fail() {
        let mut game = Game::new(20, 10, Instant::now());
        game.place_player(point2(5, 5));
        game.place_block(point2(3, 3));
        assert!(game.try_set_player_position(point2(3, 3)).is_err());
    }

    #[test]

    fn test_blocks_block_view() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(5, 5));
        game.place_block(point2(5, 4));
        let test_square = point2(5, 3);
        assert_false!(game.square_is_fully_visible_to_player(test_square));
    }

    #[test]
    fn test_fov_mask_non_partials() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(5, 5));
        for i in 0..4 {
            game.place_block(game.player_square() + STEP_DOWN + STEP_RIGHT * i);
        }
        let relative_squares_that_should_be_fully_visible = vec![
            STEP_RIGHT,
            STEP_UP_RIGHT,
            STEP_UP,
            STEP_RIGHT * 2,
            STEP_UP_LEFT,
            STEP_LEFT,
        ];
        let relative_squares_that_should_be_fully_blocked = vec![
            STEP_DOWN * 2,
            STEP_DOWN * 2 + STEP_RIGHT,
            STEP_DOWN * 2 + STEP_RIGHT * 2,
            STEP_DOWN * 2 + STEP_RIGHT * 3,
        ];

        //   OOooOO
        //   oo@@ooOO
        //     ##%%##%%
        //     xxXXxxXX
        //
        //   ooo
        //   o@oo
        //    ####
        //    xxxx

        for step in relative_squares_that_should_be_fully_visible {
            let square = game.player_square() + step;
            assert!(
                game.square_is_fully_visible_to_player(square),
                "should be fully visible.  square: {}",
                square.to_string()
            );
        }
        for step in relative_squares_that_should_be_fully_blocked {
            let square = game.player_square() + step;
            assert!(
                !game.square_is_visible_to_player(square),
                "should be fully blocked.  square: {}",
                square.to_string()
            );
        }
    }

    #[test]
    fn test_faction_moves_closest_piece_to_player() {
        let mut game = set_up_game_with_player();
        let king_square = game.player_square() + STEP_UP_RIGHT * 3;
        game.place_new_king_pawn_faction(king_square);
        let test_square = king_square + STEP_DOWN_LEFT;
        assert_false!(game.square_is_empty(test_square));
        game.move_non_arrow_factions();
        assert!(game.square_is_empty(test_square));
    }

    #[test]

    fn test_pawn_reproduction_in_surrounded_squares() {
        let mut game = set_up_10x10_game();
        let test_square = point2(5, 5);
        let faction = game.get_new_faction();
        for step in ORTHOGONAL_STEPS {
            game.place_piece(Piece::new(OmniDirectionalPawn, faction), test_square + step);
        }
        assert_eq!(game.pieces.len(), 4);
        for _ in 0..=TURNS_TO_SPAWN_PAWN {
            game.tick_pawn_incubation();
        }
        assert!(game.pieces.len() > 4);
    }

    #[test]

    fn test_pawn_reproduction_does_not_apply_to_filled_squares() {
        let mut game = set_up_10x10_game();

        let test_square = point2(5, 5);
        let faction = game.get_new_faction();

        for step in ORTHOGONAL_STEPS {
            game.place_piece(Piece::new(OmniDirectionalPawn, faction), test_square + step);
        }
        game.place_piece(Piece::new(OmniDirectionalPawn, faction), test_square);

        assert_eq!(game.pieces.len(), 5);
        for _ in 0..=TURNS_TO_SPAWN_PAWN {
            game.tick_pawn_incubation();
        }
        assert_eq!(game.pieces.len(), 5);
    }

    #[test]

    fn test_faction_with_only_pawns_becomes_red_pawns() {
        let mut game = set_up_10x10_game();

        let king_square = point2(5, 5);
        let test_square = king_square + STEP_UP_RIGHT;
        game.place_new_king_pawn_faction(king_square);
        let placed_faction = game.get_piece_at(king_square).unwrap().faction;

        assert_eq!(
            game.get_piece_at(test_square).unwrap().faction,
            placed_faction
        );
        assert_ne!(
            game.get_piece_at(test_square).unwrap().faction,
            game.red_pawn_faction
        );

        game.capture_piece_at(king_square);
        game.on_turn_end();

        let the_piece = game.get_piece_at(test_square).unwrap();
        assert_ne!(the_piece.faction, placed_faction);
        assert_eq!(the_piece.faction, game.red_pawn_faction);
    }

    #[test]

    fn test_red_pawn_looks_red() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_red_pawn(square);
        game.draw_headless_now();
        let glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_world_square(square);
        assert_eq!(glyphs.get(0).unwrap().fg_color, RED_PAWN_COLOR);
    }

    #[test]

    fn test_red_pawns_dont_move_if_stable() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(0, 0));
        let center_square = point2(5, 5);
        let pawn_squares: SquareSet = ORTHOGONAL_STEPS
            .iter()
            .map(|&step| center_square + step)
            .collect();
        for &pawn_square in &pawn_squares {
            game.place_red_pawn(pawn_square);
        }
        game.move_non_arrow_factions();
        let found_pawn_squares: SquareSet = game.pieces.keys().cloned().collect();
        assert_eq!(pawn_squares, found_pawn_squares);
    }

    #[test]

    fn test_red_pawn_will_move_into_protection() {
        let mut game = set_up_10x10_game();
        let moving_pawn_square = point2(5, 5);
        let correct_end_square = moving_pawn_square + STEP_LEFT;
        game.place_red_pawn(correct_end_square + STEP_DOWN_LEFT);
        game.place_red_pawn(moving_pawn_square);
        game.move_piece_at_square_and_return_end_position_if_moved(moving_pawn_square);
        assert!(game.pieces.contains_key(&correct_end_square));
    }

    #[test]

    fn test_red_pawns_dont_try_to_capture_each_other() {
        let mut game = set_up_10x10_game();
        let start_square = point2(5, 5);

        for dx in 0..3 {
            for dy in 0..3 {
                let vec = vec2(dx, dy);
                game.place_red_pawn(start_square + vec);
            }
        }
        let num_pieces_at_start = game.pieces.len();
        game.move_piece_at_square_and_return_end_position_if_moved(start_square);
        assert_eq!(num_pieces_at_start, game.pieces.len());
    }

    #[test]

    fn test_red_pawns_try_to_not_pack_tightly() {
        let mut game = set_up_10x10_game();
        let pawn_squares = (4..=6).flat_map(|x| (4..=5).map(move |y| point2(x, y)));
        for square in pawn_squares {
            game.place_red_pawn(square);
        }
        assert_eq!(game.piece_type_count(OmniDirectionalPawn), 6);
        let test_square = point2(5, 5);
        assert_false!(game.square_is_empty(test_square));
        game.move_piece_at_square_and_return_end_position_if_moved(test_square);
        assert!(game.square_is_empty(test_square));
    }

    #[test]

    fn test_red_pawns_slightly_prefer_movement_over_non_movement() {
        let mut game = set_up_10x10_game();
        let pawn_square = point2(5, 5);
        game.place_red_pawn(pawn_square);
        assert_false!(game.square_is_empty(pawn_square));
        game.move_piece_at_square_and_return_end_position_if_moved(pawn_square);
        assert!(game.square_is_empty(pawn_square));
    }

    #[test]

    fn test_death_cube_kills_player() {
        let mut game = set_up_game_with_player();
        let death_cube_start_pos = (game.player_square() + STEP_LEFT).to_f32();
        let death_cube_start_vel = STEP_RIGHT.to_f32() * 20.0;
        game.place_linear_death_cube(death_cube_start_pos, death_cube_start_vel);
        assert!(game.player_is_alive());
        game.tick_death_cubes(Duration::from_secs_f32(1.0));
        assert_false!(game.player_is_alive());
    }

    #[test]

    fn test_death_cube_kills_rook() {
        let mut game = set_up_10x10_game();
        let rook_square = point2(5, 5);
        game.place_piece(Piece::new(Rook, game.default_enemy_faction), rook_square);
        let death_cube_start_pos = (rook_square + STEP_LEFT).to_f32();
        let death_cube_start_vel = STEP_RIGHT.to_f32() * 20.0;
        game.place_linear_death_cube(death_cube_start_pos, death_cube_start_vel);
        assert!(!game.pieces.is_empty());
        game.tick_death_cubes(Duration::from_secs_f32(1.0));
        assert!(game.pieces.is_empty());
    }

    #[test]

    fn test_death_cube_can_be_seen() {
        let mut game = set_up_10x10_game();
        let test_square = point2(5, 5);

        game.draw_headless_now();
        assert!(game
            .graphics
            .screen
            .get_screen_glyphs_at_world_square(test_square)
            .looks_solid());

        let death_cube_start_pos = test_square.to_f32() + vec2(0.3, 0.0);
        game.place_linear_death_cube(death_cube_start_pos, vec2(0.0, 0.0));

        game.draw_headless_now();
        assert!(!game
            .graphics
            .screen
            .get_screen_glyphs_at_world_square(test_square)
            .looks_solid());
    }

    #[test]

    fn test_death_cube_moves() {
        let mut game = set_up_10x10_game();
        game.place_linear_death_cube(point2(3.0, 4.5), vec2(1.0, 0.0));
        game.tick_death_cubes(Duration::from_secs_f32(1.0));
        assert_about_eq!(game.death_cubes[0].position.x, 4.0);
    }

    #[test]

    fn test_death_cube_visually_moves() {
        let mut game = set_up_10x10_game();
        let test_square = point2(5, 5);

        game.draw_headless_now();
        let get_solidness = |game: &Game| -> Vec<bool> {
            (0..4)
                .map(|dx| {
                    game.graphics
                        .screen
                        .get_screen_glyphs_at_world_square(test_square + STEP_RIGHT * dx)
                        .looks_solid()
                })
                .collect()
        };

        let squares_that_look_solid = get_solidness(&game);
        assert_eq!(squares_that_look_solid, vec![true, true, true, true]);

        let death_cube_start_pos = test_square.to_f32() + vec2(0.3, 0.0);
        game.place_linear_death_cube(death_cube_start_pos, vec2(1.0, 0.0));

        game.draw_headless_now();
        let squares_that_look_solid = get_solidness(&game);
        assert_eq!(squares_that_look_solid, vec![false, false, true, true]);

        game.tick_death_cubes(Duration::from_secs_f32(1.0));

        game.draw_headless_now();
        let squares_that_look_solid = get_solidness(&game);
        assert_eq!(squares_that_look_solid, vec![true, false, false, true]);
    }

    #[test]

    fn test_death_cubes_change_color_over_time() {
        let mut game = set_up_10x10_game();
        let test_square = point2(3, 3);
        game.place_linear_death_cube(test_square.to_f32(), vec2(0.0, 0.0));
        game.draw_headless_at_duration_from_start(Duration::from_secs_f32(1.0));
        let cube_color_1 = game
            .graphics
            .screen
            .get_screen_glyphs_at_world_square(test_square)
            .get_solid_color()
            .unwrap();
        game.draw_headless_at_duration_from_start(Duration::from_secs_f32(1.23432));
        let cube_color_2 = game
            .graphics
            .screen
            .get_screen_glyphs_at_world_square(test_square)
            .get_solid_color()
            .unwrap();
        assert_ne!(cube_color_1, cube_color_2);
    }

    #[test]

    fn test_death_cube_turret_shoots_death_cubes() {
        let mut game = set_up_10x10_game();
        let turret_square = point2(5, 5);
        game.place_death_turret(turret_square);
        assert!(game.death_cubes.is_empty());
        game.tick_realtime_effects(Duration::from_secs_f32(5.0));
        assert!(!game.death_cubes.is_empty());

        game.tick_death_cubes(Duration::from_secs_f32(1.0));
        assert!(!game.pieces.is_empty());
    }

    #[test]

    fn test_death_cubes_vanish_when_off_board() {
        let mut game = set_up_nxn_game(5);
        game.place_linear_death_cube(point2(4.9, 4.9), vec2(20.0, 0.0));
        assert!(!game.death_cubes.is_empty());
        game.tick_death_cubes(Duration::from_secs_f32(5.0));
        assert!(game.death_cubes.is_empty());
    }

    #[test]

    fn test_player_blink() {
        let mut game = set_up_game_with_player();
        let start_pos = game.player_square();
        game.player_blink(STEP_RIGHT);
        let square_blink_dist = (game.player_square() - start_pos).square_length();
        assert!(square_blink_dist > 1);
    }

    #[test]

    fn test_blink_is_also_strafe() {
        let mut game = set_up_game_with_player();
        let start_pos = game.player_square();
        game.raw_set_player_faced_direction(STEP_UP.into());
        game.player_blink(STEP_RIGHT);
        assert_eq!(game.player_faced_direction(), STEP_UP.into());
    }

    #[test]

    fn test_player_no_blink_through_block() {
        let mut game = set_up_game_with_player();
        let start_pos = game.player_square();
        let block_pos = game.player_square() + STEP_RIGHT * 3;
        game.place_block(block_pos);
        game.player_blink(STEP_RIGHT);
        assert_eq!(game.player_square(), block_pos + STEP_LEFT);
    }

    #[test]

    fn test_blink_leaves_blue_trail() {
        let mut game = set_up_game_with_player();
        let start_pos = game.player_square();
        game.player_blink(STEP_RIGHT);
        let end_pos = game.player_square();

        // TODO: why is the duration necessary? (might be just a random empty block)
        game.draw_headless_at_duration_from_start(Duration::from_secs_f32(0.1));

        // check all the intermediate squares, but only require at least one of the two characters in each square has a particle
        (start_pos.x + 1..end_pos.x).for_each(|x| {
            let square = point2(x, start_pos.y);
            let glyphs = game
                .graphics
                .screen
                .get_screen_glyphs_at_world_square(square);
            //assert!(!glyphs.looks_solid());
            assert!(
                glyphs[0].fg_color == BLINK_EFFECT_COLOR
                    || glyphs[1].fg_color == BLINK_EFFECT_COLOR
            );
        })
    }

    #[test]

    fn test_overlapping_blink_trails_have_uniform_color() {
        let mut game = set_up_game_with_player();
        let start_pos = game.player_square();
        game.player_blink(STEP_RIGHT);
        let end_pos = game.player_square();
        game.player_blink(STEP_LEFT);

        let blink_step = end_pos - start_pos;

        for i in 0..20 {
            // TODO: why is the duration necessary? (might be just randomness)
            let delta = Duration::from_secs_f32(i as f32 * 0.1);
            game.draw_headless_at_duration_from_start(delta);
            //game.draw_headless_now();
            for dx in 1..blink_step.x {
                let square = start_pos + vec2(dx, 0);
                let glyphs = game
                    .graphics
                    .screen
                    .get_screen_glyphs_at_world_square(square);
                // There might not be particles in every character square.  Don't test the empty ones
                if !glyphs[0].looks_solid() {
                    assert_eq!(glyphs[0].fg_color, BLINK_EFFECT_COLOR);
                }
                if !glyphs[1].looks_solid() {
                    assert_eq!(glyphs[1].fg_color, BLINK_EFFECT_COLOR);
                }
            }
        }
    }

    #[test]

    fn test_try_to_blink_but_blocked() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(0, 0));
        game.player_blink(STEP_LEFT);
    }

    #[test]

    fn test_protected_piece_has_fully_colored_background() {
        let mut game = set_up_10x10_game();
        let square1 = point2(5, 5);
        game.place_red_pawn(square1);
        game.place_red_pawn(square1 + STEP_UP_RIGHT);
        game.draw_headless_now();
        let pawn_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_world_square(square1);

        assert_eq!(pawn_glyphs[0].bg_color, DANGER_SQUARE_COLOR);
        assert_eq!(pawn_glyphs[1].bg_color, DANGER_SQUARE_COLOR);
    }

    #[test]

    fn test_blink_range_upgrade() {
        let mut game = set_up_nxn_game(20);
        let start = point2(5, 5);
        game.place_player(start);
        let start_blink_range = 3;
        game.player().blink_range = start_blink_range;
        game.player_blink(STEP_RIGHT);
        assert_eq!(
            (start - game.player_square()).square_length().sqrt(),
            start_blink_range as i32
        );

        let upgrade_square = point2(5, 6);
        game.place_upgrade(BlinkRange, upgrade_square);

        game.move_player_to(upgrade_square);

        game.player_blink(STEP_RIGHT);

        assert_eq!(
            (upgrade_square - game.player_square())
                .square_length()
                .sqrt(),
            start_blink_range as i32 + 1
        );
    }

    #[test]

    fn test_kings_drop_upgrades() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_piece(Piece::new(King, game.default_enemy_faction), square);
        assert!(game.upgrades.is_empty());
        game.capture_piece_at(square);
        assert_eq!(game.upgrades.get(&square).unwrap(), &BlinkRange);
    }

    #[test]

    fn test_soldier() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        let piece = Piece::from_type(OmniDirectionalSoldier);
        game.place_piece(piece, square);
        assert_false!(piece.can_turn());
        assert!(game
            .on_board_move_squares_for_piece_at(square, false)
            .contains(&(square + STEP_RIGHT)));
        assert!(game
            .guarded_squares_for_piece_at(square)
            .contains(&(square + STEP_RIGHT)))
    }

    #[test]

    fn test_turning_soldier_turns_toward_player() {
        let mut game = set_up_10x10_game();
        let player_square = point2(5, 5);
        game.place_player(player_square);

        let soldier_square = player_square + STEP_LEFT * 3;

        game.place_piece(Piece::from_type(TurningSoldier), soldier_square);
        game.get_mut_piece_at(soldier_square)
            .unwrap()
            .set_faced_direction(STEP_UP.into());

        assert_eq!(
            game.move_options_for_piece_at(soldier_square),
            vec![soldier_square + STEP_UP]
        );
        game.move_all_pieces();
        assert_eq!(
            game.move_options_for_piece_at(soldier_square),
            vec![soldier_square + STEP_RIGHT]
        );
    }

    #[test]

    fn test_turning_pawn_turns_toward_player() {
        let mut game = set_up_10x10_game();
        let player_square = point2(5, 5);
        game.place_player(player_square);

        let square = player_square + STEP_LEFT * 3;
        game.place_piece(Piece::from_type(TurningPawn), square);

        game.get_mut_piece_at(square)
            .unwrap()
            .set_faced_direction(STEP_UP.into());

        assert_eq!(
            game.move_options_for_piece_at(square),
            vec![square + STEP_UP]
        );
        game.move_all_pieces();
        assert_eq!(
            game.move_options_for_piece_at(square),
            vec![square + STEP_RIGHT]
        );
    }

    #[test]

    fn test_player_radial_attack() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_player(square);
        KING_STEPS
            .iter()
            .for_each(|&step: &WorldStep| game.place_piece(Piece::pawn(), square + step));
        game.place_piece(Piece::pawn(), square + STEP_RIGHT * 2);
        assert_eq!(game.pieces.len(), 9);
        game.do_player_radial_attack();
        assert_eq!(game.pieces.len(), 1);
    }

    #[test]

    fn test_player_spear_attack() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_player(square);
        game.player().faced_direction = STEP_RIGHT.into();

        game.place_piece(Piece::pawn(), square + STEP_RIGHT * 2);
        assert_eq!(game.pieces.len(), 1);
        game.do_player_spear_attack();
        assert_eq!(game.pieces.len(), 0);
    }

    #[test]

    fn test_arrow_travels() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_arrow(square, STEP_RIGHT.into());
        assert!(game.is_arrow_at(square));
        assert_eq!(game.arrows().len(), 1);
        game.tick_projectile_arrows();
        assert!(game.is_arrow_at(square + STEP_RIGHT));
        assert_eq!(game.arrows().len(), 1);
    }

    #[test]

    fn test_draw_arrows() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_arrow(square, STEP_RIGHT.into());
        game.draw_headless_now();
        let glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_world_square(square);
        assert_false!(glyphs.looks_solid());
    }

    #[test]

    fn test_player_shoot_arrow() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_player(square);
        game.player().faced_direction = STEP_RIGHT.into();
        assert!(game.arrows().is_empty());
        game.do_player_shoot_arrow();
        assert_false!(game.arrows().is_empty());
    }

    #[test]

    fn test_player_shoot_arrow_diagonal() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_player(square);
        game.player().faced_direction = STEP_UP_RIGHT.into();
        assert!(game.arrows().is_empty());
        game.do_player_shoot_arrow();
        assert_false!(game.arrows().is_empty());
    }

    #[test]

    fn test_player_can_capture_arrow() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_player(square + STEP_UP);
        game.place_arrow(square, STEP_RIGHT.into());
        assert_false!(game.arrows().is_empty());
        game.move_player_to(square);
        assert!(game.arrows().is_empty());
    }

    #[test]

    fn test_player_can_step_through_portal() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(5, 5));
        game.place_single_sided_one_way_portal(
            WorldSquareWithOrthogonalDir::from_square_and_step(point2(5, 5), STEP_RIGHT),
            WorldSquareWithOrthogonalDir::from_square_and_step(point2(5, 7), STEP_LEFT),
        );
        game.try_slide_player(STEP_RIGHT).expect("move player");

        assert_eq!(game.player_square(), point2(5, 7));
        assert_eq!(game.player_faced_direction(), STEP_LEFT.into());
    }

    #[test]

    fn test_portal_steps() {
        let mut game = set_up_10x10_game();
        let entrance_step =
            WorldSquareWithOrthogonalDir::from_square_and_step(point2(2, 6), STEP_UP);
        let exit_step =
            WorldSquareWithOrthogonalDir::from_square_and_step(point2(5, 2), STEP_RIGHT);
        game.place_single_sided_one_way_portal(entrance_step, exit_step);
        assert_eq!(
            game.portal_aware_single_step(entrance_step.into()).unwrap(),
            exit_step.into()
        );
    }

    #[test]

    fn test_move_through_multiple_portals() {
        let mut game = set_up_10x10_game();
        let start = WorldSquareWithOrthogonalDir::from_square_and_step(point2(2, 6), STEP_RIGHT);
        let mid = WorldSquareWithOrthogonalDir::from_square_and_step(point2(5, 5), STEP_DOWN);
        let end = WorldSquareWithOrthogonalDir::from_square_and_step(point2(5, 2), STEP_LEFT);
        game.place_single_sided_one_way_portal(start, mid);
        game.place_single_sided_one_way_portal(mid, end);
        assert_eq!(
            game.multiple_portal_aware_steps(start.into(), 2).unwrap(),
            end.into()
        );
    }

    #[test]

    fn test_arrow_through_portal() {
        let mut game = set_up_10x10_game();
        let start = WorldSquareWithOrthogonalDir::from_square_and_step(point2(2, 6), STEP_RIGHT);
        let end = WorldSquareWithOrthogonalDir::from_square_and_step(point2(5, 2), STEP_DOWN);
        game.place_single_sided_one_way_portal(start, end);
        game.place_arrow(start.square(), KingWorldStep::new(start.direction().into()));
        game.tick_projectile_arrows();
        assert_eq!(game.arrows().get(&end.square()), Some(&STEP_DOWN.into()));
    }

    #[test]

    fn test_piece_capture_through_portal() {
        let mut game = set_up_10x10_game();
        let enemy_square = point2(5, 5);
        let player_square = point2(2, 2);
        let entrance = WorldSquareWithOrthogonalDir::from_square_and_step(enemy_square, STEP_RIGHT);
        let exit = WorldSquareWithOrthogonalDir::from_square_and_step(player_square, STEP_DOWN);
        game.place_single_sided_one_way_portal(entrance, exit);
        game.place_piece(Piece::from_type(OmniDirectionalSoldier), enemy_square);
        game.place_player(player_square);
        assert!(game.player_is_alive());
        game.move_all_pieces();
        assert_false!(game.player_is_alive());
        assert!(game.is_non_player_piece_at(player_square));
    }

    #[test]

    fn test_spear_stab_through_portal() {
        let mut game = set_up_10x10_game();
        let enemy_square = point2(5, 5);
        let player_square = point2(2, 2);
        let entrance =
            WorldSquareWithOrthogonalDir::from_square_and_step(player_square, STEP_RIGHT);
        let exit = WorldSquareWithOrthogonalDir::from_square_and_step(enemy_square, STEP_DOWN);

        game.place_single_sided_one_way_portal(entrance, exit);

        game.place_piece(Piece::from_type(OmniDirectionalSoldier), enemy_square);

        game.place_player(player_square);
        game.player().faced_direction = entrance.direction().to_step::<WorldStep>().into();

        assert_false!(game.pieces.is_empty());
        game.do_player_spear_attack();
        assert!(game.pieces.is_empty());
    }

    #[test]

    fn test_arrow_does_not_turn_toward_player() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(5, 5));
        let arrow_square = point2(3, 5);
        game.place_arrow(arrow_square, STEP_LEFT.into());
        game.move_all_pieces();
        assert!(game.is_arrow_at(arrow_square + STEP_LEFT));
        assert_eq!(
            game.arrows().get(&(arrow_square + STEP_LEFT)),
            Some(&STEP_LEFT.into())
        );
    }

    #[test]

    fn test_see_through_portal() {
        let mut game = set_up_10x10_game();

        let player_square = point2(2, 2);
        game.place_player(player_square);

        let enemy_square = player_square + STEP_UP * 2;
        game.place_piece(Piece::from_type(OmniDirectionalSoldier), enemy_square);

        let entrance = WorldSquareWithOrthogonalDir::from_square_and_step(
            player_square + STEP_RIGHT,
            STEP_RIGHT,
        );
        let exit =
            WorldSquareWithOrthogonalDir::from_square_and_step(enemy_square + STEP_DOWN, STEP_UP);
        game.place_single_sided_one_way_portal(entrance, exit);

        game.draw_headless_now();
        let visible_enemy_square = player_square + STEP_RIGHT * 3;

        // game.graphics.draw_string_to_draw_buffer(enemy_square + STEP_UP, "123456789");
        // game.graphics
        //     .draw_string_to_draw_buffer(player_square + STEP_RIGHT, "123456789");
        // game.update_screen_from_draw_buffer_headless();

        // game.graphics.print_draw_buffer(
        //     world_square_to_left_world_character_square(player_square),
        //     5,
        // );
        // game.graphics.print_screen_buffer();

        let fov = game.player_field_of_view();

        assert_eq!(fov.transformed_sub_fovs().len(), 1);
        assert_eq!(
            fov.rasterized()
                .times_absolute_square_is_visible(enemy_square),
            2
        );
        assert_eq!(
            game.graphics
                .screen
                .get_screen_glyphs_at_world_square(visible_enemy_square)
                .to_clean_string(),
            game.graphics
                .get_drawable_for_square_from_draw_buffer(enemy_square)
                .unwrap()
                .to_glyphs()
                .to_clean_string()
        );
    }

    #[ignore = "Turns out not a great feature"]
    #[test]

    fn test_screen_jumps_to_player_if_far_from_screen_center() {
        let mut game = set_up_nxn_game(20);
        let start_square = point2(5, 5);
        let recenter_radius = 10;
        let square2 = point2(start_square.x + recenter_radius - 1, 5);
        let square3 = point2(start_square.x + recenter_radius + 1, 5);
        game.place_player(start_square);
        game.borrow_graphics_mut()
            .screen
            .set_screen_center_by_world_square(start_square);
        game.draw_headless_now();
        assert_eq!(
            game.graphics.screen.screen_center_as_world_square(),
            game.player_square()
        );

        assert_false!(game
            .graphics
            .screen
            .get_screen_glyphs_at_world_square(game.player_square())
            .looks_solid());

        game.move_player_to(square2);
        game.draw_headless_now();
        assert_ne!(
            game.graphics.screen.screen_center_as_world_square(),
            game.player_square()
        );
        assert!(game
            .graphics
            .screen
            .get_screen_glyphs_at_world_square(game.player_square())
            .looks_solid());

        game.move_player_to(square3);
        game.draw_headless_now();
        assert_eq!(
            game.graphics.screen.screen_center_as_world_square(),
            game.player_square()
        );
        assert_false!(game
            .graphics
            .screen
            .get_screen_glyphs_at_world_square(game.player_square())
            .looks_solid());
    }

    #[test]

    fn test_observed_crash_from_one_pillar_shadow() {
        let mut game = set_up_nxn_game(20);
        let player_square = point2(0, 0);
        let block_offset = vec2(14, -5);
        game.place_player(player_square);
        game.place_block(player_square + block_offset);
        game.draw_headless_now();
        // shouldn't crash
    }

    #[test]

    fn test_observed_crash_from_seeing_back_of_portal() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(5, 5));
        let block_square = game.player_square() + STEP_RIGHT * 4;
        let entrance_square = block_square + STEP_UP_RIGHT;
        let exit_square = block_square + STEP_DOWN_RIGHT * 4;
        game.place_single_sided_one_way_portal(
            WorldSquareWithOrthogonalDir::from_square_and_step(entrance_square, STEP_DOWN),
            WorldSquareWithOrthogonalDir::from_square_and_step(exit_square, STEP_LEFT),
        );
        game.draw_headless_now();
    }

    #[test]

    fn test_observed_crash_from_being_near_portal() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(5, 5));
        game.place_single_sided_one_way_portal(
            WorldSquareWithOrthogonalDir::from_square_and_step(
                game.player_square() + STEP_DOWN_RIGHT,
                STEP_DOWN,
            ),
            WorldSquareWithOrthogonalDir::from_square_and_step(
                game.player_square() + STEP_DOWN_LEFT * 3,
                STEP_RIGHT,
            ),
        );
        game.draw_headless_now();
    }

    #[test]

    fn test_observed_crash_from_being_near_portal__2() {
        let mut game = set_up_nxn_game(20);
        game.place_player(point2(5, 5));
        game.place_single_sided_one_way_portal(
            WorldSquareWithOrthogonalDir::from_square_and_step(
                game.player_square() + STEP_DOWN_LEFT,
                STEP_DOWN,
            ),
            WorldSquareWithOrthogonalDir::from_square_and_step(
                game.player_square() + STEP_DOWN_LEFT * 3,
                STEP_RIGHT,
            ),
        );
        game.draw_headless_now();
    }

    #[test]

    fn test_see_through_portal_while_next_to_it() {
        let mut game = set_up_10x10_game();

        let player_square = point2(5, 5);
        game.place_player(player_square);

        let enemy_square = player_square + STEP_RIGHT * 2;
        game.place_piece(Piece::from_type(OmniDirectionalSoldier), enemy_square);

        let entrance =
            WorldSquareWithOrthogonalDir::from_square_and_step(player_square, STEP_RIGHT);
        let exit = WorldSquareWithOrthogonalDir::from_square_and_step(enemy_square, STEP_RIGHT);
        game.place_single_sided_one_way_portal(entrance, exit);

        game.draw_headless_now();
        let correct_apparent_enemy_square = player_square + STEP_RIGHT;

        // game.graphics.draw_string_to_draw_buffer(enemy_square + STEP_UP, "123456789");
        // game.graphics
        //     .draw_string_to_draw_buffer(player_square + STEP_RIGHT, "123456789");
        // game.update_screen_from_draw_buffer_headless();

        // game.graphics.print_draw_buffer(
        //     world_square_to_left_world_character_square(player_square),
        //     5,
        // );
        // game.graphics.print_screen_buffer();

        let fov = game.player_field_of_view();

        assert_eq!(fov.transformed_sub_fovs().len(), 1);
        assert_eq!(
            fov.rasterized()
                .times_absolute_square_is_visible(enemy_square),
            1
        );
        assert_eq!(
            game.graphics
                .screen
                .get_screen_glyphs_at_world_square(correct_apparent_enemy_square)
                .to_clean_string(),
            game.graphics
                .get_drawable_for_square_from_draw_buffer(enemy_square)
                .unwrap()
                .to_glyphs()
                .to_clean_string()
        );
    }

    #[test]

    fn test_portals_causing_shadows_at_certain_angles() {
        let mut game = set_up_nxn_game(20);

        let player_square = point2(10, 10);
        game.place_player(player_square);

        let entrance = WorldSquareWithOrthogonalDir::from_square_and_step(
            player_square + STEP_DOWN_RIGHT * 3,
            STEP_RIGHT,
        );
        let exit = entrance.with_offset(STEP_RIGHT * 2);
        game.place_single_sided_one_way_portal(entrance, exit);

        game.draw_headless_now();
        let square_that_should_be_visible = entrance.square() + STEP_DOWN_LEFT;

        // game.graphics.draw_string_to_draw_buffer(enemy_square + STEP_UP, "123456789");
        // game.graphics
        //     .draw_string_to_draw_buffer(player_square + STEP_RIGHT, "123456789");
        // game.update_screen_from_draw_buffer_headless();

        // game.graphics.print_draw_buffer(
        //     world_square_to_left_world_character_square(player_square),
        //     5,
        // );
        // game.graphics.screen.print_screen_buffer();

        let fov = game.rasterized_player_field_of_view();

        assert_eq!(
            fov.times_absolute_square_is_visible(square_that_should_be_visible),
            1
        );
        assert_ne!(
            game.graphics
                .get_drawable_for_square_from_draw_buffer(square_that_should_be_visible)
                .unwrap()
                .to_glyphs()
                .get_solid_color()
                .unwrap(),
            OUT_OF_SIGHT_COLOR
        );
    }

    #[test]

    fn test_rotate_screen() {
        let mut game = set_up_nxn_game(20);

        let player_square = point2(10, 10);
        game.place_player(player_square);
        game.raw_set_player_faced_direction(STEP_RIGHT.into());

        let step_to_enemy = STEP_RIGHT * 2;

        let enemy_square = player_square + step_to_enemy;
        game.place_piece(Piece::from_type(OmniDirectionalSoldier), enemy_square);

        game.draw_headless_now();
        let enemy_chars = game
            .graphics
            .get_drawable_for_square_from_draw_buffer(enemy_square)
            .unwrap()
            .to_glyphs()
            .to_clean_string();

        let player_square = game.player_square();

        assert_eq!(
            game.graphics
                .screen
                .get_screen_glyphs_at_world_square(enemy_square)
                .to_clean_string(),
            enemy_chars
        );

        let player_screen_char_square = game
            .graphics
            .screen
            .world_square_to_left_screen_buffer_character_square(player_square);

        assert_eq!(
            game.graphics
                .screen
                .get_screen_buffered_glyph(player_screen_char_square + STEP_RIGHT.cast_unit() * 4)
                .character,
            enemy_chars.chars().collect_vec()[0]
        );

        game.graphics
            .screen
            .rotate(NormalizedOrthoAngle::new_from_quarter_turns(3));

        game.draw_headless_now();

        assert_eq!(
            game.graphics
                .screen
                .get_screen_glyphs_at_world_square(enemy_square)
                .to_clean_string(),
            enemy_chars
        );
        let player_screen_char_square = game
            .graphics
            .screen
            .world_square_to_left_screen_buffer_character_square(player_square);

        assert_eq!(
            game.graphics
                .screen
                .get_screen_buffered_glyph(
                    player_screen_char_square + SCREEN_STEP_UP.cast_unit() * 2
                )
                .character,
            enemy_chars.chars().collect_vec()[0]
        );
    }

    #[test]

    fn test_screen_rotates_when_stepping_through_portal() {
        let mut game = set_up_10x10_game();
        let player_square = point2(5, 5);
        game.place_player(player_square);
        let entrance_step =
            WorldSquareWithOrthogonalDir::from_square_and_step(player_square, STEP_RIGHT);
        let exit_step = WorldSquareWithOrthogonalDir::from_square_and_step(
            player_square + STEP_DOWN_RIGHT * 3,
            STEP_UP,
        );
        game.place_single_sided_two_way_portal(entrance_step, exit_step);

        game.draw_headless_now();
        assert_eq!(
            game.graphics.screen.rotation(),
            NormalizedOrthoAngle::new_from_quarter_turns(0)
        );

        game.try_slide_player_by_direction(STEP_RIGHT.into(), 1)
            .ok();
        assert_eq!(
            game.portal_aware_single_step(entrance_step.into()).unwrap(),
            exit_step.into()
        );

        game.draw_headless_now();
        assert_eq!(
            game.graphics.screen.rotation(),
            NormalizedOrthoAngle::new_from_quarter_turns(1)
        );
    }

    #[test]

    fn test_move_relative_to_screen() {
        let mut game = set_up_10x10_game();
        let player_square = point2(5, 5);
        game.place_player(player_square);
        game.graphics
            .screen
            .rotate(NormalizedOrthoAngle::new_from_quarter_turns(1));
        game.try_slide_player_relative_to_screen(SCREEN_STEP_UP)
            .expect("slide");
        assert_eq!(game.player_square(), player_square + STEP_LEFT);
    }

    #[test]

    fn test_blink_relative_to_screen() {
        let mut game = set_up_10x10_game();
        let player_square = point2(5, 5);
        game.place_player(player_square);
        game.graphics
            .screen
            .rotate(NormalizedOrthoAngle::new_from_quarter_turns(1));
        game.player_blink_relative_to_screen(SCREEN_STEP_UP);
        assert_eq!(game.player_square().y, player_square.y);
        assert!(game.player_square().x < player_square.x);
    }

    #[test]

    fn test_rotated_shadows() {
        let player_square = point2(5, 5);

        let mut unrotated_game = set_up_10x10_game();
        let mut rotated_game = set_up_10x10_game();
        rotated_game
            .graphics
            .screen
            .rotate(NormalizedOrthoAngle::new_from_quarter_turns(-1));

        let mut games = [unrotated_game, rotated_game];
        let shadow_glyphs: Vec<DoubleGlyph> = games
            .iter_mut()
            .map(|game: &mut Game| {
                game.place_player(player_square);
                let right_of_player = player_square
                    + game
                        .graphics
                        .screen
                        .screen_step_to_world_step(SCREEN_STEP_RIGHT);

                game.place_block(right_of_player);
                game.draw_headless_now();

                let shadow_screen_square = game
                    .graphics
                    .screen
                    .world_square_to_screen_buffer_square(player_square)
                    + SCREEN_STEP_UP_RIGHT;
                game.graphics
                    .screen
                    .get_glyphs_at_screen_square(shadow_screen_square)
            })
            .collect_vec();
        assert_eq!(
            shadow_glyphs[0].to_clean_string(),
            shadow_glyphs[1].to_clean_string()
        );
    }

    #[test]

    fn test_can_fail_to_walk_into_block_without_crashing() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(5, 5));
        game.place_block(game.player_square() + STEP_RIGHT);
        game.try_slide_player_relative_to_screen(SCREEN_STEP_RIGHT)
            .ok();
    }

    #[test]

    fn test_partial_shadows_are_drawn() {
        let player_square = point2(5, 5);
        let mut game = set_up_10x10_game();
        game.place_player(player_square);
        let right_of_player = player_square
            + game
                .graphics
                .screen
                .screen_step_to_world_step(SCREEN_STEP_RIGHT);

        game.place_block(right_of_player);
        game.draw_headless_now();

        let shadow_screen_square = game
            .graphics
            .screen
            .world_square_to_screen_buffer_square(player_square)
            + SCREEN_STEP_UP_RIGHT;
        let glyphs = game
            .graphics
            .screen
            .get_glyphs_at_screen_square(shadow_screen_square);
        assert_eq!("🭞🭚", glyphs.to_clean_string());
        assert_ne!(glyphs[0].fg_color, glyphs[1].bg_color);
    }

    fn set_up_player_just_left_of_portal_through_wall() -> Game {
        let player_square = point2(5, 5);
        let mut game = set_up_10x10_game();
        game.place_player(player_square);

        let entrance_square = player_square;
        let exit_square = entrance_square + STEP_RIGHT * 2;
        let entrance =
            WorldSquareWithOrthogonalDir::from_square_and_step(entrance_square, STEP_RIGHT);
        let exit = WorldSquareWithOrthogonalDir::from_square_and_step(exit_square, STEP_RIGHT);
        game.place_double_sided_two_way_portal(entrance, exit);

        let r = 2;
        let n = 2 * r + 1;
        (0..n).for_each(|i| {
            game.place_block(entrance_square + STEP_RIGHT + STEP_UP * r + STEP_DOWN * i)
        });
        game
    }

    #[test]

    fn test_do_not_see_block_through_portal() {
        let mut game = set_up_player_just_left_of_portal_through_wall();

        game.draw_headless_now();

        game.graphics.screen.print_screen_buffer();

        let up_right_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_UP_RIGHT);
        assert_false!(up_right_glyphs.looks_solid());
    }

    #[test]

    fn test_freestanding_portals_can_be_seamless() {
        let player_square = point2(5, 5);
        let mut game = set_up_10x10_game();
        game.place_player(player_square);
        game.graphics.tint_portals = false;

        let entrance_square = game.player_square();
        let exit_square = entrance_square + STEP_RIGHT * 3;
        let entrance =
            WorldSquareWithOrthogonalDir::from_square_and_step(entrance_square, STEP_RIGHT);
        let exit = WorldSquareWithOrthogonalDir::from_square_and_step(exit_square, STEP_RIGHT);
        game.place_double_sided_two_way_portal(entrance, exit);

        game.draw_headless_now();

        game.graphics.screen.print_screen_buffer();

        let glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_UP_RIGHT);

        assert!(glyphs.looks_solid());
    }

    fn assert_screen_is_stable(mut game: &mut Game, n: usize) {
        let consecutive_frames = (0..n)
            .map(|_i| {
                game.draw_headless_now();
                game.graphics.screen.current_screen_state.clone()
            })
            .collect_vec();

        assert_eq!(consecutive_frames.len(), n);

        let mut the_iter = consecutive_frames.iter();
        let first_frame = the_iter.next().unwrap();
        the_iter.for_each(|f| {
            if f != first_frame {
                Screen::print_buffer_of_glyphs(first_frame);
                Screen::print_buffer_of_glyphs(f);
            }
            assert_eq!(f, first_frame)
        });

        assert!(consecutive_frames.iter().all_equal());
    }

    #[test]

    fn test_portal_edges_are_stable__dense_horizontal() {
        let player_square = point2(0, 5);
        let mut game = set_up_nxm_game(10, 30);
        game.place_player(player_square);

        game.place_dense_horizontal_portals(player_square, 3, 6);
        assert_screen_is_stable(&mut game, 5);
    }

    #[test]

    fn test_portal_edges_are_stable__simple_case() {
        let player_square = point2(0, 2);
        let mut game = set_up_nxm_game(5, 5);
        game.place_player(player_square);

        game.place_offset_rightward_double_sided_two_way_portal(player_square, STEP_RIGHT);

        assert_screen_is_stable(&mut game, 5);
    }

    #[test]

    fn test_portal_edges_are_stable__two_deep() {
        let player_square = point2(0, 2);
        let mut game = set_up_nxm_game(5, 10);
        game.place_player(player_square);

        game.place_dense_horizontal_portals(player_square, 1, 2);

        assert_screen_is_stable(&mut game, 10);
    }

    fn place_portal_capping_block_bar(game: &mut Game, square: WorldSquare, length: u32) {
        game.place_offset_rightward_double_sided_two_way_portal(
            square + STEP_LEFT,
            STEP_RIGHT * length as i32,
        );
        (0..length)
            .map(|i| square + STEP_RIGHT * i as i32)
            .for_each(|abs_square| game.place_block(abs_square));
    }

    #[test]

    fn test_portal_drawn_in_correct_order_over_partially_visible_block() {
        let player_square = point2(5, 5);
        let test_square = SCREEN_STEP_DOWN_RIGHT + SCREEN_STEP_RIGHT * 2;

        let mut game = set_up_nxm_game(10, 20);
        game.place_player(player_square);
        place_portal_capping_block_bar(&mut game, player_square + STEP_DOWN_RIGHT + STEP_RIGHT, 5);

        game.draw_headless_now();

        game.graphics.screen.print_screen_buffer();
        let test_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(test_square);

        assert_eq!(test_glyphs.to_clean_string(), "🬹🭑");
        assert_eq!(test_glyphs.map(|g| g.bg_color), [BLOCK_FG; 2]);
    }

    #[ignore = "May or may not want this"]
    #[test]

    fn test_shadow_arc_squashed_by_portal_still_looks_connected() {
        let player_square = point2(5, 5);
        let test_square = SCREEN_STEP_DOWN_RIGHT + SCREEN_STEP_RIGHT * 3;

        let mut game = set_up_nxm_game(10, 10);
        game.place_player(player_square);

        let bar_left_end = player_square + STEP_DOWN_RIGHT + STEP_RIGHT;
        game.place_offset_rightward_double_sided_two_way_portal(
            bar_left_end + STEP_LEFT,
            STEP_RIGHT * 20,
        );
        (0..2)
            .map(|i| bar_left_end + STEP_RIGHT * i)
            .for_each(|abs_square| game.place_block(abs_square));

        game.draw_headless_now();

        game.graphics.screen.print_screen_buffer();
        let test_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(test_square);

        assert_eq!(test_glyphs.to_clean_string(), "🬹🭑");
        todo!();
    }

    #[test]

    fn test_set_floor_color() {
        let mut game = set_up_nxm_game(10, 10);
        game.draw_headless_now();
        let default_floor_color: RGB8 = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_ZERO)
            .get_solid_color()
            .unwrap();
        assert_ne!(default_floor_color, RED);

        game.graphics.set_solid_floor_color(RED);
        game.draw_headless_now();
        let new_floor_color: RGB8 = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_ZERO)
            .get_solid_color()
            .unwrap();
        assert_eq!(new_floor_color, RED);
    }

    #[test]

    fn test_portals_tint_views() {
        let test_square: ScreenBufferStep = SCREEN_STEP_RIGHT * 3;
        let player_square: WorldSquare = point2(3, 3);
        let mut game = set_up_10x10_game();
        game.place_player(player_square);
        game.graphics.set_solid_floor_color(GREY);
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();
        let get_color = |game: &Game| -> RGB8 {
            game.graphics
                .screen
                .get_screen_glyphs_at_visual_offset_from_center(test_square)
                .get_solid_color()
                .unwrap()
        };
        let start_color: RGB8 = get_color(&game);

        assert_eq!(start_color.b, start_color.r);

        game.place_offset_rightward_double_sided_two_way_portal(player_square, STEP_UP * 5);
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();

        let end_color: RGB8 = get_color(&game);

        assert!(end_color.b < end_color.r, "Should tint red");
    }

    #[test]

    fn test_horizontal_wide_portal_has_no_internal_defects() {
        let mut game = set_up_nxm_game(30, 30);
        let player_square: WorldSquare = point2(3, 15);
        game.place_player(player_square);

        let entrance = WorldSquareWithOrthogonalDir::from_square_and_step(
            player_square + STEP_RIGHT * 2 + STEP_UP * 2,
            STEP_RIGHT,
        );
        let exit = entrance.stepped_n(5);
        game.place_wide_portal(entrance, exit, 5);

        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();

        let top_left_test_square = SCREEN_STEP_RIGHT * 3;

        (0..4).for_each(|dx| {
            (0..3).for_each(|dy_down| {
                let test_square =
                    top_left_test_square + SCREEN_STEP_RIGHT * dx + SCREEN_STEP_DOWN * dy_down;
                let glyphs = game
                    .graphics
                    .screen
                    .get_screen_glyphs_at_visual_offset_from_center(test_square);
                assert!(glyphs.looks_solid(), "offset: ({},{})", dx, dy_down);
            })
        });
    }

    #[test]

    fn test_horizontal_wide_portal_has_smooth_edge() {
        let mut game = set_up_nxm_game(16, 10);
        let player_square: WorldSquare = point2(3, game.board_size.height() as i32 / 2);
        game.place_player(player_square);

        let entrance = WorldSquareWithOrthogonalDir::from_square_and_step(
            player_square + STEP_RIGHT * 0 + STEP_UP * 2,
            STEP_RIGHT,
        );
        let exit = entrance.stepped_n(2);
        game.place_wide_portal(entrance, exit, 5);

        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();
        let test_relative_squares_on_screen_top_to_bottom = vec![
            ScreenBufferStep::new(1, -4),
            ScreenBufferStep::new(1, -3),
            ScreenBufferStep::new(1, 3),
            ScreenBufferStep::new(1, 4),
        ];
        let test_steps_in_world_top_to_bottom = test_relative_squares_on_screen_top_to_bottom
            .iter()
            .map(|&screen_step| game.graphics.screen.screen_step_to_world_step(screen_step))
            .collect_vec();

        let fov = game.player_field_of_view();

        debug_print_fov_as_absolute(&fov, 5);

        test_steps_in_world_top_to_bottom
            .iter()
            .map(|&world_step| {
                fov.rasterized()
                    .absolute_top_down_portal_entrance_shapes_at_relative_square(world_step)
            })
            .for_each(|visibilities_of_rel_square: Vec<SquareVisibility>| {
                let vis1: SquareVisibility = visibilities_of_rel_square[0];
                let vis2: SquareVisibility = visibilities_of_rel_square[1];
                assert!(vis1.about_complementary(vis2));
                assert!(vis1.is_visually_complementary_to(vis2));
            });

        let correct_strings_top_to_bottom = vec!["🭋█", "🭅█", "🭖█", "🭦█"];

        (0..test_relative_squares_on_screen_top_to_bottom.len()).for_each(|i| {
            assert_eq!(
                game.graphics
                    .screen
                    .get_screen_glyphs_at_visual_offset_from_center(
                        test_relative_squares_on_screen_top_to_bottom[i]
                    )
                    .to_clean_string(),
                correct_strings_top_to_bottom[i]
            )
        });
    }

    #[test]

    fn test_vertical_wide_portals_have_no_internal_defects() {
        let mut game = set_up_nxm_game(30, 30);
        let player_square: WorldSquare = point2(15, 5);
        game.place_player(player_square);

        let entrance = WorldSquareWithOrthogonalDir::from_square_and_step(
            player_square + STEP_UP * 2 + STEP_LEFT * 2,
            STEP_UP,
        );
        let exit = entrance.stepped_n(3);
        game.place_wide_portal(entrance, exit, 5);
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();

        let bottom_left_test_square = SCREEN_STEP_UP * 3;

        (0..3).for_each(|dx| {
            (0..4).for_each(|dy| {
                let test_square =
                    bottom_left_test_square + SCREEN_STEP_RIGHT * dx + SCREEN_STEP_UP * dy;
                let glyphs = game
                    .graphics
                    .screen
                    .get_screen_glyphs_at_visual_offset_from_center(test_square);
                assert!(glyphs.looks_solid(), "offset: ({},{})", dx, dy);
            })
        });
    }

    #[test]

    fn test_vertical_wide_portals_have_smooth_edges() {
        let mut game = set_up_nxm_game(30, 30);
        let player_square: WorldSquare = point2(15, 5);
        game.place_player(player_square);

        let entrance = WorldSquareWithOrthogonalDir::from_square_and_step(
            player_square + STEP_UP * 0 + STEP_LEFT * 2,
            STEP_UP,
        );
        let exit = entrance.stepped_n(3);
        game.place_wide_portal(entrance, exit, 5);
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();

        let test_squares_left_to_right = vec![
            ScreenBufferStep::new(-4, -1),
            ScreenBufferStep::new(-3, -1),
            ScreenBufferStep::new(3, -1),
            ScreenBufferStep::new(4, -1),
        ];
        let correct_strings_left_to_right = vec!["🬎🬎", "🭓█", "█🭞", "🬎🬎"];

        let bottom_left_test_square = SCREEN_STEP_UP * 3;

        (0..test_squares_left_to_right.len()).for_each(|i| {
            assert_eq!(
                game.graphics
                    .screen
                    .get_screen_glyphs_at_visual_offset_from_center(test_squares_left_to_right[i])
                    .to_clean_string(),
                correct_strings_left_to_right[i]
            )
        });
    }

    #[test]

    fn test_rotated_wide_portals_have_no_internal_defects() {
        let mut game = set_up_nxm_game(30, 30);
        let player_square: WorldSquare = point2(5, 5);
        game.place_player(player_square);

        let width = 5;
        let entrance = WorldSquareWithOrthogonalDir::from_square_and_step(
            player_square + STEP_RIGHT * 2 + STEP_UP * 2,
            STEP_RIGHT,
        );
        let exit = WorldSquareWithOrthogonalDir::from_square_and_step(
            entrance.square() + STEP_UP + STEP_RIGHT * 3,
            STEP_UP,
        );
        (0..width).for_each(|i| {
            game.place_double_sided_two_way_portal(
                entrance.strafed_right_n(i),
                exit.strafed_right_n(i),
            );
        });
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();

        let top_left_test_square = SCREEN_STEP_RIGHT * 3;

        (0..4).for_each(|dx| {
            (-2..3).for_each(|dy| {
                let test_square =
                    top_left_test_square + SCREEN_STEP_RIGHT * dx + SCREEN_STEP_UP * dy;
                let glyphs = game
                    .graphics
                    .screen
                    .get_screen_glyphs_at_visual_offset_from_center(test_square);
                assert!(glyphs.looks_solid(), "offset: ({},{})", dx, dy);
            })
        })
    }

    #[test]

    fn test_turning_wide_portal_specific_defect() {
        let mut game = set_up_nxm_game(30, 30);
        let player_square: WorldSquare = point2(5, 15);
        game.place_player(player_square);

        let width = 3;
        (0..width).for_each(|i| {
            let entrance = WorldSquareWithOrthogonalDir::from_square_and_step(
                player_square + STEP_RIGHT * 2 + STEP_UP,
                STEP_RIGHT,
            )
            .strafed_right_n(i);
            let exit = WorldSquareWithOrthogonalDir::from_square_and_step(
                game.player_square() + STEP_UP * 8 + STEP_RIGHT,
                STEP_UP,
            )
            .strafed_right_n(i);
            game.place_double_sided_two_way_portal(entrance, exit);
        });

        game.populate_draw_buffer(Instant::now());

        let rel_square = STEP_RIGHT * 3 + STEP_UP;
        let player_fov = game.rasterized_player_field_of_view();
        let visibilities_at_rel_square =
            player_fov.top_down_portals_for_relative_square(rel_square);
        assert_eq!(visibilities_at_rel_square.len(), 1);
        let maybe_drawable: Option<DrawableEnum> = game
            .graphics
            .maybe_drawable_for_rel_square_of_fov(&player_fov, rel_square);
        let glyphs = maybe_drawable.unwrap().to_glyphs();
        assert!(
            glyphs.looks_solid(),
            "The glyphs: {}, the chars: {}",
            glyphs.to_string(),
            glyphs.to_clean_string()
        );
    }

    #[test]

    fn test_player_rotates_with_portal() {
        let mut game = set_up_10x10_game();
        let player_square = point2(5, 5);
        game.place_player_with_direction(player_square, STEP_RIGHT);
        game.place_single_sided_one_way_portal(
            WorldSquareWithOrthogonalDir::from_square_and_step(player_square, STEP_RIGHT),
            WorldSquareWithOrthogonalDir::from_square_and_step(
                player_square + STEP_UP_RIGHT,
                STEP_UP,
            ),
        );
        game.draw_headless_now();
        //game.graphics.screen.print_screen_buffer();
        let before_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_ZERO);
        game.try_slide_player(STEP_RIGHT).expect("");
        game.draw_headless_now();
        //game.graphics.screen.print_screen_buffer();
        let after_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_ZERO);

        assert_eq!(
            before_glyphs.to_clean_string(),
            after_glyphs.to_clean_string()
        );
    }

    #[test]

    fn test_move_vertical_looks_correct() {
        let mut game = set_up_10x10_game();
        game.place_player_with_direction(point2(5, 5), STEP_UP);
        game.draw_headless_now();
        let before_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_ZERO);
        game.try_slide_player_relative_to_screen(SCREEN_STEP_UP)
            .expect("");
        let after_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_ZERO);
        assert_eq!(
            before_glyphs.to_clean_string(),
            after_glyphs.to_clean_string()
        );
        assert_eq!(
            before_glyphs.to_clean_string(),
            Glyph::get_glyphs_for_player(STEP_UP.into()).to_clean_string()
        );
    }

    #[test]

    fn test_player_seen_through_portal_is_rotated_correctly() {
        let mut game = set_up_10x10_game();
        let player_square = point2(5, 5);
        game.place_player_with_direction(player_square, STEP_RIGHT);
        game.place_single_sided_one_way_portal(
            WorldSquareWithOrthogonalDir::from_square_and_step(player_square, STEP_RIGHT),
            WorldSquareWithOrthogonalDir::from_square_and_step(player_square, STEP_DOWN),
        );
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();
        let player_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_ZERO);
        let seen_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_RIGHT);

        assert_ne!(
            player_glyphs.to_clean_string(),
            seen_glyphs.to_clean_string()
        );
        assert_eq!(seen_glyphs[0].character, '🢁');
    }

    #[test]

    fn test_player_diagonal_step_around_adjacent_portal() {
        for orthodir in ORTHOGONAL_STEPS {
            [-1, 1].into_iter().for_each(|i| {
                let otherdir = orthodir.quarter_rotated_ccw(i);
                let diagdir = orthodir + otherdir;

                let mut game = set_up_nxm_game(10, 25);
                let start_square = point2(5, 5);
                game.place_player(start_square);
                game.place_single_sided_one_way_portal(
                    WorldSquareWithOrthogonalDir::from_square_and_step(
                        game.player_square(),
                        orthodir,
                    ),
                    WorldSquareWithOrthogonalDir::from_square_and_step(
                        game.player_square() + STEP_RIGHT * 10,
                        STEP_UP,
                    ),
                );
                game.try_slide_player(diagdir).expect("slide");
                assert_eq!(game.player_square(), start_square + diagdir);
            });
        }
    }

    #[test]

    fn test_player_diagonal_step_into_off_adjacent_portal() {
        for orthodir in ORTHOGONAL_STEPS {
            for i in [-1, 1] {
                let strafedir = orthodir.quarter_rotated_ccw(i);
                let diagdir = orthodir + strafedir;

                let mut game = set_up_nxm_game(10, 25);
                let start_square = point2(5, 5);
                game.place_player(start_square);
                let far_square = game.player_square() + STEP_RIGHT * 10;
                game.place_single_sided_one_way_portal(
                    WorldSquareWithOrthogonalDir::from_square_and_step(
                        game.player_square() + strafedir,
                        orthodir,
                    ),
                    WorldSquareWithOrthogonalDir::from_square_and_step(far_square, STEP_UP),
                );
                game.try_slide_player(diagdir).expect("slide");
                assert_eq!(game.player_square(), far_square);
            }
        }
    }

    #[test]

    fn test_player_diagonal_step_into_matching_convex_corner_portal() {
        for left_orthodir in ORTHOGONAL_STEPS {
            let right_orthodir = left_orthodir.quarter_rotated_ccw(-1);
            let diagonaldir = left_orthodir + right_orthodir;
            let mut game = set_up_10x10_game();
            let start_square = point2(5, 5);
            game.place_player(start_square);

            let left_entrance = WorldSquareWithOrthogonalDir::from_square_and_step(
                start_square + left_orthodir,
                right_orthodir,
            );
            let exit_offset = 3;
            let left_exit = left_entrance.stepped().strafed_left_n(exit_offset);
            let right_entrance = WorldSquareWithOrthogonalDir::from_square_and_step(
                start_square + right_orthodir,
                left_orthodir,
            );
            let right_exit = right_entrance.stepped().stepped_n(exit_offset);

            game.place_single_sided_one_way_portal(left_entrance, left_exit);
            game.place_single_sided_one_way_portal(right_entrance, right_exit);

            game.try_slide_player(diagonaldir).expect("should slide");
            assert_eq!(game.player_square(), left_exit.square());
        }
    }

    #[test]

    fn test_player_diagonal_step_into_mismatched_convex_corner_portal() {
        for left_orthodir in ORTHOGONAL_STEPS {
            let right_orthodir = left_orthodir.quarter_rotated_ccw(-1);
            let diagonaldir = left_orthodir + right_orthodir;
            let mut game = set_up_10x10_game();
            let start_square = point2(5, 5);
            game.place_player(start_square);

            let left_entrance = WorldSquareWithOrthogonalDir::from_square_and_step(
                start_square + left_orthodir,
                right_orthodir,
            );
            let exit_offset = 3;
            let left_exit = left_entrance.stepped().strafed_left_n(exit_offset);
            let right_entrance = WorldSquareWithOrthogonalDir::from_square_and_step(
                start_square + right_orthodir,
                left_orthodir,
            );
            let right_exit = right_entrance.stepped().strafed_right_n(exit_offset);

            game.place_single_sided_one_way_portal(left_entrance, left_exit);
            game.place_single_sided_one_way_portal(right_entrance, right_exit);

            game.try_slide_player(diagonaldir)
                .expect_err("should not slide");
            assert_eq!(game.player_square(), start_square);
        }
    }

    #[test]

    fn test_player_diagonal_step_into_matching_concave_corner_portal() {
        for left_orthodir in ORTHOGONAL_STEPS {
            let right_orthodir = left_orthodir.quarter_rotated_ccw(-1);
            let diagonaldir = left_orthodir + right_orthodir;
            let mut game = set_up_10x10_game();
            let start_square = point2(5, 5);
            game.place_player(start_square);

            let left_entrance =
                WorldSquareWithOrthogonalDir::from_square_and_step(start_square, left_orthodir);
            let exit_offset = 3;
            let left_exit = left_entrance.stepped().strafed_right_n(exit_offset);
            let right_entrance =
                WorldSquareWithOrthogonalDir::from_square_and_step(start_square, right_orthodir);
            let right_exit = right_entrance.stepped().stepped_n(exit_offset);

            game.place_single_sided_one_way_portal(left_entrance, left_exit);
            game.place_single_sided_one_way_portal(right_entrance, right_exit);

            game.try_slide_player(diagonaldir).expect("should slide");
            assert_eq!(game.player_square(), left_exit.strafed_right().square());
        }
    }

    #[test]

    fn test_player_diagonal_step_into_mismatched_concave_corner_portal() {
        for left_orthodir in ORTHOGONAL_STEPS {
            let right_orthodir = left_orthodir.quarter_rotated_ccw(-1);
            let diagonaldir = left_orthodir + right_orthodir;
            let mut game = set_up_10x10_game();
            let start_square = point2(5, 5);
            game.place_player(start_square);

            let left_entrance =
                WorldSquareWithOrthogonalDir::from_square_and_step(start_square, left_orthodir);
            let exit_offset = 3;
            let left_exit = left_entrance.stepped().stepped_n(exit_offset);
            let right_entrance =
                WorldSquareWithOrthogonalDir::from_square_and_step(start_square, right_orthodir);
            let right_exit = right_entrance.stepped().stepped_n(exit_offset);

            game.place_single_sided_one_way_portal(left_entrance, left_exit);
            game.place_single_sided_one_way_portal(right_entrance, right_exit);

            game.try_slide_player(diagonaldir)
                .expect_err("should not slide");
            assert_eq!(game.player_square(), start_square);
        }
    }

    #[test]

    fn test_push_widget() {
        let mut game = set_up_10x10_game();
        let start_square = point2(5, 5);
        game.place_player(start_square);
        let widget_val = 4;
        game.place_widget(Widget::new(widget_val), start_square + STEP_RIGHT);
        game.try_slide_player(STEP_RIGHT).expect("should slide");
        assert_eq!(game.player_square(), start_square + STEP_RIGHT);
        let correct_widget_end_square = start_square + STEP_RIGHT * 2;
        assert!(game.widgets.contains_key(&correct_widget_end_square));
        assert_eq!(
            game.widgets.get(&correct_widget_end_square).unwrap().val(),
            widget_val
        );
    }

    #[test]

    fn test_draw_widget() {
        let mut game = set_up_10x10_game();
        let start_square = point2(5, 5);
        game.place_player(start_square);
        let widget_val = 4;
        game.place_widget(Widget::new(widget_val), start_square + STEP_RIGHT);
        game.draw_headless_now();
        let widget_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_RIGHT);
        assert_eq!(widget_glyphs.to_clean_string(), "④ ")
    }

    #[test]

    fn test_draw_floor_arrows() {
        let mut game = set_up_10x10_game();
        let start_square = point2(5, 5);
        game.place_player(start_square);
        let pushable_square = start_square + STEP_RIGHT;
        game.place_floor_push_arrow(pushable_square, UP);
        game.draw_headless_now();
        let glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_RIGHT);
        //⯬ ⯭ ⯮ ⯯
        assert_eq!(glyphs.to_clean_string(), "⯭ ")
    }

    #[test]

    fn test_floor_arrows_push_widget() {
        let mut game = set_up_10x10_game();
        let start_square = point2(5, 5);
        //game.place_player(start_square);
        let widget_square = start_square + STEP_RIGHT;
        game.place_widget(Widget::new(4), widget_square);
        game.place_floor_push_arrow(widget_square, UP);

        assert!(game.widgets.contains_key(&widget_square));
        game.tick_game_logic();
        assert!(game.widgets.contains_key(&(widget_square + STEP_UP)));
    }
    #[test]
    fn test_observed_fov_failure() {
        let mut game = set_up_10x10_game();
        let start_square = point2(0, 0);
        game.place_player(start_square);
        // let widget_square = start_square + STEP_RIGHT * 2 + STEP_UP;
        game.place_single_sided_one_way_portal((2, 1, RIGHT), (2, 4, UP));
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();
    }
    #[test]
    fn test_observed_fov_failure__2() {
        let mut game = set_up_10x10_game();
        let start_square = point2(0, 0);
        game.place_player(start_square);
        // let widget_square = start_square + STEP_RIGHT * 2 + STEP_UP;
        game.place_single_sided_one_way_portal((0, 1, RIGHT), (2, 4, UP));
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();
    }

    #[test]
    fn test_widget_visible_next_to_turning_portal() {
        let mut game = set_up_10x10_game();
        let start_square = point2(2, 2);
        game.place_player(start_square);
        let widget_square = start_square + STEP_RIGHT * 2 + STEP_UP;
        game.place_widget(Widget::new(4), widget_square);
        for i in 0..2 {
            game.place_single_sided_one_way_portal(
                WorldSquareWithOrthogonalDir::from_square_and_step(
                    widget_square + STEP_DOWN * i,
                    STEP_RIGHT,
                ),
                WorldSquareWithOrthogonalDir::from_square_and_step(
                    widget_square + STEP_RIGHT * (3 + i) + STEP_UP * 3,
                    STEP_UP,
                ),
            );
        }
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();
        let fov = game.rasterized_player_field_of_view();

        let glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_UP + SCREEN_STEP_RIGHT * 2);
        assert_false!(glyphs.looks_solid());
    }

    #[test]

    fn test_floating_hunter_drone__place_and_draw() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(4, 5));
        game.place_floating_hunter_drone(
            WorldPoint::new(5.0, 5.0),
            STEP_RIGHT.to_f32(),
            Angle::degrees(0.0),
        );
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();

        let drone_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_RIGHT);
        let sight_line_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_RIGHT * 3);

        assert_eq!(drone_glyphs.get_solid_color().unwrap(), HUNTER_DRONE_COLOR);
        assert!(char_is_braille(sight_line_glyphs[0].character));
        assert!(char_is_braille(sight_line_glyphs[1].character));
        assert_eq!(sight_line_glyphs[0].fg_color, SIGHT_LINE_SEEKING_COLOR);
    }

    #[test]

    fn test_floating_hunter_drone__rotate_over_time() {
        let mut game = set_up_10x10_game();
        game.place_floating_hunter_drone(
            WorldPoint::new(5.0, 5.0),
            STEP_RIGHT.to_f32(),
            Angle::degrees(0.0),
        );

        let start_angle = game.floating_hunter_drones[0].sight_direction;

        game.tick_realtime_effects(Duration::from_secs_f32(0.5));

        let end_angle = game.floating_hunter_drones[0].sight_direction;

        assert_ne!(start_angle, end_angle);
    }

    #[test]

    fn test_floating_hunter_drone__move_over_time() {
        let mut game = set_up_10x10_game();
        game.place_floating_hunter_drone(
            WorldPoint::new(5.0, 5.0),
            STEP_RIGHT.to_f32(),
            Angle::degrees(0.0),
        );

        let start_pos = game.floating_hunter_drones[0].position;

        game.tick_realtime_effects(Duration::from_secs_f32(0.5));

        let end_pos = game.floating_hunter_drones[0].position;

        assert_ne!(start_pos, end_pos);
    }

    #[test]

    fn test_floating_hunter_drone__bounce_off_board_edge() {
        let mut game = set_up_10x10_game();
        game.place_floating_hunter_drone(
            WorldPoint::new(9.0, 5.0),
            STEP_RIGHT.to_f32(),
            Angle::degrees(0.0),
        );

        let start_vel = game.floating_hunter_drones[0].velocity;

        assert!(start_vel.x > 0.0);
        game.tick_realtime_effects(Duration::from_secs_f32(5.0));

        let end_vel = game.floating_hunter_drones[0].velocity;

        assert!(end_vel.x < 0.0);
    }

    #[test]

    fn test_reflect_off_board_edges() {
        let game = set_up_nxm_game(10, 20);

        assert_eq!(
            game.reflect_off_board_edges(point2(0.0, 0.0), STEP_RIGHT.to_f32()),
            STEP_RIGHT.to_f32(),
            "at origin"
        );
        assert_eq!(
            game.reflect_off_board_edges(point2(-0.6, 0.0), STEP_LEFT.to_f32()),
            STEP_RIGHT.to_f32(),
            "left edge"
        );
        assert_eq!(
            game.reflect_off_board_edges(point2(5.0, 9.9), STEP_UP_LEFT.to_f32()),
            STEP_DOWN_LEFT.to_f32(),
            "top edge"
        );
        assert_eq!(
            game.reflect_off_board_edges(point2(5.0, -9.9), STEP_DOWN_LEFT.to_f32()),
            STEP_UP_LEFT.to_f32(),
            "bottom edge"
        );
        assert_eq!(
            game.reflect_off_board_edges(point2(5.0, -9.9), STEP_UP_LEFT.to_f32()),
            STEP_UP_LEFT.to_f32(),
            "bottom edge, after reflection"
        );
        assert_eq!(
            game.reflect_off_board_edges(point2(20.0, 5.0), STEP_RIGHT.to_f32()),
            STEP_LEFT.to_f32(),
            "right edge"
        );
        assert_eq!(
            game.reflect_off_board_edges(point2(20.0, 5.0), STEP_RIGHT.to_f32() * 5.0),
            STEP_LEFT.to_f32() * 5.0,
            "right edge, but faster"
        );
        assert_eq!(
            game.reflect_off_board_edges(point2(19.6, 9.7), STEP_UP_RIGHT.to_f32()),
            STEP_DOWN_LEFT.to_f32(),
            "two edges at once"
        );
    }

    #[test]

    fn test_hunter_drone_rotates_with_screen_rotation() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(5, 5));
        game.place_floating_hunter_drone(
            WorldPoint::new(6.5, 5.0),
            STEP_RIGHT.to_f32(),
            Angle::degrees(0.0),
        );
        game.graphics
            .screen
            .set_rotation(NormalizedOrthoAngle::new_from_quarter_turns(1));
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();
        let upper_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_DOWN);
        let middle_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_DOWN * 2);
        let lower_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_DOWN * 3);
        assert_eq!(upper_glyphs.to_clean_string(), "▄▄");
        assert_eq!(middle_glyphs.to_clean_string(), "▀▀");
        assert_eq!(lower_glyphs.chars(), [SPACE, '⡇']); // Might need to flip horizontally at some point
    }
    #[test]

    fn test_conveyor_belt__place_and_draw() {
        let mut game = set_up_10x10_game();
        let player_square = point2(4, 5);
        game.place_player(player_square);
        let belt_square = player_square + STEP_DOWN;
        assert!(belt_square.is_even());
        game.place_conveyor_belt(belt_square, RIGHT);
        let advance_and_get_glyphs = |game: &mut Game, duration: Duration| {
            game.tick_realtime_effects(duration);
            game.draw_headless_now();

            game.graphics
                .screen
                .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_DOWN)
        };

        let glyphs1 = advance_and_get_glyphs(&mut game, Duration::from_secs_f32(0.0));

        assert!(glyphs1.get_solid_color().is_some());

        let glyphs2 = advance_and_get_glyphs(
            &mut game,
            Duration::from_secs_f32(CONVEYOR_BELT_VISUAL_PERIOD_S / 8.0),
        );

        assert_ne!(glyphs1, glyphs2);
        assert_eq!(glyphs2.chars(), [RIGHT_HALF_BLOCK, FULL_BLOCK]);

        let glyphs2_5 = advance_and_get_glyphs(
            &mut game,
            Duration::from_secs_f32(CONVEYOR_BELT_VISUAL_PERIOD_S / 2.0),
        );

        assert_eq!(glyphs2_5.chars(), [LEFT_HALF_BLOCK, SPACE]);

        let glyphs3 = advance_and_get_glyphs(
            &mut game,
            Duration::from_secs_f32(CONVEYOR_BELT_VISUAL_PERIOD_S / 2.0),
        );

        assert_eq!(glyphs2, glyphs3);
    }

    #[test]

    fn test_conveyor_belt__push_player() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_player(square);
        let dir = RIGHT;
        game.place_conveyor_belt(square, dir);

        assert_eq!(game.player_square(), square);
        game.tick_realtime_effects(Duration::from_secs_f32(
            CONVEYOR_BELT_MOVEMENT_PERIOD_S * 1.1,
        ));
        assert_eq!(game.player_square(), square.stepped(dir));
    }
    #[test]

    fn test_conveyor_belt__push_widget() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_widget(Widget::new(5), square);
        let dir = RIGHT;
        game.place_conveyor_belt(square, dir);

        assert!(game.widgets.contains_key(&square));
        game.tick_realtime_effects(Duration::from_secs_f32(
            CONVEYOR_BELT_MOVEMENT_PERIOD_S * 1.1,
        ));
        assert!(game.widgets.contains_key(&(square.stepped(dir))));
    }
    #[test]

    fn test_conveyor_belt__push_hunter_drone() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_floating_hunter_drone(square.to_f32(), STEP_ZERO.to_f32(), Angle::degrees(0.0));

        let dir = RIGHT;
        game.place_conveyor_belt(square, dir);

        let pos = game.floating_hunter_drones.iter().next().unwrap().position;
        assert!((pos - square.to_f32()).length() < 0.001);

        let dt = Duration::from_secs_f32(CONVEYOR_BELT_MOVEMENT_PERIOD_S * 0.4635);
        game.tick_realtime_effects(dt);

        let new_pos = game.floating_hunter_drones.iter().next().unwrap().position;
        let new_correct_pos =
            square.to_f32() + STEP_RIGHT.to_f32() * dt.as_secs_f32() * Game::conveyor_belt_speed();

        assert!((new_pos - new_correct_pos).length() < 0.001);
    }
    #[test]

    fn test_conveyor_belt__pushes_death_cube() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_linear_death_cube(square.to_f32(), vec2(0.0, 0.0));
        let dir = RIGHT;
        game.place_conveyor_belt(square, dir);

        let pos = game.death_cubes.get(0).unwrap().position;
        assert!((pos - square.to_f32()).length() < 0.001);

        let dt = Duration::from_secs_f32(CONVEYOR_BELT_MOVEMENT_PERIOD_S * 0.4635);
        game.tick_realtime_effects(dt);

        let new_pos = game.death_cubes.get(0).unwrap().position;
        let new_correct_pos =
            square.to_f32() + STEP_RIGHT.to_f32() * dt.as_secs_f32() * Game::conveyor_belt_speed();

        assert!((new_pos - new_correct_pos).length() < 0.001);
    }
    #[test]

    fn test_floor_arrows_push_hunter_drone() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        let start_pos = square.to_f32() + vec2(0.123, 0.342);
        game.place_floating_hunter_drone(
            start_pos.to_f32(),
            STEP_ZERO.to_f32(),
            Angle::degrees(0.0),
        );
        let dir = RIGHT;
        game.place_floor_push_arrow(square, dir);

        let pos = game.floating_hunter_drones.iter().next().unwrap().position;
        assert!((pos - start_pos).length() < 0.001);

        game.tick_game_logic();

        let new_pos = game.floating_hunter_drones.iter().next().unwrap().position;
        let new_correct_pos = start_pos.stepped(dir);

        assert!((new_pos - new_correct_pos).length() < 0.001);
    }
    #[test]

    fn test_floor_arrows_push_player() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_player(square);
        game.place_floor_push_arrow(square, RIGHT);

        assert_eq!(game.player_square(), square);
        game.tick_game_logic();
        assert_eq!(game.player_square(), square + STEP_RIGHT);
    }
    #[test]

    fn test_floor_arrow_does_not_push_too_far() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_widget(Widget::new(5), square);
        game.place_floor_push_arrow(square, RIGHT);
        game.tick_game_logic();
        assert_eq!(*game.widgets.keys().next().unwrap(), square + STEP_RIGHT);
        game.tick_game_logic();
        assert_eq!(*game.widgets.keys().next().unwrap(), square + STEP_RIGHT);
    }
    #[test]

    fn test_player_tries_to_walk_against_push_arrows() {
        let mut game = set_up_10x10_game();
        let square = point2(8, 5);
        game.place_player(square);
        for i in 0..5 {
            game.place_floor_push_arrow(square + STEP_LEFT * (i + 1), RIGHT);
        }
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();
        for i in 0..30 {
            game.try_slide_player(STEP_LEFT).ok();
            game.tick_game_logic();
        }
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();
        assert_eq!(game.player_square(), square);
    }
    #[test]

    fn test_hunter_drone_turns_towards_player_upon_detection() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_player(square);

        let start_vel = STEP_RIGHT.to_f32();
        game.place_floating_hunter_drone(
            (square + STEP_DOWN * 3).to_f32(),
            start_vel,
            Angle::degrees(90.0),
        );

        game.tick_realtime_effects(Duration::from_secs_f32(0.001));

        let drone: &mut FloatingHunterDrone = game.floating_hunter_drones.get_mut(0).unwrap();

        let end_vel = drone.velocity;

        assert_about_eq!(start_vel.x, end_vel.y);
        assert_about_eq!(start_vel.length(), end_vel.length());
    }
    #[test]

    fn test_raycast__hit_nothing() {
        let game = set_up_10x10_game();
        let result = game.raycast(point2(5.0, 5.0), Angle::degrees(0.0), 3.0);
        assert!(result.grid_entities.is_empty());
    }
    #[test]

    fn test_raycast__hit_block() {
        let mut game = set_up_10x10_game();
        let block_square = point2(9, 5);
        game.place_block(block_square);
        let result = game.raycast(
            (block_square + STEP_LEFT * 4).to_f32(),
            Angle::degrees(0.0),
            5.0,
        );
        assert_eq!(result.grid_entities[0], (STEP_RIGHT * 4, GridEntity::Block));
    }
    #[test]

    fn test_raycast__not_enough_range_to_hit_block() {
        let mut game = set_up_10x10_game();
        let block_square = point2(9, 5);
        game.place_block(block_square);
        let result = game.raycast(
            (block_square + STEP_LEFT * 4).to_f32(),
            Angle::degrees(0.0),
            3.0,
        );
        assert!(result.grid_entities.is_empty());
    }
    #[test]

    fn test_raycast__barely_hit_block() {
        let mut game = set_up_10x10_game();
        let block_square = point2(9, 5);
        game.place_block(block_square);
        let result = game.raycast(
            block_square.to_f32() - vec2(3.51, 0.0),
            Angle::degrees(0.0),
            3.02,
        );
        assert_eq!(result.grid_entities[0], (STEP_RIGHT * 4, GridEntity::Block));
    }
    #[test]

    fn test_raycast__barely_out_of_range() {
        let mut game = set_up_10x10_game();
        let block_square = point2(9, 5);
        game.place_block(block_square);
        let result = game.raycast(
            block_square.to_f32() - vec2(3.51, 0.0),
            Angle::degrees(0.0),
            3.0,
        );
        assert!(result.grid_entities.is_empty());
    }
    #[test]

    fn test_raycast_goes_through_portal() {
        let mut game = set_up_10x10_game();
        let block_square = point2(9, 5);
        game.place_block(block_square);

        let dist_from_exit = 1;
        let portal_entrance_square = block_square + STEP_UP_LEFT * 2;
        let portal_exit_square = block_square + STEP_LEFT * dist_from_exit;

        game.place_single_sided_one_way_portal(
            WorldSquareWithOrthogonalDir::from_square_and_step(portal_entrance_square, STEP_RIGHT),
            WorldSquareWithOrthogonalDir::from_square_and_step(portal_exit_square, STEP_RIGHT),
        );

        let dist_from_entrance = 3;
        let result = game.raycast(
            (portal_entrance_square + STEP_LEFT * dist_from_entrance).to_f32(),
            Angle::degrees(0.0),
            5.0,
        );
        let dist_from_portal_step = 1;
        assert_eq!(
            result.grid_entities[0],
            (
                STEP_RIGHT * (dist_from_exit + dist_from_entrance + dist_from_portal_step),
                GridEntity::Block
            )
        );
    }
    #[test]

    fn test_raycast_goes_through_turned_portal() {
        let mut game = set_up_10x10_game();
        let block_square = point2(5, 5);
        game.place_block(block_square);

        let portal_entrance_square = block_square + STEP_LEFT * 4;
        let portal_exit_square = block_square;

        game.place_single_sided_one_way_portal(
            WorldSquareWithOrthogonalDir::from_square_and_step(portal_entrance_square, STEP_UP),
            WorldSquareWithOrthogonalDir::from_square_and_step(portal_exit_square, STEP_RIGHT),
        );

        let result = game.raycast(portal_entrance_square.to_f32(), Angle::degrees(90.0), 5.0);
        assert_eq!(result.grid_entities[0], (STEP_UP, GridEntity::Block));
    }
    #[test]

    fn test_hunter_drone_raycasts_visually_go_through_portal() {
        let mut game = set_up_10x10_game();
        let drone_square = point2(2, 2);
        game.place_floating_hunter_drone(
            drone_square.to_f32(),
            STEP_ZERO.to_f32(),
            Angle::degrees(90.0),
        );
        let test_square = drone_square + STEP_RIGHT * 5;
        game.place_single_sided_one_way_portal((drone_square, STEP_UP), (test_square, STEP_DOWN));
        game.place_player(drone_square + STEP_DOWN);
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();
        let drawable = game
            .graphics
            .get_drawable_for_square_from_draw_buffer(test_square);

        assert!(matches!(drawable, Some(DrawableEnum::Braille(_))));

        let chars = drawable.unwrap().to_glyphs().chars();
        assert!(char_is_braille(chars[0]) || char_is_braille(chars[1]));
    }
    #[test]

    fn test_hunter_drone_moves_through_portal() {
        let mut game = set_up_10x10_game();
        let drone_square = point2(2, 2);
        game.place_floating_hunter_drone(
            drone_square.to_f32() + vec2(0.0, 0.49),
            STEP_UP.to_f32(),
            Angle::degrees(90.0),
        );
        let test_square = drone_square + STEP_RIGHT * 5;
        game.place_single_sided_one_way_portal((drone_square, STEP_UP), (test_square, STEP_DOWN));
        game.tick_realtime_effects(Duration::from_secs_f32(0.5));
        let drone = game.floating_hunter_drones[0];
        assert!(drone.position.x > drone_square.x as f32 + 2.0);
    }
    #[test]

    fn test_hunter_drone_visually_pokes_through_a_portal_a_little_bit() {
        let mut game = set_up_10x10_game();
        let drone_square = point2(2, 2);
        game.place_floating_hunter_drone(
            drone_square.to_f32() + vec2(0.0, 0.49),
            STEP_ZERO.to_f32(),
            Angle::degrees(180.0),
        );
        let test_square = drone_square + STEP_RIGHT * 5;
        game.place_single_sided_one_way_portal((drone_square, STEP_UP), (test_square, STEP_DOWN));
        game.place_player(drone_square + STEP_DOWN);
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();
        let glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_RIGHT * 5 + SCREEN_STEP_UP);

        assert_false!(glyphs.looks_solid());
    }
}
