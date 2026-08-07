use std::collections::HashMap;
use std::io::Write;
use std::time::{Duration, Instant};

use euclid::*;
use getset::CopyGetters;
use itertools::Itertools;
use ordered_float::OrderedFloat;
use rand::rngs::StdRng;
use rgb::RGB8;
use strum::IntoEnumIterator;

use crate::fov_stuff::{
    portal_aware_field_of_view_from_square, FieldOfViewResult,
};
use crate::graphics::drawable::TextDrawable;
use crate::graphics::*;
use crate::piece::PieceType::*;
use crate::piece::Upgrade::BlinkRange;
use crate::piece::*;
use crate::portal_geometry::PortalGeometry;
use crate::*;
use terminal_rendering::*;
use glyph_constants::named_colors::*;
use crate::graphics::game_colors::*;

mod ai;
mod blocks;
mod combat;
mod floating_entities;
mod realtime;
mod spawning;
mod turns;
pub use spawning::IncubatingPawn;
pub use floating_entities::{DeathCube, FloatingEntityTrait, FloatingHunterDrone, HUNTER_DRONE_SIGHT_RANGE};
pub use blocks::{conveyor_belt_speed, conveyor_period_just_elapsed, Blocks, FloorFeature, CONVEYOR_BELT_MOVEMENT_PERIOD, CONVEYOR_BELT_VISUAL_PERIOD};

const PLAYER_SIGHT_RADIUS: u32 = 16;

use floating_entities::FloatingEntityEnum;

#[derive(Clone, Eq, PartialEq, Debug, Copy)]
enum GridEntity {
    Player,
    Widget(Widget),
    Block,
}

pub struct Player {
    pub position: WorldSquare,
    pub faced_direction: KingWorldStep,
    pub blink_range: u32,
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
    blocks: Blocks,
    widgets: HashMap<WorldSquare, Widget>,
    floor_push_arrows: HashMap<WorldSquare, OrthogonalWorldStep>,
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
            blocks: Blocks::new(),
            widgets: HashMap::new(),
            floor_push_arrows: HashMap::new(),
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
            self.board_size().width as i32 / 2,
            self.board_size().height as i32 / 2,
        )
    }

    fn square_is_on_board(&self, pos: WorldSquare) -> bool {
        pos.x >= 0
            && pos.x < self.board_size().width as i32
            && pos.y >= 0
            && pos.y < self.board_size().height as i32
    }

    // TODO: test

    fn point_is_on_board(&self, point: WorldPoint) -> bool {
        point.x >= -0.5
            && point.x < self.board_size().width as f32 - 0.5
            && point.y >= -0.5
            && point.y < self.board_size().height as f32 - 0.5
    }

    pub fn try_slide_player(&mut self, movement: WorldStep) -> Result<(), ()> {
        assert!(is_orthodiagonal(movement));
        let movement_direction = round_to_king_step(movement);
        let movement_length = king_distance(movement);
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

        let rotation_from_portals = QuarterTurnsAnticlockwise::from_start_and_end_directions(
            direction.into(),
            new_dir.into(),
        );
        self.graphics.screen.rotate(rotation_from_portals);

        Ok(())
    }

    fn get_grid_entity_at_square(&self, square: WorldSquare) -> Option<GridEntity> {
        if self.try_get_player_square() == Some(square) {
            Some(GridEntity::Player)
        } else if self.blocks.is_block_at(square) {
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
        let moved_drones = floating_entities_at_start
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
            .extract_if(.., |drone| {
                world_point_to_world_square(drone.position) == square
            })
            .map(|drone| FloatingEntityEnum::FloatingHunterDrone(drone))
            .collect_vec();
        let death_cubes_from_square = self
            .death_cubes
            .extract_if(.., |cube: &mut DeathCube| {
                world_point_to_world_square(cube.position) == square
            })
            .map(|cube| FloatingEntityEnum::DeathCube(cube))
            .collect_vec();

        [hunter_drones_from_square, death_cubes_from_square].concat()
    }

    pub fn move_player_to(&mut self, square: WorldSquare) {
        self.try_set_player_position(square)
            .expect(&("failed move player to ".to_owned() + &point_to_string(square)));
    }

    fn floor_color_at_square(&self, square: WorldSquare) -> RGB8 {
        self.graphics.floor_color_at_square(square)
    }

    pub fn player_blink_relative_to_screen(&mut self, screen_step: ScreenBufferStep) {
        let world_step = self.graphics.screen.screen_step_to_world_step(screen_step);
        self.player_blink(world_step);
    }

    pub fn player_blink(&mut self, direction: WorldStep) {
        assert!(is_king_step(direction));
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
            } else if !self.square_is_on_board(next_square) || !self.square_is_empty(next_square) {
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
            .filter(|(_, &piece)| piece.piece_type == Arrow)
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

        if let Some(&upgrade) = self.blocks.upgrades.get(&square) {
            self.apply_upgrade(upgrade);
            self.blocks.upgrades.remove(&square);
        }

        self.raw_set_player_position(square);

        return Ok(());
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
        return &mut self.graphics;
    }
    pub fn graphics(&self) -> &Graphics {
        return &self.graphics;
    }
    pub fn pieces(&mut self) -> &mut HashMap<WorldSquare, Piece> {
        return &mut self.pieces;
    }

    fn find_pieces(&self, target_piece: Piece) -> SquareSet {
        self.pieces
            .iter()
            .filter(|(_, &piece)| piece == target_piece)
            .map(|(&square, _)| square)
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
            self.world_time_since_start().as_secs_f32() / CONVEYOR_BELT_VISUAL_PERIOD.as_secs_f32();
        self.graphics
            .draw_conveyor_belts(&self.blocks.conveyor_belts, global_phase_offset);

        self.graphics.draw_move_marker_squares(
            self.move_squares_for_all_pieces(false),
            self.squares_threatened_by_any_piece(false),
            self.move_squares_for_all_pieces(true),
            self.squares_threatened_by_any_piece(true),
        );

        self.graphics.draw_blocks(&self.blocks.blocks);
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
        self.blocks
            .upgrades
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
                .load_screen_buffer_from_fov(self.player_field_of_view());
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

    fn raycast(&self, start_point: WorldPoint, direction: Angle<f32>, range: f32) -> RaycastResult {
        let naive_line = WorldLine::from_ray(start_point, direction, range);

        let line_segments_after_portal_awareness: Vec<WorldLine> = self
            .portal_geometry
            .ray_to_naive_line_segments(start_point, direction, range);

        let mut result = RaycastResult {
            grid_entities: vec![],
            endpoint: line_segments_after_portal_awareness.last().unwrap().p2,
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
                .map(point_to_string)
                .collect_vec(),
            relative_squares_on_naive_line
                .iter()
                .cloned()
                .map(vector2_to_string)
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
        self.blocks.is_upgrade_at(square)
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


    pub fn place_arrow(&mut self, square: WorldSquare, direction: KingWorldStep) {
        self.place_piece(Piece::arrow(direction), square);
    }

    pub fn place_single_sided_one_way_portal(
        &mut self,
        entrance_step: SquareWithOrthogonalDir,
        exit_step: SquareWithOrthogonalDir,
    ) {
        self.portal_geometry.create_portal(entrance_step, exit_step);
    }
    pub fn place_double_sided_one_way_portal(
        &mut self,
        entrance_step: SquareWithOrthogonalDir,
        exit_step: SquareWithOrthogonalDir,
    ) {
        self.portal_geometry
            .create_double_sided_one_way_portal(entrance_step, exit_step);
    }
    pub fn place_single_sided_two_way_portal(
        &mut self,
        entrance_step: SquareWithOrthogonalDir,
        exit_step: SquareWithOrthogonalDir,
    ) {
        self.portal_geometry
            .create_single_sided_two_way_portal(entrance_step, exit_step);
    }
    pub fn place_double_sided_two_way_portal(
        &mut self,
        entrance_step: SquareWithOrthogonalDir,
        exit_step: SquareWithOrthogonalDir,
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
        left_entrance: SquareWithOrthogonalDir,
        left_exit: SquareWithOrthogonalDir,
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
        let entrance =
            SquareWithOrthogonalDir::from_square_and_step(start_square, STEP_RIGHT.into());
        let exit = SquareWithOrthogonalDir::from_square_and_step(exit_square, STEP_RIGHT.into());
        self.place_double_sided_two_way_portal(entrance, exit);
    }

    pub fn place_widget(&mut self, pushable: Widget, square: WorldSquare) {
        self.widgets.insert(square, pushable);
    }
    pub fn place_floor_push_arrow(&mut self, square: WorldSquare, dir: WorldStep) {
        self.floor_push_arrows.insert(square, dir.into());
    }
    pub fn place_conveyor_belt(&mut self, square: WorldSquare, dir: WorldStep) {
        self.blocks.place_conveyor_belt(square, dir);
    }
    pub fn conveyor_belt_speed() -> f32 {
        conveyor_belt_speed()
    }

    pub fn place_block(&mut self, square: WorldSquare) {
        self.blocks.place_block(square);
    }
    pub fn is_block_at(&self, square: WorldSquare) -> bool {
        self.blocks.is_block_at(square)
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
            SquareWithOrthogonalDir::from_square_and_step(entrance_square, STEP_RIGHT.into());
        let exit = SquareWithOrthogonalDir::from_square_and_step(exit_square, STEP_RIGHT.into());

        self.place_double_sided_two_way_portal(entrance, exit);
    }
    pub fn set_up_simple_freestanding_portal(&mut self) {
        let entrance_square = self.player_square() + STEP_RIGHT * 8;
        let exit_square = entrance_square + STEP_RIGHT * 3 + STEP_UP * 5;

        let entrance =
            SquareWithOrthogonalDir::from_square_and_step(entrance_square, STEP_RIGHT.into());
        let exit = SquareWithOrthogonalDir::from_square_and_step(exit_square, STEP_RIGHT.into());

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
            SquareWithOrthogonalDir::from_square_and_step(entrance_square, STEP_RIGHT.into());
        let exit = SquareWithOrthogonalDir::from_square_and_step(exit_square, STEP_RIGHT.into());

        self.place_double_sided_two_way_portal(entrance, exit);
    }
    pub fn set_up_simple_test_map(&mut self) {
        let width = 2;
        let _spacing = width * 2 + 1;
        let left_entrance = SquareWithOrthogonalDir::from_square_and_worldstep(
            self.player_square() + STEP_RIGHT * 5,
            STEP_RIGHT.into(),
        );
        let left_exit = SquareWithOrthogonalDir::from_square_and_worldstep(
            left_entrance.square() + STEP_UP + STEP_RIGHT * 3,
            STEP_UP.into(),
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
            self.place_floor_push_arrow(base_square + STEP_UP * 6 + STEP_RIGHT * i, STEP_RIGHT);
            self.place_conveyor_belt(base_square + STEP_UP * 8 + STEP_RIGHT * i, STEP_LEFT);
        }

        for i in 0..4 {
            self.place_floating_hunter_drone(
                (base_square + STEP_DOWN * (5 + i)).to_f32(),
                STEP_RIGHT.to_f32() * i as f32,
                Angle::degrees(0.0),
            );
        }

        let left_entrance = SquareWithOrthogonalDir::from_square_and_worldstep(
            base_square + STEP_RIGHT * 3 + STEP_UP * 3,
            STEP_RIGHT.into(),
        );
        let left_exit = SquareWithOrthogonalDir::from_square_and_worldstep(
            left_entrance.square() + STEP_UP * 2 + STEP_RIGHT * 2,
            STEP_UP.into(),
        );
        (0..6).for_each(|i| {
            let entrance = left_entrance.strafed_right_n(i);
            let exit = left_exit.strafed_right_n(i);
            self.place_double_sided_two_way_portal(entrance, exit);
        });

        let block_square = left_entrance.square() + STEP_RIGHT * 2 + STEP_DOWN_RIGHT * 3;
        self.place_block(block_square);

        self.place_dense_horizontal_portals(base_square + STEP_RIGHT * 20, 1, 10);

        let entrance = SquareWithOrthogonalDir::from_square_and_worldstep(
            base_square + STEP_LEFT * 7 + STEP_UP * 3,
            STEP_RIGHT.into(),
        );
        let exit = entrance.stepped_n(5).strafed_right();
        self.place_wide_portal(entrance, exit, 5);

        let bars_top_right_root_square = base_square + STEP_LEFT * 20 + STEP_UP * 5;
        self.place_dotted_thin_walls(bars_top_right_root_square);
    }

    pub fn set_up_demo_map(&mut self) {
        let base_square: WorldSquare = self.player_square();

        let left_entrance = SquareWithOrthogonalDir::from_square_and_worldstep(
            base_square + STEP_RIGHT * 3 + STEP_UP * 3,
            STEP_RIGHT.into(),
        );
        let left_exit = SquareWithOrthogonalDir::from_square_and_worldstep(
            left_entrance.square() + STEP_UP * 2 + STEP_RIGHT * 2,
            STEP_UP.into(),
        );
        (0..6).for_each(|i| {
            let entrance = left_entrance.strafed_right_n(i);
            let exit = left_exit.strafed_right_n(i);
            self.place_double_sided_two_way_portal(entrance, exit);
        });

        (0..5).for_each(|dx| {
            let top_left: WorldSquare = base_square + WorldStep::new(-3, -5);
            let dy = 4;
            let left_entrance: SquareWithOrthogonalDir = (top_left, STEP_DOWN).into();
            let left_exit = left_entrance.stepped_n(dy).strafed_left();
            self.place_double_sided_two_way_portal(
                left_entrance.strafed_left_n(dx),
                left_exit.strafed_left_n(dx),
            );
        });

        self.place_death_turret(base_square + STEP_LEFT * 14);
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
        first_entrance: SquareWithOrthogonalDir,
        step: WorldStep,
        num_portals: u32,
        common_exit: SquareWithOrthogonalDir,
    ) {
        (0..num_portals).for_each(|i| {
            let entrance = (
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
        let mut entrance_step = SquareWithOrthogonalDir::from_square_and_worldstep(
            top_left + STEP_LEFT,
            STEP_RIGHT.into(),
        );
        let mut exit_step = SquareWithOrthogonalDir::from_square_and_worldstep(
            top_left + STEP_DOWN,
            STEP_DOWN.into(),
        );

        (0..4).for_each(|_| {
            (0..side_length).for_each(|_| {
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
            SquareWithOrthogonalDir::from_square_and_step(
                block_square + STEP_UP_RIGHT,
                STEP_DOWN.into(),
            ),
            SquareWithOrthogonalDir::from_square_and_step(
                block_square + STEP_DOWN_RIGHT * 4,
                STEP_LEFT.into(),
            ),
        );
        //self.place_death_turret(self.player_square() + STEP_LEFT * 14);
        self.set_up_n_pillars(3);
    }

    pub fn set_up_labyrinth(&mut self, rng: &mut StdRng) {
        let board_squares_total = self.board_size().width * self.board_size().height;
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
        let visibilities = self
            .player_field_of_view()
            .visibilities_of_relative_square(target_square_relative_to_player);
        visibilities.len() == 1
            && visibilities
                .get(0)
                .unwrap()
                .square_visibility_in_absolute_frame()
                .is_fully_visible()
    }
    pub fn square_is_not_visible_to_player(&self, square: WorldSquare) -> bool {
        let target_square_relative_to_player = square - self.player_square();
        self.player_field_of_view()
            .visibilities_of_relative_square(target_square_relative_to_player)
            .is_empty()
    }
    fn player_field_of_view(&self) -> FieldOfViewResult {
        let start_square = self.player_square();
        portal_aware_field_of_view_from_square(
            start_square,
            PLAYER_SIGHT_RADIUS,
            &self.blocks.blocks,
            &self.portal_geometry,
        )
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
mod tests;
