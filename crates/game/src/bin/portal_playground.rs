use color_hex::color_from_hex;
use euclid::point2;
use game::fov_stuff::{
    FieldOfViewResult, MappedSquare, PositionedSquareVisibilityInFov, SquareVisibility,
};
use game::{graphics::Graphics, set_up_input_thread};
use itertools::Itertools;
use rgb::RGB8;
use std::collections::{HashMap, VecDeque};
use std::fmt::Display;
use std::fs::File;
use std::io::Read;
use std::io::{stdin, stdout, Write};
use std::option_env;
use std::path::PathBuf;
use std::thread;
use std::time::{Duration, Instant};
use terminal_rendering::glyph_constants::named_colors::*;
use terminal_rendering::*;
use termion::screen::{IntoAlternateScreen, ToAlternateScreen};
use termion::{
    event::{Event, Key},
    input::{MouseTerminal, TermRead},
    raw::IntoRawMode,
};
use utility::*;

type PortalSide = SquareEdge;

enum PortalRenderingOption {
    LineOnFloor,
    Absolute,
    LineOfSight,
}

// TODO
// enum OrthoDir {
//     Right,
//     Up,
//     Left,
//     Down,
// }

mod geometry2 {

    pub type IPoint = [i32; 2];
    pub type FPoint = [f32; 2];
    pub type OrthoDir = i32;
    pub type SquareEdge = (IPoint, OrthoDir);

    pub const DIR_RIGHT: i32 = 0;
    pub const DIR_UP: i32 = 1;
    pub const DIR_LEFT: i32 = 2;
    pub const DIR_DOWN: i32 = 3;
    pub const ALL_ORTHODIRS: [OrthoDir; 4] = [0, 1, 2, 3];

    pub const STEP_RIGHT: IPoint = [1, 0];
    pub const STEP_UP: IPoint = [0, 1];
    pub const STEP_LEFT: IPoint = [-1, 0];
    pub const STEP_DOWN: IPoint = [0, -1];

    pub trait IPointExt: Sized {
        fn x(&self) -> i32;
        fn y(&self) -> i32;
        fn new(x: i32, y: i32) -> Self;
        fn add(&self, rhs: Self) -> Self {
            Self::new(self.x() + rhs.x(), self.y() + rhs.y())
        }
        fn mul(&self, rhs: i32) -> Self {
            Self::new(self.x() * rhs, self.y() * rhs)
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
    }

    impl IPointExt for IPoint {
        fn x(&self) -> i32 {
            self[0]
        }
        fn y(&self) -> i32 {
            self[1]
        }
        fn new(x: i32, y: i32) -> Self {
            [x, y]
        }
    }

    pub trait FPointExt: Sized {
        fn x(&self) -> f32;
        fn y(&self) -> f32;
        fn new(x: f32, y: f32) -> Self;
        fn add(&self, rhs: Self) -> Self {
            Self::new(self.x() + rhs.x(), self.y() + rhs.y())
        }
        fn mul(&self, rhs: f32) -> Self {
            Self::new(self.x() * rhs, self.y() * rhs)
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
        fn dist(&self, other: Self) -> f32 {
            let dx = other.x() - self.x();
            let dy = other.y() - self.y();
            dx.powi(2) + dy.powi(2)
        }
    }
    impl FPointExt for FPoint {
        fn x(&self) -> f32 {
            self[0]
        }
        fn y(&self) -> f32 {
            self[1]
        }
        fn new(x: f32, y: f32) -> Self {
            [x, y]
        }
    }

    pub trait OrthoPoseExt {
        fn rotate(&self, quarter_turns_ccw: i32) -> Self;
    }
    impl OrthoPoseExt for ([i32; 2], i32) {
        fn rotate(&self, quarter_turns_ccw: i32) -> Self {
            (self.0, (self.1 + quarter_turns_ccw).rem_euclid(4))
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

    pub fn rotate_quarter_turns(v: [i32; 2], turns: i32) -> [i32; 2] {
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
}
use geometry2::FPoint;
use geometry2::IPoint;
use geometry2::*;

struct Camera {
    lower_left_local_square_in_world: [i32; 2],
    upper_right_local_square_in_world: [i32; 2],
}
impl Camera {
    pub fn size(&self) -> [u32; 2] {
        [
            (self.upper_right_local_square_in_world[0] - self.lower_left_local_square_in_world[0])
                as u32,
            (self.upper_right_local_square_in_world[1] - self.lower_left_local_square_in_world[1])
                as u32,
        ]
    }
    pub fn width(&self) -> u32 {
        self.size()[0]
    }
    pub fn height(&self) -> u32 {
        self.size()[1]
    }
    pub fn local_to_world_square(&self, local_v: IPoint) -> IPoint {
        rotate_quarter_turns(local_v, self.quarter_turns_ccw_from_world())
            .add(self.lower_left_local_square_in_world)
    }
    pub fn local_to_world_ortho_dir(&self, dir: OrthoDir) -> OrthoDir {
        (dir + self.quarter_turns_ccw_from_world()).rem_euclid(4)
    }
    pub fn quarter_turns_ccw_from_world(&self) -> OrthoDir {
        let [x1, y1] = self.lower_left_local_square_in_world;
        let [x2, y2] = self.upper_right_local_square_in_world;

        match (x1 < x2, y1 < y2) {
            (true, true) => 0,
            (true, false) => 3,
            (false, true) => 1,
            (false, false) => 2,
        }
    }
}

// struct PortalUnderConstruction {
//     start_square: IPoint,
//     entrance_direction: Option<OrthoDir>,
//     extension_direction: Option<OrthoDir>,
//     extension_length: Option<u32>
// }

struct GameState {
    running: bool,
    width: usize,
    height: usize,
    player_square: IPoint,
    player_is_alive: bool,

    portals: HashMap<PortalSide, PortalSide>,

    last_mouse_screen_row_col: Option<[u16; 2]>,
    pub portal_rendering: PortalRenderingOption,
    board_color_function: fn(&GameState, IPoint) -> Option<RGB8>,
    portal_tint_function: fn(RGB8, u32) -> RGB8,
}
impl GameState {
    pub fn new(width: usize, height: usize) -> Self {
        GameState {
            running: true,
            width,
            height,
            player_square: [5, 5],
            player_is_alive: false,
            portals: Default::default(),
            last_mouse_screen_row_col: None,
            portal_rendering: PortalRenderingOption::LineOnFloor,
            board_color_function: Self::default_board_color,
            portal_tint_function: Self::default_portal_tint,
        }
    }
    fn default_board_color(&self, square: IPoint) -> Option<RGB8> {
        let is_white = ((square[0] / 3).rem_euclid(2) == 0) == ((square[1] / 3).rem_euclid(2) == 0);
        Some(if is_white { grey(191) } else { grey(127) })
    }
    fn radial_sin_board_colors(&self, square: IPoint) -> Option<RGB8> {
        let [cx, cy] = self.player_square;
        let dx = square[0] - cx;
        let dy = square[1] - cy;
        let d = ((dx.pow(2) + dy.pow(2)) as f32).sqrt();
        let wave_length = 7.0;
        let mid = 120.0;
        let ampl = 50.0;

        let val = mid + ampl * (d / wave_length * std::f32::consts::TAU).cos();

        if self.on_board(square) {
            Some(grey(val.round() as u8))
        } else {
            None
        }
    }
    fn default_portal_tint(color: RGB8, depth: u32) -> RGB8 {
        let tint = RED;
        let strength = (0.1 * depth as f32).min(1.0);
        tint_color(color, tint, strength)
    }
    fn rainbow_tint(color: RGB8, depth: u32) -> RGB8 {
        if depth == 0 {
            return color;
        }
        let rainbow = [RED, ORANGE, YELLOW, GREEN, CYAN, BLUE, MAGENTA];
        let tint = rainbow[(depth.saturating_sub(1) as usize).rem_euclid(rainbow.len())];
        let strength = (0.1 * depth as f32).min(1.0);
        tint_color(color, tint, strength)
    }
    fn rainbow_solid(color: RGB8, depth: u32) -> RGB8 {
        if depth == 0 {
            return color;
        }
        let rainbow = [RED, ORANGE, YELLOW, GREEN, CYAN, BLUE, MAGENTA];
        rainbow[(depth.saturating_sub(1) as usize).rem_euclid(rainbow.len())]
    }

    pub fn on_board(&self, square: IPoint) -> bool {
        square[0] >= 0
            && square[0] < self.width as i32
            && square[1] >= 0
            && square[1] < self.height as i32
    }

    pub fn process_events(&mut self, events: impl IntoIterator<Item = Event>) {
        events.into_iter().for_each(|e| self.process_event(e))
    }

    pub fn place_portal(&mut self, entrance: PortalSide, reverse_entrance: PortalSide) {
        self.portals.insert(entrance, reverse_entrance);
        self.portals.insert(reverse_entrance, entrance);
        self.portals.insert(
            other_side_of_edge(entrance),
            other_side_of_edge(reverse_entrance),
        );
        self.portals.insert(
            other_side_of_edge(reverse_entrance),
            other_side_of_edge(entrance),
        );
    }
    pub fn process_event(&mut self, event: Event) {
        match event {
            Event::Key(key) => match key {
                Key::Char('q') => self.running = false,
                Key::Char('t') => {
                    self.portal_rendering = match self.portal_rendering {
                        PortalRenderingOption::LineOnFloor => PortalRenderingOption::Absolute,
                        PortalRenderingOption::Absolute => PortalRenderingOption::LineOfSight,
                        PortalRenderingOption::LineOfSight => PortalRenderingOption::LineOnFloor,
                    }
                }
                // Key::Backspace => todo!(),
                // Key::Left => todo!(),
                // Key::Right => todo!(),
                // Key::Up => todo!(),
                // Key::Down => todo!(),
                // Key::Home => todo!(),
                // Key::End => todo!(),
                // Key::PageUp => todo!(),
                // Key::PageDown => todo!(),
                // Key::BackTab => todo!(),
                // Key::Delete => todo!(),
                // Key::Insert => todo!(),
                // Key::F(_) => todo!(),
                // Key::Alt(_) => todo!(),
                // Key::Ctrl(_) => todo!(),
                // Key::Null => todo!(),
                // Key::Esc => todo!(),
                _ => {}
            },
            Event::Mouse(mouse_event) => match mouse_event {
                termion::event::MouseEvent::Press(mouse_button, col, row) => {
                    self.last_mouse_screen_row_col = Some([row - 1, col - 1])
                }
                termion::event::MouseEvent::Release(col, row) => {
                    self.last_mouse_screen_row_col = None
                }
                termion::event::MouseEvent::Hold(col, row) => {
                    self.last_mouse_screen_row_col = Some([row - 1, col - 1])
                }
            },
            Event::Unsupported(items) => todo!(),
        }
    }
    // xy order from bottom left of screen
    fn mouse_screen_square(&self) -> Option<IPoint> {
        self.last_mouse_screen_row_col
            .map(|[screen_row, screen_col]| {
                let screen_y: i32 = self.height as i32 - i32::from(screen_row) - 1;
                [i32::from(screen_col) / 2, screen_y]
            })
    }

    fn naive_glyphs_for_rotated_world_square(
        &self,
        square: IPoint,
        rotation: i32,
    ) -> DoubleGlyphWithTransparency {
        assert!(rotation >= 0 && rotation < 4);
        todo!();
    }
    // Simple top-down, no rotation, no portals (except for entrance/exit)
    fn naive_glyphs_for_world_square(&self, square: IPoint) -> DoubleGlyphWithTransparency {
        self.naive_glyphs_for_rotated_world_square(square, 0)
    }
    fn mouse_square_xy_in_camera_frame(&self) -> Option<IPoint> {
        self.last_mouse_screen_row_col
            .map(|[screen_row, screen_col]| {
                let screen_y: i32 = self.height as i32 - i32::from(screen_row) - 1;
                [i32::from(screen_col) / 2, screen_y]
            })
    }
    fn mouse_square_xy_in_world_frame(&self) -> Option<IPoint> {
        todo!()
    }

    // first part of output tuple: frame to render
    // Second part of output tuple: list of frames that combine to the output frame.  Each frame in
    // the list is a different coherent view through portals
    //   - Same portal depth
    //   - Same Portal tint (currently identical to portal depth) (may also count non-portal light
    //   sources in future)
    //   - Same transform from local to absolute frame, even if the portals are separated
    //   physically
    fn render_with_debug_deconstruction(&self, is_debug: bool) -> (Frame, Vec<Frame>) {
        let portal_geometry =
            game::portal_geometry::PortalGeometry::from_entrances_and_reverse_entrances(
                self.portals.clone(), // .iter()
                                      // .map(|(&entrance, &reverse_exit)| (entrance, reverse_exit.rotate(2)))
                                      // .collect(),
            );
        // panic!();

        let mouse_screen_square: Option<[i32; 2]> = self.mouse_screen_square();
        let fov_center = match mouse_screen_square {
            Some(x) => x,
            None => self.player_square,
        };
        let fov = game::fov_stuff::portal_aware_field_of_view_from_square(
            fov_center.into(),
            10,
            &Default::default(),
            &portal_geometry,
        );

        // Key is (depth, absolute_position, rotation from portal)
        let mut debug_portal_visualizer_frames: HashMap<(u32, [i32; 2], i32), Frame> =
            Default::default();

        let frame = (0..self.height)
            .map(|camera_row| {
                let camera_y = self.height as i32 - camera_row as i32 - 1;
                (0..self.width as i32)
                    .map(|camera_x| {
                        let camera_col = camera_x as usize;
                        // let x = col;
                        // let y = self.height - row - 1;
                        let camera_pos: WorldSquare = [camera_x, camera_y].into();
                        let camera_pos_relative_to_fov_center =
                            camera_pos - WorldSquare::from(fov_center);
                        let visible_portions_at_relative_square: Vec<
                            PositionedSquareVisibilityInFov,
                        > = match self.portal_rendering {
                            PortalRenderingOption::LineOfSight => {
                                FieldOfViewResult::sorted_by_draw_order(
                                    fov.visibilities_of_relative_square(
                                        camera_pos_relative_to_fov_center,
                                    ),
                                )
                            }
                            PortalRenderingOption::LineOnFloor => {
                                vec![PositionedSquareVisibilityInFov::new_in_top_view(
                                    SquareVisibility::new_fully_visible(),
                                    camera_pos,
                                    camera_pos_relative_to_fov_center,
                                )]
                            }
                            PortalRenderingOption::Absolute => todo!(),
                        };

                        let glyph_layers_to_combine: Vec<[GlyphWithTransparency; 2]> =
                            visible_portions_at_relative_square
                                .clone()
                                .into_iter()
                                .map(|square_viz| self.render_one_view_of_a_square(&square_viz))
                                .collect_vec();
                        if is_debug {
                            visible_portions_at_relative_square
                                .iter()
                                .zip(glyph_layers_to_combine.iter())
                                .for_each(|(square_viz, double_glyph)| {
                                    let debug_frames_key: (u32, IPoint, i32) = (
                                        square_viz.portal_depth(),
                                        square_viz.absolute_fov_center_square().into(),
                                        square_viz
                                            .portal_rotation_from_relative_to_absolute()
                                            .quarter_turns()
                                            .rem_euclid(4),
                                    );
                                    if !debug_portal_visualizer_frames
                                        .contains_key(&debug_frames_key)
                                    {
                                        debug_portal_visualizer_frames.insert(
                                            debug_frames_key,
                                            Frame::blank(self.width * 2, self.height),
                                        );
                                    }
                                    let mut debug_frame = debug_portal_visualizer_frames
                                        .get_mut(&debug_frames_key)
                                        .unwrap();
                                    debug_frame.set_by_double_wide_grid(
                                        camera_row,
                                        camera_col,
                                        double_glyph.map(|g| g.over_solid_bg(BLACK)),
                                    );
                                });
                        }
                        // TODO: combine properly
                        glyph_layers_to_combine
                            .into_iter()
                            .rev()
                            .fold(
                                DoubleGlyph::solid_color(BLACK)
                                    .map(|g| GlyphWithTransparency::from_glyph(g)),
                                |below, above| [0, 1].map(|i| above[i].over(below[i])),
                            )
                            .map(|g| g.over_solid_bg(BLACK))
                        // glyph_layers_to_combine[0]
                    })
                    .collect_vec()
            })
            .collect_vec()
            .into();
        (
            frame,
            debug_portal_visualizer_frames.into_values().collect_vec(),
        )
    }
    fn portal_entrance_glyphs(
        &self,
        mapped_square: MappedSquare,
    ) -> Option<DoubleGlyphWithTransparency> {
        let absolute_internal_faces_with_portal_entrance = ALL_ORTHODIRS.map(|dir| {
            self.portals
                .contains_key(&(mapped_square.absolute_square.into(), dir))
        });
        let internal_faces_with_visible_portal_entrances = match self.portal_rendering {
            PortalRenderingOption::LineOfSight => {
                let visible_relative_internal_faces = ALL_ORTHODIRS.map(|dir| {
                    !mapped_square
                        .relative_square
                        .to_array()
                        .has_component_against_direction(dir)
                });
                let mut relative_internal_faces_with_portal_entrance =
                    absolute_internal_faces_with_portal_entrance.clone();
                relative_internal_faces_with_portal_entrance.rotate_right(
                    mapped_square
                        .quarter_turns_ccw_from_absolute_to_relative()
                        .quarter_turns() as usize,
                );
                array_zip(
                    visible_relative_internal_faces,
                    relative_internal_faces_with_portal_entrance,
                )
                .map(|(a, b)| a && b)
            }
            _ => absolute_internal_faces_with_portal_entrance,
        };
        let any_entrances_visible_here = internal_faces_with_visible_portal_entrances
            .iter()
            .any(|x| *x);
        if any_entrances_visible_here {
            let visible_portal_entrance_characters =
                chars_for_square_walls(internal_faces_with_visible_portal_entrances);
            let visible_portal_entrance_glyphs = visible_portal_entrance_characters.map(|c| {
                GlyphWithTransparency::from_char(c)
                    .with_primary_rgb(RED)
                    .with_primary_only()
            });
            return Some(visible_portal_entrance_glyphs);
        }
        None
    }

    pub fn render(&self) -> Frame {
        self.render_with_debug_deconstruction(false).0
    }
    fn render_one_view_of_a_square(
        &self,
        square_viz: &PositionedSquareVisibilityInFov,
    ) -> [GlyphWithTransparency; 2] {
        let mut glyphs = if let Some(board_color) =
            (self.board_color_function)(&self, square_viz.absolute_square().into())
        {
            DoubleGlyphWithTransparency::solid_color(board_color)
        } else {
            let g = GlyphWithTransparency::from_char('.');
            [g, g]
        };

        // draw visible portal entrances

        if let Some(visible_portal_entrance_glyphs) =
            self.portal_entrance_glyphs(square_viz.mapped_square())
        {
            glyphs = visible_portal_entrance_glyphs.over(glyphs);
        }

        if square_viz.absolute_square().to_array() == self.player_square && self.player_is_alive {
            glyphs = DoubleGlyphWithTransparency::solid_color(color_from_hex!("#1688f0").into());
        }

        let mouse_is_here =
            self.mouse_square_xy_in_camera_frame()
                .is_some_and(|mouse_camera_pos| {
                    mouse_camera_pos == square_viz.absolute_square().to_array()
                });
        if mouse_is_here {
            let mouse_is_on_left_half_of_square = self
                .last_mouse_screen_row_col
                .is_some_and(|[row, col]| col.rem_euclid(2) == 0);
            let mouse_index_in_square = if mouse_is_on_left_half_of_square {
                0
            } else {
                1
            };
            glyphs[mouse_index_in_square] = GlyphWithTransparency::solid_color(RED);
        }

        // apply tint
        glyphs.iter_mut().for_each(|glyph| {
            *glyph.primary_color.rgb_mut() =
                (self.portal_tint_function)(glyph.primary_color.rgb(), square_viz.portal_depth());
            *glyph.secondary_color.rgb_mut() =
                (self.portal_tint_function)(glyph.secondary_color.rgb(), square_viz.portal_depth())
        });

        // apply obscurement
        if !square_viz
            .square_visibility_in_absolute_frame()
            .is_fully_visible()
        {
            let bias_direction = square_viz
                .square_visibility_in_relative_frame()
                .visible_portion()
                .unwrap()
                .direction_away_from_plane();
            glyphs = glyphs
                .into_iter()
                .zip(
                    square_viz
                        .square_visibility_in_relative_frame()
                        .split_into_character_visibilities()
                        .into_iter(),
                )
                .map(
                    |(glyph, visible_portion_of_glyph)| match visible_portion_of_glyph {
                        None => glyph,
                        Some(visible_portion) => {
                            let window = half_plane_to_angled_block_character(
                                visible_portion,
                                bias_direction,
                            );
                            glyph.seen_through_window(window)
                        }
                    },
                )
                .collect_vec()
                .try_into()
                .unwrap()
        }
        glyphs
    }
}

fn grey(x: u8) -> RGB8 {
    RGB8::new(x, x, x)
}

fn draw_frame(writable: &mut impl Write, new_frame: &Frame, maybe_old_frame: &Option<Frame>) {
    writable.write(&new_frame.bytes_for_raw_display_over(maybe_old_frame));
    writable.flush();
}

fn main() {
    let (term_width, term_height) = termion::terminal_size().unwrap();
    let mut screen_frame = Frame::blank(term_width as usize, term_height as usize);
    let (width, height) = (50, 25);

    let mut game_state = GameState::new(width / 2, height);
    game_state.player_is_alive = true;
    game_state.portal_rendering = PortalRenderingOption::LineOfSight;
    game_state.place_portal(([10, 5], DIR_UP), ([15, 15], DIR_RIGHT));

    let mut writable =
        termion::cursor::HideCursor::from(MouseTerminal::from(stdout().into_raw_mode().unwrap()))
            .into_alternate_screen()
            .unwrap();

    let event_receiver = set_up_input_thread();

    let mut prev_drawn = None;
    let mut event_log = VecDeque::new();
    while game_state.running {
        while let Ok(event) = event_receiver.try_recv() {
            event_log.push_front(event.clone());
            event_log.truncate(5);
            game_state.process_event(event);
        }
        let frame = game_state.render();
        screen_frame.blit(&frame, [0, 0]);
        screen_frame.draw_text(
            format!("{:<30}", game_state.last_mouse_screen_row_col.to_debug()),
            [(height + 1).into(), 0],
        );
        for (i, event) in event_log.iter().enumerate() {
            screen_frame.draw_text(
                format!("{:<30}", format!("{:?}", event)),
                [height as usize + 3 + i, 0],
            );
        }
        draw_frame(&mut writable, &screen_frame, &prev_drawn);
        prev_drawn = Some(screen_frame.clone());
        thread::sleep(Duration::from_millis(21));
    }
}

fn smoothed_mouse_position(recent_square_entries: &[(f32, IPoint)], now: f32) -> FPoint {
    assert!(recent_square_entries.len() > 0);
    if recent_square_entries.len() == 1 {
        return recent_square_entries[0].1.to_float();
    }

    // simple interpolation for now
    let l = recent_square_entries.len();
    let last = recent_square_entries[l - 1];
    let prev = recent_square_entries[l - 2];
    let dt = last.0 - prev.0;
    let step = last.1.sub(prev.1);
    let next = last.1.add(step);

    if now > last.0 + dt {
        return next.to_float();
    }

    last.1
        .to_float()
        .add(step.to_float().mul((now - last.0) / dt))
}

#[cfg(test)]
mod tests {
    use super::*;
    use game::fov_stuff::LocalSquareHalfPlane;
    use pretty_assertions::assert_str_eq;
    use std::{assert_eq, assert_ne, f32::consts::TAU};
    use stdext::function_name;
    use termion::event::MouseEvent;

    #[test]
    fn test_simple_output() {
        let state = GameState::new(10, 10);
        let frame = state.render();
        assert_eq!(frame.width(), 20);
        assert_eq!(frame.height(), 10);
    }

    fn press_left(col: u16, row: u16) -> termion::event::Event {
        termion::event::Event::Mouse(termion::event::MouseEvent::Press(
            termion::event::MouseButton::Left,
            col + 1,
            row + 1,
        ))
    }
    fn drag_mouse(col: u16, row: u16) -> termion::event::Event {
        termion::event::Event::Mouse(termion::event::MouseEvent::Hold(col + 1, row + 1))
    }
    fn release_mouse(col: u16, row: u16) -> termion::event::Event {
        termion::event::Event::Mouse(termion::event::MouseEvent::Release(col + 1, row + 1))
    }

    macro_rules! compare_frame_to_file {
        ($frame:ident, $prefix:expr) => {
            let test_name: String = function_name!().replace(":", "_");
            compare_frame_for_test($frame, format!("{}_{}", $prefix, test_name))
        };
        ($frame:ident) => {
            compare_frame_to_file!($frame, "")
        };
    }

    fn compare_frame_for_test(candidate_frame: Frame, file_prefix: String) {
        let candidate_string = candidate_frame.string_for_regular_display();

        let file_directory: PathBuf = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/");
        assert!(file_directory.is_dir());
        let correct_frame_path: PathBuf = file_directory.join(file_prefix + "_good_frame.txt");

        let maybe_correct_string: Option<String> =
            std::fs::read_to_string(correct_frame_path.clone()).ok();

        // eprintln!("{}", &candidate_string);

        let blessed = option_env!("BLESS_TESTS").is_some();
        if blessed {
            std::fs::write(correct_frame_path, candidate_string).unwrap();
            return;
        }

        let correct_frame = Frame::parse_regular_display_string(
            maybe_correct_string.expect(
                &format!("No existing test output found.  Set BLESS_TESTS to canonize current output frame:\n\n{candidate_frame:?}"),
            ),
        );
        assert!(candidate_frame == correct_frame,
            "Frames do not match.  Set the BLESS_TESTS env var to lock-in current string as correct.\n\nCorrect:\n{correct_frame:?}\n\nGiven:\n{candidate_frame:?}\n\nDifferences only:\n{diff1:?}\n\n{diff2:?}❌\n",
            diff1 = correct_frame.diff_from(&candidate_frame),
            diff2 = candidate_frame.diff_from(&correct_frame)
        );

        eprintln!("{candidate_frame:?}\n✅");
    }

    #[test]
    fn test_click_a() {
        let mut game = GameState::new(12, 12);
        game.process_event(press_left(0, 0));
        let frame = game.render();
        // let no_color = frame.uncolored_regular_string();
        dbg!(&frame);
        compare_frame_to_file!(frame);
    }
    #[test]
    fn test_click_a_small() {
        let mut game = GameState::new(2, 2);
        game.process_event(press_left(0, 0));
        let frame = game.render();
        compare_frame_to_file!(frame);
    }
    #[test]
    fn test_click_b() {
        let mut game = GameState::new(12, 12);
        game.process_event(press_left(3, 9));
        let frame = game.render();
        dbg!(&frame);
        eprintln!("{}", frame.string_for_regular_display());
        assert_ne!(frame.get_xy([2, 2]).bg_color, RED);
        assert_eq!(frame.get_xy([3, 2]).bg_color, RED);
        compare_frame_to_file!(frame);
    }
    #[test]
    fn test_drag_mouse() {
        let mut game = GameState::new(12, 12);
        game.process_events([press_left(3, 3)]);
        let frame_1 = game.render();
        game.process_events([drag_mouse(4, 3)]);
        let frame_2 = game.render();
        game.process_events([drag_mouse(5, 3)]);
        let frame_3 = game.render();
        // dbg!(&frame_1, &frame_2, &frame_3);
        compare_frame_to_file!(frame_1, "1");
        compare_frame_to_file!(frame_2, "2");
        compare_frame_to_file!(frame_3, "3");
    }
    #[test]
    fn test_render_portal_edges() {
        let mut game = GameState::new(12, 12);
        game.place_portal(([1, 1], DIR_UP), ([1, 3], DIR_UP));
        game.place_portal(([3, 1], DIR_UP), ([3, 3], DIR_UP));
        game.place_portal(([3, 1], DIR_DOWN), ([3, 3], DIR_DOWN));
        game.place_portal(([5, 1], DIR_UP), ([5, 3], DIR_UP));
        game.place_portal(([5, 1], DIR_DOWN), ([5, 3], DIR_DOWN));
        game.place_portal(([5, 1], DIR_RIGHT), ([5, 3], DIR_RIGHT));
        game.place_portal(([7, 1], DIR_UP), ([7, 3], DIR_UP));
        game.place_portal(([7, 1], DIR_DOWN), ([7, 3], DIR_DOWN));
        game.place_portal(([7, 1], DIR_RIGHT), ([7, 3], DIR_RIGHT));
        game.place_portal(([7, 1], DIR_LEFT), ([7, 3], DIR_LEFT));
        game.place_portal(([9, 1], DIR_DOWN), ([9, 3], DIR_DOWN));
        game.place_portal(([9, 1], DIR_RIGHT), ([9, 3], DIR_RIGHT));
        game.place_portal(([9, 1], DIR_LEFT), ([9, 3], DIR_LEFT));
        game.portal_rendering = PortalRenderingOption::LineOnFloor;
        let frame = game.render();
        compare_frame_to_file!(frame);
    }
    #[test]
    fn test_render_part_of_square() {
        let mut game = GameState::new(12, 12);
        game.board_color_function = |_state, _square| Some(GREEN);

        let visible_portion = PositionedSquareVisibilityInFov {
            square_visibility_in_absolute_frame: SquareVisibility::from_visible_half_plane(
                LocalSquareHalfPlane::from_clockwise_sweeping_line([[3.0, 2.0], [0.0, 0.0]].into()),
            )
            .unwrap(),
            relative_square: [2, 2].into(),
            absolute_square: [2, 2].into(),
            portal_depth: 0,
            portal_rotation_from_relative_to_absolute: QuarterTurnsAnticlockwise::new(0),
        };
        let glyphs = game.render_one_view_of_a_square(&visible_portion);
        println!("{}{}", glyphs[0].to_string(), glyphs[1].to_string());
        assert_eq!(glyphs[0].character, '🭞');
        assert_eq!(glyphs[0].fg_color(), GREEN.into());
        assert_eq!(glyphs[0].bg_color(), Glyph::default_bg_color.with_alpha(0));
        assert_eq!(glyphs[1].character, '🭜');
        assert_eq!(glyphs[1].fg_color(), GREEN.into());
        assert_eq!(glyphs[1].bg_color(), Glyph::default_bg_color.with_alpha(0));
        // println!("{}",glyphs.to_clean_string());
    }
    #[test]
    fn test_render_part_of_square_with_rotation() {
        let mut game = GameState::new(12, 12);
        game.board_color_function = |_state, _square| Some(GREEN);

        let mut frame = Frame::blank(20, 3);

        for i in 0..4 {
            let visible_portion = PositionedSquareVisibilityInFov {
                square_visibility_in_absolute_frame: SquareVisibility::from_visible_half_plane(
                    LocalSquareHalfPlane::from_clockwise_sweeping_line(
                        [[3.0, 2.0], [0.0, 0.0]].into(),
                    ),
                )
                .unwrap(),
                relative_square: [2, 2].into(),
                absolute_square: [2, 2].into(),
                portal_depth: 0,
                portal_rotation_from_relative_to_absolute: i.into(),
            };
            let glyphs = game.render_one_view_of_a_square(&visible_portion);
            frame.set_by_double_wide_grid(
                1,
                2 * i as usize + 1,
                glyphs.map(|g| g.over_solid_bg(BLACK)),
            );
        }

        compare_frame_to_file!(frame);
    }
    #[test]
    fn test_render_one_line_of_sight_portal() {
        let mut game = GameState::new(12, 12);
        game.portal_rendering = PortalRenderingOption::LineOfSight;
        game.board_color_function = GameState::radial_sin_board_colors;
        game.place_portal(([5, 7], DIR_UP), ([7, 10], DIR_UP));
        // game.portal_tint_function = GameState::rainbow_solid;
        // dbg!(game.render());
        game.portal_tint_function = GameState::rainbow_tint;
        let (frame, layers) = game.render_with_debug_deconstruction(true);
        layers.into_iter().for_each(|frame| {
            dbg!(frame);
        });
        compare_frame_to_file!(frame);
    }
    #[test]
    fn test_portal_with_rotation() {
        let mut game = GameState::new(12, 12);
        game.portal_rendering = PortalRenderingOption::LineOfSight;
        game.board_color_function = |game_state, square| {
            let n = 10;
            let frac = square.y().rem_euclid(n) as f32 / n as f32;
            let val = 100 + (100.0 * frac) as u8;
            Some(grey(val))
        };

        GameState::radial_sin_board_colors;
        game.place_portal(([5, 7], DIR_UP), ([7, 10], DIR_RIGHT));
        // game.portal_tint_function = GameState::rainbow_solid;
        // dbg!(game.render());
        game.portal_tint_function = GameState::rainbow_tint;
        let (frame, layers) = game.render_with_debug_deconstruction(true);
        layers.into_iter().for_each(|frame| {
            dbg!(frame);
        });

        compare_frame_to_file!(frame);
    }

    fn sim_mouse_path(
        path_fn: impl Fn(f32) -> FPoint,
        sample_rate: f32,
        end_t: f32,
    ) -> Vec<(f32, FPoint)> {
        let n = (end_t / sample_rate).floor() as u32;
        (0..n)
            .map(|i| {
                let t = i as f32 / sample_rate;
                (t, path_fn(t))
            })
            .collect_vec()
    }
    fn path_to_square_entry_events(path: &[(f32, FPoint)]) -> Vec<(f32, IPoint)> {
        path.iter()
            .tuple_windows()
            .filter_map(|(a, b)| {
                if a.1.rounded() != b.1.rounded() {
                    Some((b.0, b.1.rounded()))
                } else {
                    None
                }
            })
            .collect_vec()
    }

    fn smoothed_mouse_path(
        square_entry_events: Vec<(f32, IPoint)>,
        sample_rate: f32,
    ) -> Vec<(f32, FPoint)> {
        assert!(square_entry_events.len() > 0);
        let t0 = square_entry_events[0].0;
        let t_end = square_entry_events.last().unwrap().0;
        let mut t = t0;
        let mut i = 0;
        let mut out = vec![];
        while t < t_end {
            while t >= square_entry_events[i].0 {
                i += 1;
            }
            out.push((t, smoothed_mouse_position(&square_entry_events[0..i], t)));
            t += 1.0 / sample_rate;
        }
        out
    }

    #[ignore]
    #[test]
    fn test_smoothed_mouse_motion() {
        let path_func = |t| [0.0, 0.0].add([5.0, 0.0].mul(t));
        let sim_path: Vec<(f32, FPoint)> = sim_mouse_path(path_func, 60.0, 3.0);
        let square_entry_events: Vec<(f32, IPoint)> = path_to_square_entry_events(&sim_path);

        let smoothed_path = smoothed_mouse_path(square_entry_events, 60.0);

        assert_eq!(sim_path.len(), smoothed_path.len());

        let errors: Vec<f32> = sim_path
            .iter()
            .zip(smoothed_path.iter())
            .map(|((t1, p1), (t2, p2))| p1.dist(*p2))
            .collect_vec();
        println!("{}", bargraph(errors, 5));
        panic!();
    }
}
