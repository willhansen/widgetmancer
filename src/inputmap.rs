use termion::event::{Event, Key, MouseButton, MouseEvent};

use crate::game::Game;
use crate::graphics::screen::{
    ScreenBufferStep, SCREEN_STEP_DOWN, SCREEN_STEP_DOWN_LEFT, SCREEN_STEP_DOWN_RIGHT,
    SCREEN_STEP_LEFT, SCREEN_STEP_RIGHT, SCREEN_STEP_UP, SCREEN_STEP_UP_LEFT, SCREEN_STEP_UP_RIGHT,
};
use crate::utility::coordinate_frame_conversions::*;
use crate::utility::{FVector, IVector};
use crate::{point2, DOWN_I, LEFT_I, RIGHT_I, UP_I};

pub struct InputMap {
    prev_mouse_square: WorldSquare,
    terminal_size: (u16, u16), // (width, height)
}

impl InputMap {
    pub fn new(width: u16, height: u16) -> InputMap {
        InputMap {
            prev_mouse_square: point2((width / 2) as i32, (height / 2) as i32),
            terminal_size: (width, height),
        }
    }

    fn char_to_screen_direction(c: char) -> Option<ScreenBufferStep> {
        match c.to_lowercase().next().unwrap() {
            'k' | 'w' => Some(SCREEN_STEP_UP),
            'j' | 's' => Some(SCREEN_STEP_DOWN),
            'h' | 'a' => Some(SCREEN_STEP_LEFT),
            'l' | 'd' => Some(SCREEN_STEP_RIGHT),
            'y' => Some(SCREEN_STEP_UP_LEFT),
            'u' => Some(SCREEN_STEP_UP_RIGHT),
            'b' => Some(SCREEN_STEP_DOWN_LEFT),
            'n' => Some(SCREEN_STEP_DOWN_RIGHT),
            _ => None,
        }
    }

    fn key_to_screen_direction(k: Key) -> Option<ScreenBufferStep> {
        match k {
            Key::Char(c) => Self::char_to_screen_direction(c),
            Key::Up => Some(SCREEN_STEP_UP),
            Key::Down => Some(SCREEN_STEP_DOWN),
            Key::Left => Some(SCREEN_STEP_LEFT),
            Key::Right => Some(SCREEN_STEP_RIGHT),
            _ => None,
        }
    }
    fn is_shifted_key(k: Key) -> bool {
        if let Key::Char(c) = k {
            // TODO: Don't know if this works with the number keys
            c.is_uppercase()
        } else {
            false
        }
    }

    pub fn handle_event(&mut self, game: &mut Game, evt: Event) {
        match evt {
            Event::Key(ke) => match ke {
                Key::Char('q') => game.quit(),
                Key::Char(' ') => game.do_player_radial_attack(),
                Key::Char('f') => game.smite_selected_square(),
                Key::Char('g') => game.do_player_spear_attack(),
                Key::Char('t') => game.do_player_shoot_arrow(),
                _ => {
                    if let Some(screen_direction) = Self::key_to_screen_direction(ke) {
                        if Self::is_shifted_key(ke) {
                            game.player_blink_relative_to_screen(screen_direction);
                        } else {
                            game.try_slide_player_relative_to_screen(screen_direction)
                                .ok();
                        }
                    }
                }
            },
            Event::Mouse(me) => match me {
                MouseEvent::Press(MouseButton::Left, term_x, term_y) => {
                    let square = self.screen_to_world(&(term_x, term_y));
                    game.try_set_player_position(square).unwrap_or_default();
                }
                MouseEvent::Press(MouseButton::Right, term_x, term_y) => {
                    self.prev_mouse_square = self.screen_to_world(&(term_x, term_y));
                }
                MouseEvent::Hold(term_x, term_y) => {
                    let square = self.screen_to_world(&(term_x, term_y));
                    //game.place_line_of_blocks(self.prev_mouse_pos, (x, y), game.selected_block);
                    game.try_set_player_position(square).unwrap_or_default();
                    self.prev_mouse_square = square;
                }
                _ => {}
            },
            _ => {}
        }
    }

    fn screen_to_world(&self, terminal_position: &(u16, u16)) -> WorldSquare {
        // terminal indexes from 1, and the y axis goes top to bottom
        WorldSquare::new(
            (terminal_position.0 as i32 - 1) / 2,
            self.terminal_size.1 as i32 - terminal_position.1 as i32,
        )
    }
}

#[cfg(test)]
mod tests {
    use ntest::timeout;
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    #[test]

    fn test_screen_to_world__lower_left() {
        let input_map = InputMap::new(100, 50);
        let terminal_pos: (u16, u16) = (1, 50);
        let world_pos = input_map.screen_to_world(&terminal_pos);
        let correct_world_pos = WorldSquare::new(0, 0);
        assert_eq!(correct_world_pos, world_pos);
    }

    #[test]

    fn test_screen_to_world__upper_left() {
        // 50 squares x 50 squares
        let input_map = InputMap::new(100, 50);
        let terminal_pos: (u16, u16) = (1, 1);
        let world_pos = input_map.screen_to_world(&terminal_pos);
        let correct_world_pos = WorldSquare::new(0, 49);
        assert_eq!(correct_world_pos, world_pos);
    }

    #[test]

    fn test_screen_to_world__lower_right() {
        let input_map = InputMap::new(100, 50);
        let terminal_pos: (u16, u16) = (100, 50);
        let world_pos = input_map.screen_to_world(&terminal_pos);
        let correct_world_pos = WorldSquare::new(49, 0);
        assert_eq!(correct_world_pos, world_pos);
    }

    #[test]

    fn test_screen_to_world__upper_right() {
        let input_map = InputMap::new(100, 50);
        let terminal_pos: (u16, u16) = (100, 1);
        let world_pos = input_map.screen_to_world(&terminal_pos);
        let correct_world_pos = WorldSquare::new(49, 49);
        assert_eq!(correct_world_pos, world_pos);
    }

    #[test]

    fn test_screen_to_world__two_characters_one_square() {
        let input_map = InputMap::new(100, 50);
        // odd to even, moving right, is same square
        let terminal_pos1: (u16, u16) = (35, 25);
        let terminal_pos2: (u16, u16) = (36, 25);
        let world_pos1 = input_map.screen_to_world(&terminal_pos1);
        let world_pos2 = input_map.screen_to_world(&terminal_pos2);
        assert_eq!(world_pos1, world_pos2);
    }
}
