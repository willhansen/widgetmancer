use euclid::default::Point2D;
use euclid::*;
use termion::event::{Event, Key, MouseButton, MouseEvent};

use crate::game::Game;
use crate::utility::coordinate_frame_conversions::*;
use crate::utility::{FVector, IVector};
use crate::{DOWN_I, LEFT_I, RIGHT_I, UP_I};

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

    fn char_to_direction(c: char) -> IVector {
        match c.to_lowercase().next().unwrap() {
            'k' | 'w' => UP_I,
            'j' | 's' => DOWN_I,
            'h' | 'a' => LEFT_I,
            'l' | 'd' => RIGHT_I,
            'y' => UP_I + LEFT_I,
            'u' => UP_I + RIGHT_I,
            'b' => DOWN_I + LEFT_I,
            'n' => DOWN_I + RIGHT_I,
            _ => vec2(0, 0),
        }
    }

    pub fn handle_event(&mut self, game: &mut Game, evt: Event) {
        match evt {
            Event::Key(ke) => match ke {
                Key::Char('q') => game.quit(),
                Key::Char(' ') => game.do_player_radial_attack(),
                Key::Char('f') => game.smite_selected_square(),
                Key::Char('g') => game.do_player_spear_attack(),

                Key::Char('k') | Key::Char('w') | Key::Up => {
                    game.try_move_player(UP_I.cast_unit()).ok();
                }
                Key::Char('h') | Key::Char('a') | Key::Left => {
                    game.try_move_player(LEFT_I.cast_unit()).ok();
                }
                Key::Char('j') | Key::Char('s') | Key::Down => {
                    game.try_move_player(DOWN_I.cast_unit()).ok();
                }
                Key::Char('l') | Key::Char('d') | Key::Right => {
                    game.try_move_player(RIGHT_I.cast_unit()).ok();
                }

                Key::Char('y') => {
                    game.try_move_player((UP_I + LEFT_I).cast_unit()).ok();
                }
                Key::Char('u') => {
                    game.try_move_player((UP_I + RIGHT_I).cast_unit()).ok();
                }
                Key::Char('b') => {
                    game.try_move_player((DOWN_I + LEFT_I).cast_unit()).ok();
                }
                Key::Char('n') => {
                    game.try_move_player((DOWN_I + RIGHT_I).cast_unit()).ok();
                }

                Key::Char('K') | Key::Char('W') => {
                    game.player_blink(UP_I.cast_unit());
                }
                Key::Char('H') | Key::Char('A') => {
                    game.player_blink(LEFT_I.cast_unit());
                }
                Key::Char('J') | Key::Char('S') => {
                    game.player_blink(DOWN_I.cast_unit());
                }
                Key::Char('L') | Key::Char('D') => {
                    game.player_blink(RIGHT_I.cast_unit());
                }

                Key::Char('Y') => {
                    game.player_blink((UP_I + LEFT_I).cast_unit());
                }
                Key::Char('U') => {
                    game.player_blink((UP_I + RIGHT_I).cast_unit());
                }
                Key::Char('B') => {
                    game.player_blink((DOWN_I + LEFT_I).cast_unit());
                }
                Key::Char('N') => {
                    game.player_blink((DOWN_I + RIGHT_I).cast_unit());
                }

                _ => {}
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
