use crate::game::Game;
use crate::{Square, DOWN_I, LEFT_I, RIGHT_I, UP_I};
use euclid::default::Point2D;
use euclid::*;
use termion::event::{Event, Key, MouseButton, MouseEvent};

pub struct InputMap {
    prev_mouse_pos: Point2D<i32>,
    terminal_size: (u16, u16), // (width, height)
}

impl InputMap {
    pub fn new(width: u16, height: u16) -> InputMap {
        InputMap {
            prev_mouse_pos: point2((width / 2) as i32, (height / 2) as i32),
            terminal_size: (width, height),
        }
    }

    pub fn handle_event(&mut self, game: &mut Game, evt: Event) {
        match evt {
            Event::Key(ke) => match ke {
                Key::Char('q') => game.quit(),

                Key::Char('k') | Key::Char('w') | Key::Up => {
                    game.move_player(UP_I.cast_unit()).unwrap_or_default()
                }
                Key::Char('h') | Key::Char('a') | Key::Left => {
                    game.move_player(LEFT_I.cast_unit()).unwrap_or_default()
                }
                Key::Char('j') | Key::Char('s') | Key::Down => {
                    game.move_player(DOWN_I.cast_unit()).unwrap_or_default()
                }
                Key::Char('l') | Key::Char('d') | Key::Right => {
                    game.move_player(RIGHT_I.cast_unit()).unwrap_or_default()
                }

                Key::Char('y') => game
                    .move_player((UP_I + LEFT_I).cast_unit())
                    .unwrap_or_default(),
                Key::Char('u') => game
                    .move_player((UP_I + RIGHT_I).cast_unit())
                    .unwrap_or_default(),
                Key::Char('b') => game
                    .move_player((DOWN_I + LEFT_I).cast_unit())
                    .unwrap_or_default(),
                Key::Char('n') => game
                    .move_player((DOWN_I + RIGHT_I).cast_unit())
                    .unwrap_or_default(),

                _ => {}
            },
            Event::Mouse(me) => match me {
                MouseEvent::Press(MouseButton::Left, term_x, term_y) => {
                    let (x, y) = self.screen_to_world(&(term_x, term_y));
                    let square = Square::new(x, y);
                    game.set_player_position(&square).unwrap_or_default();
                }
                MouseEvent::Press(MouseButton::Right, term_x, term_y) => {
                    let (x, y) = self.screen_to_world(&(term_x, term_y));
                    self.prev_mouse_pos = point2(x, y);
                }
                MouseEvent::Hold(term_x, term_y) => {
                    let (x, y) = self.screen_to_world(&(term_x, term_y));
                    //game.place_line_of_blocks(self.prev_mouse_pos, (x, y), game.selected_block);
                    self.prev_mouse_pos = point2(x, y);
                }
                _ => {}
            },
            _ => {}
        }
    }

    fn screen_to_world(&self, terminal_position: &(u16, u16)) -> Square {
        // terminal indexes from 1, and the y axis goes top to bottom
        Square::new(
            terminal_position.0 as i32 - 1,
            self.terminal_size.1 as i32 - terminal_position.1 as i32,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn test_screen_to_world__lower_left() {
        let input_map = InputMap::new(100, 50);
        let terminal_pos: (u16, u16) = (1, 50);
        let world_pos = input_map.screen_to_world(&terminal_pos);
        let correct_world_pos = Square::new(0, 0);
        assert_eq!(correct_world_pos, world_pos);
    }

    #[test]
    fn test_screen_to_world__upper_left() {
        let input_map = InputMap::new(100, 50);
        let terminal_pos: (u16, u16) = (1, 1);
        let world_pos = input_map.screen_to_world(&terminal_pos);
        let correct_world_pos = Square::new(0, 49);
        assert_eq!(correct_world_pos, world_pos);
    }
}
