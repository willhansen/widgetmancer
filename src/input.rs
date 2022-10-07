use termion::event::{Event, Key, MouseButton, MouseEvent};
use crate::game::Game;
use euclid::default::Point2D;
use euclid::point2 as p;
use crate::{DOWN_I, LEFT_I, RIGHT_I, UP_I};


pub struct Input {
    prev_mouse_pos: Point2D<i32>,
}
impl Input {
    fn handle_event(&mut self, mut game: &Game, evt: Event) {
        match evt {
            Event::Key(ke) => match ke {
                Key::Char('q') => game.quit(),
                Key::Char('k') | Key::Char('w') | Key::Up => game.player_step(UP_I),
                Key::Char('h') | Key::Char('a') | Key::Left => game.player_step(LEFT_I),
                Key::Char('j') | Key::Char('s') | Key::Down => game.player_step(DOWN_I),
                Key::Char('l') | Key::Char('d') | Key::Right => game.player_step(RIGHT_I),
                _ => {}
            },
            Event::Mouse(me) => match me {
                MouseEvent::Press(MouseButton::Left, term_x, term_y) => {
                    let (x, y) = self.screen_to_world(&(term_x, term_y));
                    self.prev_mouse_pos = p(x, y);
                }
                MouseEvent::Press(MouseButton::Right, term_x, term_y) => {
                    let (x, y) = self.screen_to_world(&(term_x, term_y));
                    game.place_player(x as f32, y as f32);
                }
                MouseEvent::Hold(term_x, term_y) => {
                    let (x, y) = self.screen_to_world(&(term_x, term_y));
                    game.place_line_of_blocks(self.prev_mouse_pos, (x, y), game.selected_block);
                    self.prev_mouse_pos = p(x, y);
                }
                _ => {}
            },
            _ => {}
        }
    }

    fn screen_to_world(&self, terminal_position: &(u16, u16)) -> (i32, i32) {
        // terminal indexes from 1, and the y axis goes top to bottom
        (
            terminal_position.0 as i32 - 1,
            self.terminal_size.1 as i32 - terminal_position.1 as i32,
        )
    }

}

