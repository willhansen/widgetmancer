use geo::Point;
use crate::Glyph;

struct Graphics {
    output_buffer: Vec<Vec<Glyph>>, // (x,y), left to right, top to bottom
    output_on_screen: Vec<Vec<Glyph>>, // (x,y), left to right, top to bottom
    terminal_size: (u16, u16),  // (width, height)
}

impl Graphics {

    fn new(width: u16, height: u16) -> Game {
        Graphics {
            output_buffer: vec![vec![Glyph::from_char(' '); height as usize]; width as usize],
            output_on_screen: vec![vec![Glyph::from_char('x'); height as usize]; width as usize],
            terminal_size: (width, height),
        }
    }

    fn world_to_screen(&self, world_position: &(i32, i32)) -> (u16, u16) {
        // terminal indexes from 1, and the y axis goes top to bottom
        // world indexes from 0, origin at bottom left
        (
            world_position.0 as u16 + 1,
            (self.terminal_size.1 as i32 - world_position.1) as u16,
        )
    }

    fn braille_bresenham_line_points(
        start_pos: Point<f32>,
        end_pos: Point<f32>,
    ) -> Vec<Point<f32>> {
        let braille_pos0 = Glyph::world_pos_to_braille_pos(start_pos);
        let braille_pos1 = Glyph::world_pos_to_braille_pos(end_pos);

        line_drawing::Bresenham::new(
            snap_to_grid(braille_pos0).x_y(),
            snap_to_grid(braille_pos1).x_y(),
        )
            .map(|(x, y)| Glyph::braille_pos_to_world_pos(p(x as f32, y as f32)))
            .collect()
    }

    fn count_braille_dots_in_square(&self, square: Point<i32>) -> i32 {
        return if self.square_is_in_world(square) {
            Glyph::count_braille_dots(self.get_buffered_glyph(square).character)
        } else {
            0
        };
    }

    fn draw_visual_braille_point(&mut self, pos: Point<f32>, color: ColorName) {
        self.draw_visual_braille_line(pos, pos, color);
    }

    fn draw_visual_braille_line(
        &mut self,
        start_pos: Point<f32>,
        end_pos: Point<f32>,
        color: ColorName,
    ) {
        let squares_to_place =
            Glyph::get_glyphs_for_colored_braille_line(start_pos, end_pos, color);

        let start_grid_square = snap_to_grid(start_pos);
        let end_grid_square = snap_to_grid(end_pos);
        let bottom_square_y = min(start_grid_square.y(), end_grid_square.y());
        let left_square_x = min(start_grid_square.x(), end_grid_square.x());

        for i in 0..squares_to_place.len() {
            for j in 0..squares_to_place[0].len() {
                if let Some(new_glyph) = squares_to_place[i][j] {
                    let grid_square = p(left_square_x + i as i32, bottom_square_y + j as i32);
                    if !self.square_is_in_world(grid_square) {
                        continue;
                    }
                    let grid_glyph =
                        &mut self.output_buffer[grid_square.x() as usize][grid_square.y() as usize];
                    if Glyph::is_braille(grid_glyph.character) {
                        let combined_braille =
                            Glyph::add_braille(grid_glyph.character, new_glyph.character);
                        *grid_glyph = new_glyph;
                        grid_glyph.character = combined_braille;
                    } else {
                        *grid_glyph = new_glyph;
                    }
                }
            }
        }
    }

    fn clear(&mut self) {
        let (width, height) = termion::terminal_size().unwrap();
        self.grid = vec![vec![Block::Air; height as usize]; width as usize];
    }
}