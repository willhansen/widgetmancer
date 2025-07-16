use crate::*;

use itertools::Itertools;
use std::fmt::Display;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

#[derive(PartialEq, Clone)]
pub struct Frame {
    pub grid: Vec<Vec<DoubleGlyph>>,
}

impl Frame {
    pub fn save_to_file(&self, path: PathBuf) {
        File::create(path)
            .unwrap()
            .write_all(&self.string_for_regular_display().as_bytes());
    }
    pub fn width(&self) -> usize {
        self.grid[0].len()
    }
    pub fn height(&self) -> usize {
        self.grid.len()
    }

    pub fn row_to_y(&self, row: usize) -> usize {
         self.height() - row - 1

    }
    pub fn y_to_row(&self, y: usize) -> usize {
        // Symmetric transform
        self.row_to_y(y)

    }

    pub fn get(&self, frame_point: [usize; 2]) -> Glyph {
        let [x, y] = frame_point;
        assert!(x >= 0 && y >= 0 && x < self.width() * 2 && y < self.height());
        let col = x / 2;
        let row = self.y_to_row(y);
        let double_glyph_index = x as usize % 2;
        self.grid[row][col][double_glyph_index]
    }

    pub fn framed(&self) -> String {
        //╭╮╯╰─│
        format!(
            "\
╭{horiz}╮
{contents}
╰{horiz}╯\
",
            horiz = "─".repeat(self.width() * 2),
            contents = self
                .string_for_regular_display()
                .lines()
                .map(|row| format!("│{row}│"))
                .join("\n")
        )
    }
    pub fn string_for_regular_display(&self) -> String {
        self.grid
            .iter()
            .map(|row| row.iter().map(|s| s.to_string()).join(""))
            .join("\n")
    }
    pub fn bytes_for_raw_display_over(&self, maybe_old_frame: &Option<Frame>) -> Vec<u8> {
        let mut output = String::new();

        let mut prev_written_row_col: Option<[usize; 2]> = None;
        for frame_x in 0..self.width() {
            for frame_y in 0..self.height() {
                let col = frame_x;
                let row = self.height() - frame_y - 1;
                let new_glyphs = self.grid[row][col].to_string();

                if let Some(old_frame) = maybe_old_frame {
                    let old_glyphs = old_frame.grid[row][col].to_string();
                    let this_square_is_same = new_glyphs == old_glyphs;
                    if this_square_is_same {
                        continue;
                    }
                }
                let just_next_horizontally = prev_written_row_col
                    .is_some_and(|[prev_row, prev_col]| row == prev_row && prev_col == col - 1);

                let should_do_linewrap = prev_written_row_col.is_some_and(|[prev_row, prev_col]| {
                    prev_row + 1 == row && col == 0 && prev_col + 1 == self.width()
                });
                let directly_below = prev_written_row_col
                    .is_some_and(|[prev_row, prev_col]| prev_row + 1 == row && col == prev_col);

                if just_next_horizontally {
                    // Do nothing
                } else if should_do_linewrap {
                    output += "\n\r";
                } else if directly_below {
                    output += "\n";
                } else {
                    output +=
                        &termion::cursor::Goto((row + 1) as u16, (col + 1) as u16).to_string();
                }

                output += &new_glyphs.to_string();

                prev_written_row_col = Some([row, col]);
            }
        }
        output.into_bytes()
    }
}

impl Display for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.framed())
    }
}
impl std::fmt::Debug for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&("\n".to_string() + &self.framed()))
    }
}
