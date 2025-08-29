pub mod glyph;
pub mod drawable_glyph;
use std::collections::HashMap;
use std::iter::once;
use std::iter::repeat_n;
use std::ops::Add;
use std::ops::Mul;
use std::ops::Sub;

use geometry2::FPoint;
use geometry2::FPointExt;
use geometry2::IPoint;
pub use glyph::*;
pub use drawable_glyph::*;

pub mod glyph_with_transparency;
pub use glyph_with_transparency::*;

pub mod screen;
use itertools::Itertools;
use ordered_float::OrderedFloat;
use rgb::RGBA8;
pub use screen::*;

pub mod frame;
pub use frame::*;


pub_mod_and_use!(angled_blocks, braille, floating_square, hextant_blocks);

pub mod glyph_constants;

use utility::geometry2::IPointExt;
pub use utility::*;

use crate::glyph::glyph_constants::named_colors::*;
use crate::glyph_constants::named_chars;
use crate::glyph_constants::FULL_BLOCK;

pub type DoubleChar = [char; 2];

#[derive(Hash, Debug, Copy, Clone, Eq, PartialEq)]
pub enum ConcatVBias {
    Top,
    Middle,
    Bottom,
}
pub trait MultilineStringExt: ToString {
    fn lines_including_trailing_newline(&self) -> std::str::Split<'_, char>;
    fn is_rectangular(&self) -> bool {
        self.lines_including_trailing_newline()
            .map(|line| line.visible_len())
            .all_equal()
    }
    fn visible_len(&self) -> usize {
        self.without_control_sequences().chars().count()
    }
    fn width(&self) -> usize {
        self.lines_including_trailing_newline()
            .map(|line| line.visible_len())
            .max()
            .unwrap()
    }
    fn without_control_sequences(&self) -> String {
        let control_char_regex = regex_static::static_regex!(r"\u{1b}\[(.*?)m");
        let x = control_char_regex.split(&self.to_string()).join("");
        x
    }
    fn height(&self) -> usize {
        self.lines_including_trailing_newline().count()
    }
    // dir is 0=right, 1=up, 2=left, 3=down
    fn pad_to_rectangle(&self) -> String {
        let w = self.width();
        self.lines_including_trailing_newline()
            .map(|line| line.to_string() + &" ".repeat(w - line.visible_len()))
            .join("\n")
    }
    fn pad_side(&self, dir: i32, n: usize) -> String {
        assert!(
            self.is_rectangular(),
            "frame: \n{}\n\n{:?}",
            self.to_string().replace(" ", "."),
            self.without_control_sequences()
                .lines_including_trailing_newline()
                .map(|l| l.visible_len())
                .collect_vec()
        );
        assert!(dir >= 0 && dir < 4);

        match dir {
            0 => self
                .lines_including_trailing_newline()
                .map(|line| line.to_string() + &" ".repeat(n))
                .join("\n"),
            1 => (" ".repeat(self.width()) + "\n").repeat(n) + &self.to_string(),
            2 => self
                .lines_including_trailing_newline()
                .map(|line| " ".repeat(n) + line)
                .join("\n"),
            3 => self.to_string() + &("\n".to_owned() + &" ".repeat(self.width())).repeat(n),
            _ => panic!("invalid direction: {dir}"),
        }
    }
    fn pad_to_height_with_vbias(&self, height: usize, vbias: ConcatVBias) -> String {
        let dheight = height - self.height();
        match vbias {
            ConcatVBias::Top => self.pad_side(3, dheight),
            ConcatVBias::Middle => self
                .pad_side(1, dheight / 2)
                .pad_side(3, dheight / 2 + dheight % 2),
            ConcatVBias::Bottom => self.pad_side(1, dheight),
        }
    }
    fn linewise_prefix(&self, prefix: impl Into<String>) -> String {
        let prefix = prefix.into();
        self.lines_including_trailing_newline()
            .map(|s| prefix.clone() + s)
            .join("\n")
    }
    fn indent(&self) -> String {
        self.linewise_prefix("\t")
    }
    fn framed(&self) -> String {
        //╭╮╯╰─│
        format!(
            "\

╭{horiz}╮
{contents}
╰{horiz}╯\
",
            horiz = "─".repeat(self.width()),
            contents = self
                .lines_including_trailing_newline()
                .map(|row| format!("│{row}│"))
                .join("\n")
        )
    }
}

impl MultilineStringExt for String {
    fn lines_including_trailing_newline(&self) -> std::str::Split<'_, char> {
        self.split('\n')
    }
}
impl MultilineStringExt for &str {
    fn lines_including_trailing_newline(&self) -> std::str::Split<'_, char> {
        self.split('\n')
    }
}

pub fn horiz_concat_strings(strings: &[String], spaces: usize) -> String {
    horiz_concat_strings_with_vbias(strings, spaces, ConcatVBias::Top)
}

pub fn horiz_concat_strings_with_vbias(
    strings: &[String],
    spaces: usize,
    vbias: ConcatVBias,
) -> String {
    let max_height = strings.iter().map(|col| col.height()).max().unwrap();
    let out_strings = strings
        .iter()
        .map(|col| {
            col.pad_to_rectangle()
                .pad_to_height_with_vbias(max_height, vbias)
        })
        .collect_vec();
    horiz_concat_equal_height_strings(&out_strings, spaces)
}
pub fn horiz_concat_equal_height_strings(strings: &[String], spaces: usize) -> String {
    let col_heights = strings.iter().map(|col| col.height()).collect_vec();
    assert!(col_heights.iter().all_equal(), "{col_heights:?}");
    assert!(strings.iter().map(|col| col.is_rectangular()).all(|x| x));
    let mut out = String::new();
    let num_cols = strings.len();
    let height = strings[0].height();
    let mut columns = strings
        .into_iter()
        .map(|s| s.lines().collect_vec())
        .collect_vec();
    (0..height)
        .map(|row| {
            (0..num_cols)
                .map(|col| columns[col][row])
                .join(&" ".repeat(spaces))
        })
        .join("\n")
}

pub fn char_point_to_local_braille_square(char_point: FPoint) -> IPoint {
    [
        (char_point[0].mul(2.0).add(0.5).round() as i32).rem_euclid(2),
        (char_point[1].mul(4.0).add(1.5).round() as i32).rem_euclid(4),
    ]
}

pub fn draw_points_in_character_grid(points: &[FPoint]) -> String {
    let char_map: HashMap<IPoint, char> = points
        .iter()
        .map(|&p| (p.rounded(), char_point_to_local_braille_square(p)))
        // .inspect(|p| {
        // println!("{}, {}", p.0.to_string(), p.1.to_string());
        // })
        .sorted()
        .group_by(|(char_pos, local_braille_pos)| *char_pos)
        .into_iter()
        .map(|(char_pos, pos_pair)| {
            let local_squares = pos_pair
                .into_iter()
                .map(|(char_pos, local_pos)| local_pos)
                .collect_vec();
            (
                char_pos,
                local_braille_squares_to_braille_char2(local_squares),
            )
        })
        .collect();
    let display_string = char_map_to_string(char_map);
    let mut frame = Frame::from_plain_string(&display_string);
    for g in frame.glyphs() {
        if char_is_braille(g.character) {
            g.fg_color = None;
            g.bg_color = grey(20).into();
        }
    }
    frame.string_for_regular_display()
}

pub fn val_to_column(val: f32, height: usize, max_abs: f32) -> Vec<DrawableGlyph> {
    assert!(max_abs > 0.0, "{max_abs} > 0.0");

    let val_in_blocks = (val / max_abs) * height as f32;

    let (num_full_blocks, maybe_last_glyph) = if val_in_blocks.abs() > max_abs {
        (
            height - 1,
            Some( DrawableGlyph::new('+', BLACK.into(), WHITE.into())),
        )
    } else {
        let num_full_blocks = val_in_blocks.trunc();
        let remainder_blocks = val_in_blocks - num_full_blocks;
        let num_full_blocks = num_full_blocks as usize;

        if remainder_blocks == 0.0 {
            (num_full_blocks, None)
        } else {
            // remainder -> offset
            // 0.0 -> 1.0 or -1.0
            // 0.99 -> -0.01
            // -0.99 -> 0.01
            // 0.5 -> -0.5
            // -0.5 -> 0.5
            // x -> -sign * (1.0-x.abs())
            let floating_square_offset = -sign(remainder_blocks) * (1.0 - remainder_blocks.abs());
            let remainder_character = floating_square::character_for_half_square_with_1d_offset(
                true,
                floating_square_offset,
            );
            match remainder_character {
                named_chars::FULL_BLOCK => (num_full_blocks + 1, None),
                named_chars::SPACE => (num_full_blocks, None),
                _ => (
                    num_full_blocks,
                    Some(
                        DrawableGlyph::from_char(remainder_character) 
                    ),
                ),
            }
        }
    };


    let full_block_glyph = DrawableGlyph::from_char(FULL_BLOCK);
    let num_occupied_squares =num_full_blocks + if maybe_last_glyph.is_some() { 1 } else { 0 }; 
    let num_blanks = height - num_occupied_squares;


    let mut out: Vec<DrawableGlyph> =
        repeat_n(DrawableGlyph::default(), num_blanks)
            .chain(once(maybe_last_glyph).filter_map(|x| x))
            .chain(repeat_n(full_block_glyph, num_full_blocks))
            .collect_vec();

    if val < 0.0 {
        out.reverse();
    }
    assert_eq!(out.len(), height);
    out
}

pub fn signed_bargraph(data: &[f32], height: usize, min: Option<f32>, max: Option<f32>) -> String {
    min.map(|x| assert!(x <= 0.0));
    max.map(|x| assert!(x >= 0.0));

    let min = min.unwrap_or_else(|| data.iter().cloned().reduce(|a, b| a.min(b)).unwrap());
    let max = max.unwrap_or_else(|| data.iter().cloned().reduce(|a, b| a.max(b)).unwrap());

    if max == min {
        return "Graph is flat".to_string();
    }

    // zero must fall on a character boundary, because that's where colors switch
    // either max or min is locked in place, while the other shifts to allow for a full integer
    // number of blocks in the total height

    let naive_height_per_dist = height as f32 / (max - min);

    let naive_max_in_blocks = max * naive_height_per_dist;
    let naive_min_in_blocks = min * naive_height_per_dist;

    let remainder_on_positive_side = naive_max_in_blocks.fract();

    let positive_height;
    let negative_height;
    let height_per_dist;

    let (min_in_blocks, max_in_blocks) = if remainder_on_positive_side == 0.0 {
        positive_height = naive_max_in_blocks.floor() as usize;
        negative_height = height - positive_height;
        height_per_dist = naive_height_per_dist;
        (naive_min_in_blocks, naive_max_in_blocks)
    } else if remainder_on_positive_side < 0.5 {
        // keep max and expand the negative region
        positive_height = naive_max_in_blocks.floor() as usize;
        negative_height = height - positive_height;

        height_per_dist = positive_height as f32 / naive_max_in_blocks;
        let min_in_blocks = negative_height as f32 / height_per_dist * -1.0;
        (min_in_blocks, naive_max_in_blocks)
    } else {
        // keep min and expand the positive region
        negative_height = naive_min_in_blocks.abs().floor() as usize;
        positive_height = height - negative_height;

        height_per_dist = (negative_height as f32 / naive_min_in_blocks).abs();
        let max_in_blocks = positive_height as f32 / height_per_dist;
        (naive_min_in_blocks, max_in_blocks)
    };

    let min = min_in_blocks / height_per_dist;
    let max = max_in_blocks / height_per_dist;

    // dbg!(positive_height, negative_height, min, max, height_per_dist);

    let col_func = |val: f32| -> String {
        let background_glyph = DrawableGlyph::default();
        let col_glyphs = if val >= 0.0 {
            let pos_glyphs = val_to_column(val, positive_height, max);
            assert_eq!(pos_glyphs.len(), positive_height);
            let neg_glyphs = repeat_n(background_glyph, negative_height);
            pos_glyphs.into_iter().chain(neg_glyphs).collect_vec()
        } else {
            let pos_glyphs = repeat_n(background_glyph, positive_height);
            let neg_glyphs = val_to_column(val, negative_height, min.abs());
            assert_eq!(neg_glyphs.len(), negative_height);
            pos_glyphs.into_iter().chain(neg_glyphs).collect_vec()
        };
        col_glyphs.into_iter().map(|g| g.to_string()).join("\n")
    };

    let columns = data.iter().map(|x| col_func(*x)).collect_vec();
    let graph = horiz_concat_equal_height_strings(&columns, 0).framed();

    let graph = graph
        .lines()
        .enumerate()
        .map(|(i, l)| {
            let appendish = |s: String| {
                l.chars()
                    .into_iter()
                    .take(data.len() + 1)
                    .collect::<String>()
                    + &s
            };
            if i == 0 {
                appendish(format!("┬─{max}"))
            } else if i == height + 1 {
                appendish(format!("┴─{min}"))
            } else if i == positive_height {
                appendish("🭽▔0.0".to_string())
            } else {
                l.to_string()
            }
        })
        .join("\n");

    format!("{graph}")
}

pub fn bargraph(data: &[f32], height: usize, max: Option<f32>) -> String {
    let max = max.unwrap_or_else(|| data.iter().cloned().reduce(|a, b| a.max(b)).unwrap());
    assert!(data.iter().all(|&x| x >= 0.0));

    let col_func = |val: f32| -> String {
        val_to_column(val, height, max)
            .into_iter()
            .map(|g| g.to_string())
            .join("\n")
    };

    let columns = data.iter().map(|x| col_func(*x)).collect_vec();
    let graph = horiz_concat_strings_with_vbias(&columns, 0, ConcatVBias::Bottom).framed();

    let graph = graph
        .lines()
        .enumerate()
        .map(|(i, l)| {
            if i == 0 {
                l.chars()
                    .into_iter()
                    .take(data.len() + 1)
                    .collect::<String>()
                    + "┬─"
                    + &format!("{max}")
            } else if i == height + 1 {
                l.chars()
                    .into_iter()
                    .take(data.len() + 1)
                    .collect::<String>()
                    + "┴─"
                    + "0.0"
            } else {
                l.to_string()
            }
        })
        .join("\n");

    format!("{graph}")
}

pub mod test_utils {
    use super::*;
    use std::str::FromStr;
    pub use stdext::function_name;
    use std::path::PathBuf;


    #[macro_export]
    macro_rules! data_dir_for_test {
        () => {
            {
                let main_dir: PathBuf = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/");

                let test_name: String = function_name!().replace(":", "_");

                let test_dir = main_dir.join(test_name);

                std::fs::create_dir_all(&test_dir).ok();

                test_dir
            }
        };
    }

    #[macro_export]
    macro_rules! assert_value_not_less_than_past {
        ($val:expr, $key:expr) => {
            let file_path = data_dir_for_test!().join($key).with_extension("txt");
            assert_each_of_array_not_fn_than_past_fn(&[$val], file_path, f32::lt, |new, past| format!("Error: {new}<{past}"))
        };
    }
    #[macro_export]
    macro_rules! assert_value_not_more_than_past {
        ($val:expr, $key:expr) => {
            let file_path = data_dir_for_test!().join($key).with_extension("txt");
            assert_each_of_array_not_fn_than_past_fn(&[$val], file_path, f32::gt, |new, past| format!("Error: {new}>{past}"))
        };
    }
    #[macro_export]
    macro_rules! assert_array_not_less_than_past {
        ($val:expr, $key:expr) => {
            let file_path = data_dir_for_test!().join($key).with_extension("txt");
            assert_each_of_array_not_fn_than_past_fn($val, file_path, f32::lt, |new, past| format!("Error: {new}<{past}"))
        };
    }


    #[macro_export]
    macro_rules! get_past_array {
        ($key:expr) => { {
            let file_path = data_dir_for_test!().join($key).with_extension("txt");
            // file_path.read
            // assert_each_of_array_not_fn_than_past_fn($val, file_path, f32::gt, |new, past| format!("Error: {new}>{past}"))
            get_blessed_string(file_path).unwrap().lines().map(|line|
                 f32::from_str(line).unwrap()
            ).collect_vec()
        } };
    }

    #[macro_export]
    macro_rules! assert_array_not_more_than_past {
        ($val:expr, $key:expr) => {
            let file_path = data_dir_for_test!().join($key).with_extension("txt");
            assert_each_of_array_not_fn_than_past_fn($val, file_path, f32::gt, |new, past| format!("Error: {new}>{past}"))
        };
    }

    #[macro_export]
    macro_rules! assert_frame_same_as_past {
        ($frame:ident, $key:expr, $verbose:expr) => {
            if !$key.is_empty() {
                println!("Key: {}", $key);
            }
            let file_path = data_dir_for_test!().join($key).with_extension("txt");
            assert_frame_same_as_past_fn($frame, file_path, $verbose)
        };
        ($frame:ident, $key:expr) => {
            assert_frame_same_as_past!($frame, $key, false)
        };
    }

    fn get_or_set_blessed_string(candidate: String, path: PathBuf) -> Option<String> {
        const BLESS_NEWBORNS: bool = false;

        let blessed = option_env!("BLESS_TESTS").is_some() || (BLESS_NEWBORNS && !path.is_file());
        if blessed {
            std::fs::write(path, candidate).unwrap();
            return None;
        }

        let correct_string = std::fs::read_to_string(path.clone()).expect(
        &format!("No existing test output found.  Set BLESS_TESTS to canonize current candidate:\n\n{candidate:?}"),
        );
        Some(correct_string)

    }
    pub fn get_blessed_string(path: PathBuf) -> Option<String> {
        macro_rules! the_var { () => { "ALLOW_SKIP_BLESSED_FILE"}}
        if option_env!(the_var!()).is_some() {
            return None;
        }

        Some( std::fs::read_to_string(path.clone()).expect(
        &format!("No existing blessed file found at {}.  Set {} to skip.", path.display(), the_var!()),
        ))
    }


    pub fn assert_each_of_array_not_fn_than_past_fn(candidate_value: &[f32], file_path: PathBuf, fail_func: fn(&f32, &f32) -> bool, format_func: fn(f32, f32) -> String) {
        let candidate_string = candidate_value.iter().map(|x|x.to_string()).join("\n");
        let Some(correct_string) = get_or_set_blessed_string(candidate_string, file_path) else {
            return;
        };
        let array_len = candidate_value.len();
        correct_string.lines().enumerate().zip(candidate_value).for_each(|((i, line), &candidate_value)| {

            let correct_val = f32::from_str(line).unwrap();
            assert!(!fail_func(&candidate_value, &correct_val), "{}{}", format_func(candidate_value, correct_val), if array_len > 1 {format!(" at index {i}")} else {"".to_string()});
        });
        // TODO: replace file if passed and different

    }

    pub fn assert_frame_same_as_past_fn(candidate_frame: Frame, blessed_file_path: PathBuf, verbose: bool) {
        let candidate_string = candidate_frame.string_for_regular_display();


        let Some(correct_string) = get_or_set_blessed_string(candidate_string, blessed_file_path) else {
            return;
        };

        let correct_frame = Frame::parse_regular_display_string(
        correct_string
        );

        let good = candidate_frame.string_for_regular_display() == correct_frame.string_for_regular_display();

        if !good {

            let f = if verbose {
                |frame: &Frame| format!("Frame:\n{}\n\nRaw:\n{}\n\nHuman readable:\n{}\n", 
                    format!("{:?}", &frame).indent(), 
                    frame.escaped_regular_display_string().indent(), frame.readable_string().indent())
            } else {
                |frame: &Frame| format!("Frame:\n{:?}\n", &frame)
            };



            let mut err_string = 
            format!(
            "Frames do not match.  Set the BLESS_TESTS env var to lock-in current string as correct.\n\nCorrect:\n{}\n\nGiven:\n{}\n\nDifferences only:\n{diff1:?}\n\n{diff2:?}\n",
            f(&correct_frame).indent(),
            f(&candidate_frame).indent(),
            diff1 = correct_frame.diff_from(&candidate_frame),
            diff2 = candidate_frame.diff_from(&correct_frame)
        );


            err_string += "❌";
        assert!(good, "{}", err_string);

        }


        eprintln!("{candidate_frame:?}\n✅");
    }

}

#[cfg(test)]
mod tests {
    use std::f32::consts::TAU;

    use pretty_assertions::{assert_eq, assert_ne};
    use stdext::function_name;
    use std::path::PathBuf;
    use test_utils::*;

    use crate::glyph_constants::LOWER_HALF_BLOCK;

    use super::*;

    #[test]
    fn test_horiz_concat() {
        let a = "a";
        let b = " \nb\n ";
        let c = "\nc\n";
        let s = [a, b, c].map(|x| x.to_string());

        let l = [
            (ConcatVBias::Top, "a  \n bc\n   "),
            (ConcatVBias::Middle, "   \nabc\n   "),
            (ConcatVBias::Bottom, "   \n bc\na  "),
        ];

        for (bias, good) in l {
            let r = horiz_concat_strings_with_vbias(&s, 0, bias);
            dbg!(&r, &good);
            assert_eq!(
                r,
                good,
                "\nr:\n{:?}\n\ngood:\n{:?}",
                r.replace(" ", "."),
                good.replace(" ", ".")
            );
        }
    }
    #[test]
    fn test_string_pad() {
        assert_eq!("a".pad_side(0, 2), "a  ".to_string());
        assert_eq!("a".pad_side(0, 2).pad_side(1, 1), "   \na  ".to_string());
        assert_eq!("a".pad_side(1, 2), " \n \na".to_string());
        assert_eq!("a".pad_side(2, 2), "  a".to_string());
        assert_eq!("a".pad_side(2, 2).pad_side(0, 2), "  a  ".to_string());
    }
    #[test]
    fn test_print_braille_points_simple() {
        let p = (0..20)
            .map(|t| [t as f32 * 0.2, t as f32 * 0.1])
            .collect_vec();
        let out_string = draw_points_in_character_grid(&p);
        let correct_string = "   ⢀⠄
 ⢀⠔⠁ 
⠐⠁   ";
        println!("{}", out_string);
        println!("out:\n{:?}\n\ncorrect:\n{:?}", out_string, correct_string);
        assert_eq!(&out_string, correct_string);
    }
    #[test]
    fn test_print_braille_points() {
        let p = (0..100)
            .map(|t| [t as f32 / 3.0, (t as f32 * TAU / 100.0).sin() * 3.0])
            .collect_vec();
        let out_string = draw_points_in_character_grid(&p);

        let correct_string = "     ⢀⣠⠤⠴⠤⠤⣀                      
   ⢠⠔⠉      ⠙⠢⡀                   
 ⢀⠜⠁          ⠈⢢⡀                 
⠐⠁              ⠘⢤               ⡠
                  ⠙⢄           ⣠⠊ 
                    ⠑⢤⡀     ⢀⡠⠚   
                      ⠈⠑⠒⠲⠒⠚⠉     ";
        println!("out:\n{}\n\ncorrect:\n{}", out_string, correct_string);
        assert_eq!(&out_string, correct_string);
    }
    #[test]
    fn test_val_to_column() {
        let h = 4;
        let col = val_to_column(1.5, h, 4.0);
        
        let frame = Frame::from_column_glyphs(col);
        dbg!(&frame);
        assert_eq!(frame.height(), h);
        assert_eq!(frame.width(), 1);
        assert_eq!(frame.get_xy([0,1]).character, LOWER_HALF_BLOCK);
        assert_eq!(frame.get_xy([0,0]).character, FULL_BLOCK);
        assert_frame_same_as_past!(frame, "a", true);
    }

}
