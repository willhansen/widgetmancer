use std::collections::HashSet;
use std::f32::consts::PI;

use euclid::*;
use rgb::RGB8;
use strum::IntoEnumIterator;
use strum_macros::Display;
use strum_macros::EnumIter;

use crate::glyph::DoubleGlyph;
use crate::glyph_constants::*;
use crate::piece::PieceType::*;
use crate::utility::coordinate_frame_conversions::*;
use crate::utility::{
    adjacent_king_steps, get_new_rng, random_choice, DIAGONAL_STEPS, KING_STEPS, ORTHOGONAL_STEPS,
    STEP_RIGHT,
};
use crate::{get_4_rotations_of, get_8_quadrants_of, quarter_turns_counter_clockwise, Glyph};

pub const MAX_PIECE_RANGE: u32 = 5;

#[derive(Debug, Display, Copy, Clone, Eq, PartialEq, EnumIter)]
pub enum Upgrade {
    BlinkRange,
}

#[derive(Debug, Display, Copy, Clone, Eq, PartialEq, EnumIter, Hash)]
pub enum PieceType {
    OmniDirectionalPawn,
    TurningPawn,
    OmniDirectionalSoldier,
    TurningSoldier,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
    DeathCubeTurret,
    Arrow,
}

const TURNING_PIECE_TYPES: &'static [PieceType] = &[TurningPawn, TurningSoldier, Arrow];

#[derive(Debug, Copy, Clone, Default, Eq, PartialEq, Hash)]
pub struct Faction {
    pub(crate) id: u32,
}

impl Faction {
    pub fn from_id(id: u32) -> Faction {
        Faction { id }
    }
}

pub struct FactionFactory {
    id_of_next_faction: u32,
}

impl FactionFactory {
    pub fn new() -> FactionFactory {
        FactionFactory {
            // Default faction is 0.  This is also the first generated.
            id_of_next_faction: 0,
        }
    }
    pub fn get_new_faction(&mut self) -> Faction {
        let faction = Faction::from_id(self.id_of_next_faction);
        self.id_of_next_faction += 1;
        faction
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone, Hash)]
pub struct Piece {
    pub piece_type: PieceType,
    pub faction: Faction,
    faced_direction: Option<WorldStep>,
}

impl Piece {
    pub fn pawn() -> Piece {
        Piece::from_type(OmniDirectionalPawn)
    }
    pub fn knight() -> Piece {
        Piece::from_type(Knight)
    }
    pub fn bishop() -> Piece {
        Piece::from_type(Bishop)
    }
    pub fn rook() -> Piece {
        Piece::from_type(Rook)
    }
    pub fn queen() -> Piece {
        Piece::from_type(Queen)
    }
    pub fn king() -> Piece {
        Piece::from_type(King)
    }
    pub fn arrow(dir: WorldStep) -> Piece {
        let mut piece = Piece::from_type(Arrow);
        piece.set_faced_direction(dir);
        piece
    }

    pub fn faced_direction(&self) -> WorldStep {
        if let Some(dir) = self.faced_direction {
            dir
        } else {
            panic!("Piece isn't directional!");
        }
    }

    pub fn set_faced_direction(&mut self, dir: WorldStep) {
        assert!(self.can_turn());
        self.faced_direction = Some(dir);
    }

    pub fn can_turn(&self) -> bool {
        self.faced_direction.is_some()
    }

    pub fn turned_by_quarters(&self, quarter_turns: i32) -> Piece {
        assert!(self.can_turn());
        Piece {
            piece_type: self.piece_type,
            faction: self.faction,
            faced_direction: Some(quarter_turns_counter_clockwise(
                &self.faced_direction(),
                quarter_turns,
            )),
        }
    }

    pub fn turned_versions(&self) -> HashSet<Piece> {
        assert!(self.can_turn());

        [-1, 1]
            .into_iter()
            .map(|i| self.turned_by_quarters(i))
            .collect()
    }

    pub fn random_subordinate_type() -> PieceType {
        let mut rng = get_new_rng();
        let options = vec![OmniDirectionalPawn, Knight, Bishop, Rook, Queen];
        *random_choice(&mut rng, &options)
    }

    pub fn glyphs(&self) -> DoubleGlyph {
        Piece::glyphs_for_type(self.piece_type)
    }

    pub fn glyphs_for_type(piece_type: PieceType) -> DoubleGlyph {
        Piece::chars_for_type(piece_type)
            .map(|character| Glyph::fg_only(character, ENEMY_PIECE_COLOR))
    }

    pub fn chars_for_type(piece_type: PieceType) -> [char; 2] {
        match piece_type {
            TurningPawn => ['♟', ' '],
            TurningSoldier => ['S', 'o'],
            OmniDirectionalPawn => ['O', 'P'],
            OmniDirectionalSoldier => ['O', 'S'],
            Knight => ['♞', ' '],
            Bishop => ['♝', ' '],
            Rook => ['♜', ' '],
            Queen => ['♛', ' '],
            King => ['♚', ' '],
            DeathCubeTurret => ['𐄳', ' '],
            _ => panic!("Tried to draw unknown piece type: {:?}", piece_type),
        }
    }

    pub fn move_directions_for_type(piece_type: PieceType) -> StepList {
        match piece_type {
            Bishop => get_4_rotations_of(WorldStep::new(1, 1)),
            Rook => get_4_rotations_of(WorldStep::new(1, 0)),
            Queen => (0..=1)
                .map(|y| WorldStep::new(1, y))
                .map(get_4_rotations_of)
                .flatten()
                .collect(),
            _ => vec![],
        }
    }
    pub fn capture_directions_for_type(piece_type: PieceType) -> StepList {
        match piece_type {
            _ => Self::move_directions_for_type(piece_type),
        }
    }
    fn piece_type(&self) -> PieceType {
        self.piece_type
    }

    fn faction(&self) -> Faction {
        self.faction
    }

    pub fn new(piece_type: PieceType, faction: Faction) -> Piece {
        Piece {
            piece_type,
            faction,
            faced_direction: if TURNING_PIECE_TYPES.contains(&piece_type) {
                Some(STEP_RIGHT)
            } else {
                None
            },
        }
    }

    pub(crate) fn from_type(piece_type: PieceType) -> Piece {
        Piece::new(piece_type, Faction::default())
    }

    pub(crate) fn relative_move_steps(&self) -> StepList {
        match self.piece_type {
            OmniDirectionalPawn | OmniDirectionalSoldier => ORTHOGONAL_STEPS.into(),
            King => KING_STEPS.into(),
            Knight => get_8_quadrants_of(WorldStep::new(1, 2)),
            TurningPawn | TurningSoldier => vec![self.faced_direction.unwrap()],
            _ => vec![],
        }
    }

    pub(crate) fn relative_capture_steps(&self) -> StepList {
        match self.piece_type {
            OmniDirectionalPawn => DIAGONAL_STEPS.into(),
            TurningPawn => adjacent_king_steps(self.faced_direction.unwrap())
                .into_iter()
                .collect(),
            _ => self.relative_move_steps(),
        }
    }

    pub(crate) fn move_directions(&self) -> StepList {
        Self::move_directions_for_type(self.piece_type)
    }

    pub(crate) fn capture_directions(&self) -> StepList {
        Self::capture_directions_for_type(self.piece_type)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    #[test]
    fn test_turning_vs_omnidirectional_pawn() {
        assert_eq!(
            HashSet::from_iter(Piece::from_type(OmniDirectionalPawn).relative_move_steps()),
            HashSet::from(ORTHOGONAL_STEPS)
        );
        assert_eq!(
            HashSet::from_iter(Piece::from_type(OmniDirectionalPawn).relative_capture_steps()),
            HashSet::from(DIAGONAL_STEPS)
        );

        assert_eq!(
            Piece::from_type(TurningPawn,).relative_move_steps().len(),
            1
        );
        assert_eq!(
            Piece::from_type(TurningPawn,)
                .relative_capture_steps()
                .len(),
            2
        );
    }
    #[test]
    fn test_turning_vs_omnidirectional_soldier() {
        assert_eq!(
            HashSet::from_iter(Piece::from_type(OmniDirectionalSoldier).relative_move_steps()),
            HashSet::from(ORTHOGONAL_STEPS)
        );
        assert_eq!(
            HashSet::from_iter(Piece::from_type(OmniDirectionalSoldier).relative_capture_steps()),
            HashSet::from(ORTHOGONAL_STEPS)
        );

        assert_eq!(
            Piece::from_type(TurningSoldier,)
                .relative_move_steps()
                .len(),
            1
        );
        assert_eq!(
            Piece::from_type(TurningSoldier,).relative_capture_steps(),
            Piece::from_type(TurningSoldier,).relative_move_steps()
        );
    }
}
