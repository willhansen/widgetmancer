use std::collections::HashSet;
use std::f32::consts::PI;

use derive_more::Constructor;
use euclid::*;
use getset::CopyGetters;
use rgb::RGB8;
use strum::IntoEnumIterator;
use strum_macros::Display;
use strum_macros::EnumIter;

use crate::glyph::DoubleGlyph;
use crate::glyph_constants::*;
use crate::piece::PieceType::*;
use crate::utility::*;
use crate::{get_8_octant_transforms_of, Glyph};

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

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Faction {
    Unaligned,
    RedPawn,
    DeathCube,
    Enemy(u32),
}

impl Default for Faction {
    fn default() -> Self {
        Faction::Enemy(0)
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
        let faction = Faction::Enemy(self.id_of_next_faction);
        self.id_of_next_faction += 1;
        faction
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone, Hash, Constructor, CopyGetters)]
#[get_copy = "pub"]
pub struct NStep {
    stepp: WorldStep,
    n: Option<u32>,
}

impl NStep {
    pub fn one(stepp: WorldStep) -> Self {
        NStep { stepp, n: Some(1) }
    }
    pub fn dir(step: WorldStep) -> Self {
        NStep {
            stepp: step,
            n: None,
        }
    }

    pub fn quadrant_symmetries(&self) -> Vec<Self> {
        self.stepp
            .quadrant_rotations_going_ccw()
            .into_iter()
            .map(|step| NStep::new(step, self.n))
            .collect()
    }
    pub fn octant_symmetries(&self) -> Vec<Self> {
        get_8_octant_transforms_of(self.stepp)
            .into_iter()
            .map(|step| NStep::new(step, self.n))
            .collect()
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone, Hash)]
pub struct Piece {
    pub piece_type: PieceType,
    pub faction: Faction,
    faced_direction: Option<KingWorldStep>,
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
    pub fn arrow(dir: KingWorldStep) -> Piece {
        let mut piece = Piece::from_type(Arrow);
        piece.set_faced_direction(dir);
        piece.faction = Faction::Unaligned;
        piece
    }

    pub fn faced_direction(&self) -> KingWorldStep {
        if let Some(dir) = self.faced_direction {
            dir
        } else {
            panic!("Piece isn't directional!");
        }
    }

    pub fn set_faced_direction(&mut self, dir: KingWorldStep) {
        assert!(self.can_turn());
        self.faced_direction = Some(dir);
    }

    pub fn can_turn(&self) -> bool {
        self.faced_direction.is_some()
    }

    pub fn turned_versions(&self) -> HashSet<Piece> {
        assert!(self.can_turn());

        [-1, 1]
            .into_iter()
            .map(|i| self.quarter_rotated_ccw(i))
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
                Some(STEP_RIGHT.into())
            } else {
                None
            },
        }
    }

    pub(crate) fn from_type(piece_type: PieceType) -> Piece {
        Piece::new(piece_type, Faction::default())
    }

    pub(crate) fn relative_moves(&self) -> NStepList {
        match self.piece_type {
            OmniDirectionalPawn | OmniDirectionalSoldier => ORTHOGONAL_STEPS.map(NStep::one).into(),
            King => KING_STEPS.map(NStep::one).into(),
            Knight => NStep::one(WorldStep::new(1, 2)).octant_symmetries(),
            TurningPawn | TurningSoldier => vec![NStep::one(self.faced_direction.unwrap().into())],
            Bishop => DIAGONAL_STEPS.map(NStep::dir).into(),
            Rook => ORTHOGONAL_STEPS.map(NStep::dir).into(),
            Queen => KING_STEPS.map(NStep::dir).into(),
            _ => vec![],
        }
    }

    pub(crate) fn relative_captures(&self) -> NStepList {
        match self.piece_type {
            OmniDirectionalPawn => DIAGONAL_STEPS.map(NStep::one).into(),
            TurningPawn => adjacent_king_steps(self.faced_direction.unwrap().into())
                .into_iter()
                .map(NStep::one)
                .collect(),
            _ => self.relative_moves(),
        }
    }
}

impl QuarterTurnRotatable for Piece {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Piece {
        assert!(self.can_turn());
        Piece {
            piece_type: self.piece_type,
            faction: self.faction,
            faced_direction: Some(
                self.faced_direction()
                    .quarter_rotated_ccw(quarter_turns_ccw),
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use ntest::timeout;
    use std::collections::HashSet;

    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    #[test]

    fn test_turning_vs_omnidirectional_pawn() {
        assert_eq!(
            HashSet::from_iter(Piece::from_type(OmniDirectionalPawn).relative_moves()),
            HashSet::from(ORTHOGONAL_STEPS.map(NStep::one))
        );
        assert_eq!(
            HashSet::from_iter(Piece::from_type(OmniDirectionalPawn).relative_captures()),
            HashSet::from(DIAGONAL_STEPS.map(NStep::one))
        );

        assert_eq!(Piece::from_type(TurningPawn).relative_moves().len(), 1);
        assert_eq!(Piece::from_type(TurningPawn).relative_captures().len(), 2);
    }

    #[test]

    fn test_turning_vs_omnidirectional_soldier() {
        assert_eq!(
            HashSet::from_iter(Piece::from_type(OmniDirectionalSoldier).relative_moves()),
            HashSet::from(ORTHOGONAL_STEPS.map(NStep::one))
        );
        assert_eq!(
            HashSet::from_iter(Piece::from_type(OmniDirectionalSoldier).relative_captures()),
            HashSet::from(ORTHOGONAL_STEPS.map(NStep::one))
        );

        assert_eq!(Piece::from_type(TurningSoldier).relative_moves().len(), 1);
        assert_eq!(
            Piece::from_type(TurningSoldier).relative_captures(),
            Piece::from_type(TurningSoldier).relative_moves()
        );
    }
}
