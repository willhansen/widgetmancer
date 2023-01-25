use crate::glyph::glyph_constants::ENEMY_PIECE_COLOR;
use crate::glyph::{DoubleGlyph, Glyph};
use crate::piece::PieceType::{Bishop, DeathCubeTurret, King, Knight, Pawn, Queen, Rook, Soldier};
use crate::piece::{Faction, Movable, Piece, PieceType};
use crate::utility::coordinate_frame_conversions::{StepList, WorldStep};
use crate::utility::{
    get_4_rotations_of, get_8_quadrants_of, get_new_rng, random_choice, DIAGONAL_STEPS, KING_STEPS,
    ORTHOGONAL_STEPS,
};

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct SimplePiece {
    pub piece_type: PieceType,
    pub faction: Faction,
}

impl SimplePiece {
    pub fn pawn() -> SimplePiece {
        SimplePiece::from_type(Pawn)
    }
    pub fn knight() -> SimplePiece {
        SimplePiece::from_type(Knight)
    }
    pub fn bishop() -> SimplePiece {
        SimplePiece::from_type(Bishop)
    }
    pub fn rook() -> SimplePiece {
        SimplePiece::from_type(Rook)
    }
    pub fn queen() -> SimplePiece {
        SimplePiece::from_type(Queen)
    }
    pub fn king() -> SimplePiece {
        SimplePiece::from_type(King)
    }

    pub fn random_subordinate_type() -> PieceType {
        let mut rng = get_new_rng();
        let options = vec![Pawn, Knight, Bishop, Rook, Queen];
        *random_choice(&mut rng, &options)
    }

    pub fn glyphs(&self) -> DoubleGlyph {
        SimplePiece::glyphs_for_type(self.piece_type)
    }

    pub fn glyphs_for_type(piece_type: PieceType) -> DoubleGlyph {
        SimplePiece::chars_for_type(piece_type)
            .map(|character| Glyph::fg_only(character, ENEMY_PIECE_COLOR))
    }

    pub fn chars_for_type(piece_type: PieceType) -> [char; 2] {
        match piece_type {
            Pawn => ['♟', ' '],
            Soldier => ['S', 'o'],
            Knight => ['♞', ' '],
            Bishop => ['♝', ' '],
            Rook => ['♜', ' '],
            Queen => ['♛', ' '],
            King => ['♚', ' '],
            DeathCubeTurret => ['𐄳', ' '],
            _ => panic!("Tried to draw unknown piece type: {:?}", piece_type),
        }
    }
    pub fn relative_move_steps_for_type(piece_type: PieceType) -> StepList {
        match piece_type {
            Pawn | Soldier => ORTHOGONAL_STEPS.into(),
            King => KING_STEPS.into(),
            Knight => get_8_quadrants_of(WorldStep::new(1, 2)),
            _ => vec![],
        }
    }

    pub fn relative_capture_steps_for_type(piece_type: PieceType) -> StepList {
        match piece_type {
            Pawn => DIAGONAL_STEPS.into(),
            _ => Self::relative_move_steps_for_type(piece_type),
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
}

impl Piece for SimplePiece {
    fn piece_type(&self) -> PieceType {
        self.piece_type
    }

    fn faction(&self) -> Faction {
        self.faction
    }

    fn new(piece_type: PieceType, faction: Faction) -> SimplePiece {
        SimplePiece {
            piece_type,
            faction,
        }
    }

    fn from_type(piece_type: PieceType) -> SimplePiece {
        SimplePiece::new(piece_type, Faction::default())
    }
}

impl Movable for SimplePiece {
    fn relative_move_steps(&self) -> StepList {
        Self::relative_move_steps_for_type(self.piece_type)
    }

    fn relative_capture_steps(&self) -> StepList {
        Self::relative_capture_steps_for_type(self.piece_type)
    }

    fn move_directions(&self) -> StepList {
        Self::move_directions_for_type(self.piece_type)
    }

    fn capture_directions(&self) -> StepList {
        Self::capture_directions_for_type(self.piece_type)
    }
}
