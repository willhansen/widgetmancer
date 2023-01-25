use crate::piece::simple_piece::SimplePiece;
use crate::piece::*;
use crate::utility::coordinate_frame_conversions::WorldStep;
use crate::utility::STEP_RIGHT;

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct TurningPiece {
    pub simple_piece: SimplePiece,
    pub faced_direction: WorldStep,
}

impl Piece for TurningPiece {
    fn piece_type(&self) -> PieceType {
        self.simple_piece.piece_type
    }

    fn faction(&self) -> Faction {
        self.simple_piece.faction
    }

    fn new(piece_type: PieceType, faction: Faction) -> Self {
        TurningPiece {
            simple_piece: SimplePiece::new(piece_type, faction),
            faced_direction: STEP_RIGHT,
        }
    }

    fn from_type(piece_type: PieceType) -> Self {
        Self::new(piece_type, Faction::default())
    }
}
