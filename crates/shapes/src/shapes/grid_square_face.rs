use poses::*;
use coordinate::*;

pub struct GridSquareFace(SquareWithOrthogonalDirection<ICoord>);

impl GridSquareFace {
    pub fn midpoint(&self) -> FCoord {
        self.square.to_f32() + self.direction().to_step::<WorldMove>() * 0.5
    }

    pub fn face_is_on_same_line<OtherType: Into<Self>>(&self, other: OtherType) -> bool {
        let other_face: Self = other.into();
        let directions_are_parallel = self.dir.is_parallel(other_face.dir);
        if !directions_are_parallel {
            return false;
        }

        let pos_on_dir_axis = self.square().position_on_orthogonal_axis(self.dir());
        let stepped_pos_on_dir_axis = self
            .stepped()
            .square()
            .position_on_orthogonal_axis(self.dir());
        let other_pos_on_dir_axis = other_face.square().position_on_orthogonal_axis(self.dir());

        let same_direction = self.dir() == other_face.dir();
        if same_direction {
            other_pos_on_dir_axis == pos_on_dir_axis
        } else {
            other_pos_on_dir_axis == stepped_pos_on_dir_axis
        }
    }
    pub fn faces_overlap<OtherType: Into<Self> + std::marker::Copy>(
        &self,
        other_face: OtherType,
    ) -> bool {
        *self == other_face.into() || *self == other_face.into().stepped().turned_back()
    }
    pub fn face_line_segment(&self) -> impl line_segment::Operations<SquareType::Floating> {
        line::square_face_as_line(self.square, self.dir)
    }

    pub fn face_crosses_positive_x_axis(&self) -> bool {
        if self.square == SquareType::zero() {
            return self.direction() == OrthogonalDirection::RIGHT;
        }

        self.square.x() > SquareType::DataType::zero()
            && self.square.y() == 0
            && self.direction().is_horizontal()
    }

    pub fn center_point_of_face(&self) -> <SquareType as coordinate::Operations>::Floating {
        self.square().to_f32()
            + self
                .dir()
                .to_step::<<SquareType as coordinate::Operations>::Floating>()
                * 0.5
    }
    pub fn end_points_of_face(&self) -> [<SquareType as coordinate::Operations>::Floating; 2] {
        [self.left(), self.right()].map(|dir| {
            self.center_point_of_face()
                + dir.to_step::<<SquareType as coordinate::Operations>::Floating>() * 0.5
        })
    }
    pub fn end_points_of_face_in_ccw_order(&self) -> [<SquareType as coordinate::Operations>::Floating; 2] {
        let mut ps = self.end_points_of_face();
        if !two_points_are_ccw_with_origin(ps[0], ps[1]) {
            ps.reverse();
        }
        ps
    }
    pub fn cw_end_of_face(&self) -> <SquareType as coordinate::Operations>::Floating {
        self.end_points_of_face_in_ccw_order()[0]
    }
    pub fn ccw_end_of_face(&self) -> <SquareType as coordinate::Operations>::Floating {
        self.end_points_of_face_in_ccw_order()[1]
    }
    pub fn face_end_point_approx_touches_point(
        &self,
        point: <SquareType as coordinate::Operations>::Floating,
    ) -> bool {
        let tolerance = 1e-6;
        self.end_points_of_face()
            .into_iter()
            .any(|end_point| end_point.about_eq(point, tolerance))
    }
    pub fn face_flipped_to_face_origin(&self) -> Self {
        if self.square().dot(self.direction()) < 0 {
            self.stepped().turned_back()
        } else {
            *self
        }
    }
    pub fn both_sides_of_face(&self) -> [Self; 2] {
        [*self, self.stepped().turned_back()]
    }
}
