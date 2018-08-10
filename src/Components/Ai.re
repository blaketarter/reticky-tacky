let result_win = 100;
let result_prevent_win = 90;
let result_two_sequential = 50;
let result_two_non_sequential = 40;
let result_one = 1;
let result_none = 0;
let placement_center = 3;
let placement_corner = 2;
let placement_default = 1;
let placement_none = 0;

type direction =
  | Horizontal(Board.row)
  | Vertical(Board.row)
  | Diagonal(Board.row);

let score_of_horizontal =
    (~coord: Coord.t, ~token: Token.t, row: Board.row)
    : int => {
  let (column, _) = coord;

  switch (column, token, row) {
  | (None, _, _)
  | (_, Empty, _)
  | (C1, Circle(_) | Box(_), (Circle(_) | Box(_), _, _)) => result_none
  | (C2, Circle(_) | Box(_), (_, Circle(_) | Box(_), _)) => result_none
  | (C3, Circle(_) | Box(_), (_, _, Circle(_) | Box(_))) => result_none
  | (C1 | C2 | C3, Circle(_) | Box(_), (Empty, Empty, Empty)) => result_one
  | (C1, Circle(_), (Empty, Circle(_), Circle(_)))
  | (C1, Box(_), (Empty, Box(_), Box(_))) => result_win
  | (C1, Circle(_), (Empty, Empty | Box(_), Circle(_)))
  | (C1, Box(_), (Empty, Empty | Circle(_), Box(_))) => result_two_non_sequential
  | (C1, Circle(_), (Empty, Circle(_), Empty | Box(_)))
  | (C1, Box(_), (Empty, Box(_), Empty | Circle(_))) => result_two_sequential
  | (C1, Circle(_), (Empty, Empty | Box(_), Empty | Box(_)))
  | (C1, Box(_), (Empty, Empty | Circle(_), Empty | Circle(_))) => result_one
  | (C2, Circle(_), (Circle(_), Empty, Circle(_)))
  | (C2, Box(_), (Box(_), Empty, Box(_))) => result_win
  | (C2, Circle(_), (Empty | Box(_), Empty, Circle(_)))
  | (C2, Box(_), (Empty | Circle(_), Empty, Box(_))) => result_two_non_sequential
  | (C2, Circle(_), (Circle(_), Empty, Empty | Box(_)))
  | (C2, Box(_), (Box(_), Empty, Empty | Circle(_))) => result_two_sequential
  | (C2, Circle(_), (Empty | Box(_), Empty, Empty | Box(_)))
  | (C2, Box(_), (Empty | Circle(_), Empty, Empty | Circle(_))) => result_one
  | (C3, Circle(_), (Circle(_), Circle(_), Empty))
  | (C3, Box(_), (Box(_), Box(_), Empty)) => result_win
  | (C3, Circle(_), (Empty | Box(_), Circle(_), Empty))
  | (C3, Box(_), (Empty | Circle(_), Box(_), Empty)) => result_two_non_sequential
  | (C3, Circle(_), (Circle(_), Empty | Box(_), Empty))
  | (C3, Box(_), (Box(_), Empty | Circle(_), Empty)) => result_two_sequential
  | (C3, Circle(_), (Empty | Box(_), Empty | Box(_), Empty))
  | (C3, Box(_), (Empty | Circle(_), Empty | Circle(_), Empty)) => result_one
  };
};

let score_of_vertical =
    (~coord: Coord.t, ~token: Token.t, row: Board.row)
    : int => {
  let (_, r) = coord;

  switch (r, token, row) {
  | (None, _, _)
  | (_, Empty, _)
  | (R1, Circle(_) | Box(_), (Circle(_) | Box(_), _, _)) => result_none
  | (R2, Circle(_) | Box(_), (_, Circle(_) | Box(_), _)) => result_none
  | (R3, Circle(_) | Box(_), (_, _, Circle(_) | Box(_))) => result_none
  | (R1 | R2 | R3, Circle(_) | Box(_), (Empty, Empty, Empty)) => result_one
  | (R1, Circle(_), (Empty, Circle(_), Circle(_)))
  | (R1, Box(_), (Empty, Box(_), Box(_))) => result_win
  | (R1, Circle(_), (Empty, Empty | Box(_), Circle(_)))
  | (R1, Box(_), (Empty, Empty | Circle(_), Box(_))) => result_two_non_sequential
  | (R1, Circle(_), (Empty, Circle(_), Empty | Box(_)))
  | (R1, Box(_), (Empty, Box(_), Empty | Circle(_))) => result_two_sequential
  | (R1, Circle(_), (Empty, Empty | Box(_), Empty | Box(_)))
  | (R1, Box(_), (Empty, Empty | Circle(_), Empty | Circle(_))) => result_one
  | (R2, Circle(_), (Circle(_), Empty, Circle(_)))
  | (R2, Box(_), (Box(_), Empty, Box(_))) => result_win
  | (R2, Circle(_), (Empty | Box(_), Empty, Circle(_)))
  | (R2, Box(_), (Empty | Circle(_), Empty, Box(_))) => result_two_non_sequential
  | (R2, Circle(_), (Circle(_), Empty, Empty | Box(_)))
  | (R2, Box(_), (Box(_), Empty, Empty | Circle(_))) => result_two_sequential
  | (R2, Circle(_), (Empty | Box(_), Empty, Empty | Box(_)))
  | (R2, Box(_), (Empty | Circle(_), Empty, Empty | Circle(_))) => result_one
  | (R3, Circle(_), (Circle(_), Circle(_), Empty))
  | (R3, Box(_), (Box(_), Box(_), Empty)) => result_win
  | (R3, Circle(_), (Empty | Box(_), Circle(_), Empty))
  | (R3, Box(_), (Empty | Circle(_), Box(_), Empty)) => result_two_non_sequential
  | (R3, Circle(_), (Circle(_), Empty | Box(_), Empty))
  | (R3, Box(_), (Box(_), Empty | Circle(_), Empty)) => result_two_sequential
  | (R3, Circle(_), (Empty | Box(_), Empty | Box(_), Empty))
  | (R3, Box(_), (Empty | Circle(_), Empty | Circle(_), Empty)) => result_one
  };
};

let score_of_diagonal =
    (~coord: Coord.t, ~token: Token.t, row: Board.row)
    : int => {
  let (column, _) = coord;

  switch (column, token, row) {
  | (None, _, _)
  | (_, Empty, _)
  | (C1, Circle(_) | Box(_), (Circle(_) | Box(_), _, _)) => result_none
  | (C2, Circle(_) | Box(_), (_, Circle(_) | Box(_), _)) => result_none
  | (C3, Circle(_) | Box(_), (_, _, Circle(_) | Box(_))) => result_none
  | (C1 | C2 | C3, Circle(_) | Box(_), (Empty, Empty, Empty)) => result_one
  | (C1, Circle(_), (Empty, Circle(_), Circle(_)))
  | (C1, Box(_), (Empty, Box(_), Box(_))) => result_win
  | (C1, Circle(_), (Empty, Empty | Box(_), Circle(_)))
  | (C1, Box(_), (Empty, Empty | Circle(_), Box(_))) => result_two_non_sequential
  | (C1, Circle(_), (Empty, Circle(_), Empty | Box(_)))
  | (C1, Box(_), (Empty, Box(_), Empty | Circle(_))) => result_two_sequential
  | (C1, Circle(_), (Empty, Empty | Box(_), Empty | Box(_)))
  | (C1, Box(_), (Empty, Empty | Circle(_), Empty | Circle(_))) => result_one
  | (C2, Circle(_), (Circle(_), Empty, Circle(_)))
  | (C2, Box(_), (Box(_), Empty, Box(_))) => result_win
  | (C2, Circle(_), (Empty | Box(_), Empty, Circle(_)))
  | (C2, Box(_), (Empty | Circle(_), Empty, Box(_))) => result_two_non_sequential
  | (C2, Circle(_), (Circle(_), Empty, Empty | Box(_)))
  | (C2, Box(_), (Box(_), Empty, Empty | Circle(_))) => result_two_sequential
  | (C2, Circle(_), (Empty | Box(_), Empty, Empty | Box(_)))
  | (C2, Box(_), (Empty | Circle(_), Empty, Empty | Circle(_))) => result_one
  | (C3, Circle(_), (Circle(_), Circle(_), Empty))
  | (C3, Box(_), (Box(_), Box(_), Empty)) => result_win
  | (C3, Circle(_), (Empty | Box(_), Circle(_), Empty))
  | (C3, Box(_), (Empty | Circle(_), Box(_), Empty)) => result_two_non_sequential
  | (C3, Circle(_), (Circle(_), Empty | Box(_), Empty))
  | (C3, Box(_), (Box(_), Empty | Circle(_), Empty)) => result_two_sequential
  | (C3, Circle(_), (Empty | Box(_), Empty | Box(_), Empty))
  | (C3, Box(_), (Empty | Circle(_), Empty | Circle(_), Empty)) => result_one
  };
};

let score_of_row = (~coord: Coord.t, ~token: Token.t, row: direction) : int => {
  let score_result =
    switch (row) {
    | Horizontal(r) => score_of_horizontal(~coord, ~token, r)
    | Vertical(r) => score_of_vertical(~coord, ~token, r)
    | Diagonal(r) => score_of_diagonal(~coord, ~token, r)
    };

  if (score_result > 0) {
    let score_placement =
      switch (coord) {
      | (None, _)
      | (_, None) => placement_none
      | (C2, R2) => placement_center
      | (C1, R1 | R3)
      | (C3, R1 | R3) => placement_corner
      | (C1 | C3, R2)
      | (C2, R1 | R3) => placement_default
      };

    score_result + score_placement;
  } else {
    score_result;
  };
};

let score_of_coord = (~board: Board.t, ~coord: Coord.t, token: Token.t) : int =>
  switch (board) {
  | Board((x1, x2, x3), (y1, y2, y3), (z1, z2, z3)) =>
    let row1 = Horizontal((x1, x2, x3));
    let row2 = Horizontal((y1, y2, y3));
    let row3 = Horizontal((z1, z2, z3));
    let column1 = Vertical((x1, y1, z1));
    let column2 = Vertical((x2, y2, z2));
    let column3 = Vertical((x3, y3, z3));
    let diagonal1 = Diagonal((x1, y2, z3));
    let diagonal2 = Diagonal((x3, y2, z1));

    let rows =
      switch (coord) {
      | (None, _)
      | (_, None) => []
      | (C1, R1) => [column1, row1, diagonal1]
      | (C1, R2) => [column1, row2]
      | (C1, R3) => [column1, row3, diagonal2]
      | (C2, R1) => [column2, row1]
      | (C2, R2) => [column2, row2, diagonal1, diagonal2]
      | (C2, R3) => [column2, row3]
      | (C3, R1) => [column3, row1, diagonal2]
      | (C3, R2) => [column3, row2]
      | (C3, R3) => [column3, row3, diagonal2]
      };

    List.fold_left(
      (highest_score, current_row) => {
        let score = score_of_row(~coord, ~token, current_row);
        if (score > highest_score) {
          score;
        } else {
          highest_score;
        };
      },
      0,
      rows,
    );
  };
