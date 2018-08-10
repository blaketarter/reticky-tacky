let player = Player.Box;
let token = Token.Box(Token.initialValue);
let waitTime = 60;

type result =
  | Win
  | PreventWin
  | None;
type count =
  | Three
  | TwoSequential
  | TwoNonSequential
  | One
  | None;
type placement =
  | Center
  | Corner
  | Edge
  | None;

let score_of_result = (r: result) =>
  switch (r) {
  | Win => 200
  | PreventWin => 100
  | None => 0
  };

let score_of_count = (c: count) =>
  switch (c) {
  | Three => 40
  | TwoSequential => 30
  | TwoNonSequential => 20
  | One => 10
  | None => 0
  };

let score_of_placement = (p: placement) =>
  switch (p) {
  | Center => 3
  | Corner => 2
  | Edge => 1
  | None => 0
  };

type index =
  | One
  | Two
  | Three;

type move =
  | Move(index, Token.t, Board.row);

let score_of_move = (~coord: Coord.t, move: move) : int => {
  let move_result: result =
    switch (move) {
    | Move(_, Empty, (_, _, _)) => None
    | Move(_, _, (_, Empty, Empty)) => None
    | Move(_, _, (Empty, _, Empty)) => None
    | Move(_, _, (Empty, Empty, _)) => None
    | Move(One, Circle(_) | Box(_), (Circle(_) | Box(_), _, _)) => None
    | Move(Two, Circle(_) | Box(_), (_, Circle(_) | Box(_), _)) => None
    | Move(Three, Circle(_) | Box(_), (_, _, Circle(_) | Box(_))) => None
    | Move(One, Circle(_), (Empty, Circle(_), Circle(_))) => Win
    | Move(Two, Circle(_), (Circle(_), Empty, Circle(_))) => Win
    | Move(Three, Circle(_), (Circle(_), Circle(_), Empty)) => Win
    | Move(One, Box(_), (Empty, Box(_), Box(_))) => Win
    | Move(Two, Box(_), (Box(_), Empty, Box(_))) => Win
    | Move(Three, Box(_), (Box(_), Box(_), Empty)) => Win
    | Move(One, Box(_), (Empty, Circle(_), Circle(_))) => PreventWin
    | Move(Two, Box(_), (Circle(_), Empty, Circle(_))) => PreventWin
    | Move(Three, Box(_), (Circle(_), Circle(_), Empty)) => PreventWin
    | Move(One, Circle(_), (Empty, Box(_), Box(_))) => PreventWin
    | Move(Two, Circle(_), (Box(_), Empty, Box(_))) => PreventWin
    | Move(Three, Circle(_), (Box(_), Box(_), Empty)) => PreventWin
    | Move(One | Two | Three, Circle(_) | Box(_), (_, _, _)) => None
    };

  let score_result = score_of_result(move_result);

  let move_count: count =
    switch (move) {
    | Move(_, Empty, _) => None
    | Move(One, Circle(_) | Box(_), (Circle(_) | Box(_), _, _)) => None
    | Move(Two, Circle(_) | Box(_), (_, Circle(_) | Box(_), _)) => None
    | Move(Three, Circle(_) | Box(_), (_, _, Circle(_) | Box(_))) => None
    | Move(One, Circle(_), (Empty, Circle(_), Circle(_))) => Three
    | Move(Two, Circle(_), (Circle(_), Empty, Circle(_))) => Three
    | Move(Three, Circle(_), (Circle(_), Circle(_), Empty)) => Three
    | Move(One, Box(_), (Empty, Box(_), Box(_))) => Three
    | Move(Two, Box(_), (Box(_), Empty, Box(_))) => Three
    | Move(Three, Box(_), (Box(_), Box(_), Empty)) => Three
    | Move(One, Circle(_), (Empty, Empty | Box(_), Circle(_))) =>
      TwoNonSequential
    | Move(Three, Circle(_), (Circle(_), Empty | Box(_), Empty)) =>
      TwoNonSequential
    | Move(One, Box(_), (Empty, Empty | Circle(_), Box(_))) =>
      TwoNonSequential
    | Move(Three, Box(_), (Box(_), Empty | Circle(_), Empty)) =>
      TwoNonSequential
    | Move(One, Circle(_), (Empty, Circle(_), Box(_) | Empty)) =>
      TwoSequential
    | Move(Two, Circle(_), (Circle(_), Empty, Box(_) | Empty)) =>
      TwoSequential
    | Move(Two, Circle(_), (Box(_) | Empty, Empty, Circle(_))) =>
      TwoSequential
    | Move(Three, Circle(_), (Box(_) | Empty, Circle(_), Empty)) =>
      TwoSequential
    | Move(One, Box(_), (Empty, Box(_), Circle(_) | Empty)) => TwoSequential
    | Move(Two, Box(_), (Box(_), Empty, Circle(_) | Empty)) => TwoSequential
    | Move(Two, Box(_), (Circle(_) | Empty, Empty, Box(_))) => TwoSequential
    | Move(Three, Box(_), (Circle(_) | Empty, Box(_), Empty)) =>
      TwoSequential
    | Move(One, Circle(_), (Empty, Empty | Box(_), Empty | Box(_))) => One
    | Move(Two, Circle(_), (Empty | Box(_), Empty, Empty | Box(_))) => One
    | Move(Three, Circle(_), (Empty | Box(_), Empty | Box(_), Empty)) => One
    | Move(One, Box(_), (Empty, Empty | Circle(_), Empty | Circle(_))) => One
    | Move(Two, Box(_), (Empty | Circle(_), Empty, Empty | Circle(_))) => One
    | Move(Three, Box(_), (Empty | Circle(_), Empty | Circle(_), Empty)) =>
      One
    };

  let score_count = score_of_count(move_count);

  let move_placement: placement =
    switch (coord) {
    | (None, _)
    | (_, None) => None
    | (C2, R2) => Center
    | (C1, R1 | R3)
    | (C3, R1 | R3) => Corner
    | (C1 | C3, R2)
    | (C2, R1 | R3) => Edge
    };

  let score_placement = score_of_placement(move_placement);

  let score_result_placement = score_result + score_count;

  score_result_placement > 0 ?
    score_result_placement + score_placement : score_result_placement;
};

let score_of_coord = (~board: Board.t, ~coord: Coord.t, token: Token.t) : int =>
  switch (board) {
  | Board((x1, x2, x3), (y1, y2, y3), (z1, z2, z3)) =>
    let row1 = (x1, x2, x3);
    let row2 = (y1, y2, y3);
    let row3 = (z1, z2, z3);
    let column1 = (x1, y1, z1);
    let column2 = (x2, y2, z2);
    let column3 = (x3, y3, z3);
    let diagonal1 = (x1, y2, z3);
    let diagonal2 = (z1, y2, x3);

    let moves =
      switch (coord) {
      | (None, _)
      | (_, None) => []
      | (C1, R1) => [(column1, One), (row1, One), (diagonal1, One)]
      | (C1, R2) => [(column1, Two), (row2, One)]
      | (C1, R3) => [(column1, Three), (row3, One), (diagonal2, One)]
      | (C2, R1) => [(column2, One), (row1, Two)]
      | (C2, R2) => [
          (column2, Two),
          (row2, Two),
          (diagonal1, Two),
          (diagonal2, Two),
        ]
      | (C2, R3) => [(column2, Three), (row3, Two)]
      | (C3, R1) => [(column3, One), (row1, Three), (diagonal2, Three)]
      | (C3, R2) => [(column3, Two), (row2, Three)]
      | (C3, R3) => [(column3, Three), (row3, Three), (diagonal1, Three)]
      };

    List.fold_left(
      (highest_score, current_move) => {
        let (row, index) = current_move;
        let score = score_of_move(~coord, Move(index, token, row));
        if (score > highest_score) {
          score;
        } else {
          highest_score;
        };
      },
      0,
      moves,
    );
  };

let move = (state: State.t, score_coord) : State.t =>
  if (Options.aiAutoPlay === false) {
    state;
  } else if (state.aiCountDown > 0) {
    {...state, aiCountDown: state.aiCountDown - 1};
  } else {
    let (score, coord) = score_coord;
    {
      ...state,
      board:
        score > 0 ?
          Board.insertTokenAtCoord(~coord, ~board=state.board, token) :
          state.board,
      aiCountDown: waitTime,
    }
    |> Move.next;
  };

let turn = (state: State.t, env) : State.t =>
  if (state.player === player) {
    let score_coord =
      Grid.reduce(
        (sc, (x, y)) => {
          let coord = Coord.coord_of_xy(~x, ~y);
          let player_token = state.player |> Player.token_of_player;
          let score =
            score_of_coord(~board=state.board, ~coord, player_token);
          let (current_high_score, _) = sc;

          if (Options.debugAi) {
            let (x, y) = Token.getCoords(~xPos=x, ~yPos=y);
            Helpers.centerText(
              ~body=score |> string_of_int,
              ~pos=(x + Options.tokenInset, y + Options.tokenInset),
              ~font=None,
              env,
            );
          };
          if (score > current_high_score) {
            (score, coord);
          } else {
            sc;
          };
        },
        (0, Coord.empty),
      );

    score_coord |> move(state);
  } else {
    state;
  };