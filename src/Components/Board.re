type token =
  | Empty
  | X
  | O;

type row = (token, token, token);

type t =
  | Board(row, row, row);

let empty =
  Board(
    (Empty, Empty, Empty),
    (Empty, Empty, Empty),
    (Empty, Empty, Empty),
  );

let getNthRow = (n: int, b: t) : row =>
  switch (n, b) {
  | (0, Board(r, _, _)) => r
  | (1, Board(_, r, _)) => r
  | (2, Board(_, _, r)) => r
  | (_, Board(_, _, r)) => r
  };

let getNthItem = (n: int, r: row) : token =>
  switch (n, r) {
  | (0, (m, _, _)) => m
  | (1, (_, m, _)) => m
  | (2, (_, m, _)) => m
  | (_, (_, _, _)) => Empty
  };

let isBoardWon = (b: t) : bool =>
  switch (b) {
  | Board((X, X, X), (_, _, _), (_, _, _))
  | Board((O, O, O), (_, _, _), (_, _, _))
  | Board((_, _, _), (X, X, X), (_, _, _))
  | Board((_, _, _), (O, O, O), (_, _, _))
  | Board((_, _, _), (_, _, _), (X, X, X))
  | Board((_, _, _), (_, _, _), (O, O, O))
  | Board((X, _, _), (X, _, _), (X, _, _))
  | Board((O, _, _), (O, _, _), (O, _, _))
  | Board((_, X, _), (_, X, _), (_, X, _))
  | Board((_, O, _), (_, O, _), (_, O, _))
  | Board((_, _, X), (_, _, X), (_, _, X))
  | Board((_, _, O), (_, _, O), (_, _, O))
  | Board((X, _, _), (_, X, _), (_, _, X))
  | Board((O, _, _), (_, O, _), (_, _, O))
  | Board((_, _, X), (_, X, _), (X, _, _))
  | Board((_, _, O), (_, O, _), (O, _, _)) => true
  | Board((_, _, _), (_, _, _), (_, _, _)) => false
  };

let isBoardFull = (b: t) : bool =>
  switch (b) {
  | Board(
      (X | O, X | O, X | O),
      (X | O, X | O, X | O),
      (X | O, X | O, X | O),
    ) =>
    true
  | Board((_, _, _), (_, _, _), (_, _, _)) => false
  };

let getTokenCoords = (~yPos: int, ~xPos: int) => (
  xPos
  * (Options.cellSize + Options.spacing)
  + Options.spacing
  + Options.tokenInset,
  yPos
  * (Options.cellSize + Options.spacing)
  + Options.spacing
  + Options.tokenInset,
);

let drawToken = (~yPos: int, ~xPos: int, t: token, env) => {
  let (x, y) = getTokenCoords(~xPos, ~yPos);
  switch (t) {
  | Empty => ()
  | X =>
    Circle.draw(
      ~color=Circle.Blue,
      ~xPos=x,
      ~yPos=y,
      ~size=Options.tokenSize,
      env,
    )
  | O =>
    Box.draw(
      ~color=Box.Orange,
      ~xPos=x,
      ~yPos=y,
      ~size=Options.tokenSize,
      env,
    )
  };
};

let drawRow = (~yPos, r: row, env) =>
  switch (r) {
  | (x, y, z) =>
    drawToken(~yPos, ~xPos=0, x, env);
    drawToken(~yPos, ~xPos=1, y, env);
    drawToken(~yPos, ~xPos=2, z, env);
  };

let insetTokenAtColumn = (~column: Coord.c, ~row: row, token: token) : row =>
  switch (column, row) {
  | (C1, (_, x, y)) => (token, x, y)
  | (C2, (x, _, y)) => (x, token, y)
  | (C3, (x, y, _)) => (x, y, token)
  | (None, r) => r
  };

let insertTokenAtCoord = (~coord: Coord.t, ~board: t, token: token) : t =>
  switch (coord, board) {
  | ((c, R1), Board(r, x, y)) =>
    Board(insetTokenAtColumn(~column=c, ~row=r, token), x, y)
  | ((c, R2), Board(x, r, y)) =>
    Board(x, insetTokenAtColumn(~column=c, ~row=r, token), y)
  | ((c, R3), Board(x, y, r)) =>
    Board(x, y, insetTokenAtColumn(~column=c, ~row=r, token))
  | ((_, None), b) => b
  };

let draw = (b: t, env) =>
  switch (b) {
  | Board(x, y, z) =>
    drawRow(~yPos=0, x, env);
    drawRow(~yPos=1, y, env);
    drawRow(~yPos=2, z, env);
  };