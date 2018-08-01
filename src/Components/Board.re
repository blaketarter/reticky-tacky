type filledToken = [ | `Circle | `Box];
type token = [ | `Empty | filledToken];

type row = (token, token, token);

type t =
  | Board(row, row, row);

let empty =
  Board(
    (`Empty, `Empty, `Empty),
    (`Empty, `Empty, `Empty),
    (`Empty, `Empty, `Empty),
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
  | (_, (_, _, _)) => `Empty
  };

let isBoardWon = (b: t) : bool =>
  switch (b) {
  | Board((`Circle, `Circle, `Circle), (_, _, _), (_, _, _))
  | Board((`Box, `Box, `Box), (_, _, _), (_, _, _))
  | Board((_, _, _), (`Circle, `Circle, `Circle), (_, _, _))
  | Board((_, _, _), (`Box, `Box, `Box), (_, _, _))
  | Board((_, _, _), (_, _, _), (`Circle, `Circle, `Circle))
  | Board((_, _, _), (_, _, _), (`Box, `Box, `Box))
  | Board((`Circle, _, _), (`Circle, _, _), (`Circle, _, _))
  | Board((`Box, _, _), (`Box, _, _), (`Box, _, _))
  | Board((_, `Circle, _), (_, `Circle, _), (_, `Circle, _))
  | Board((_, `Box, _), (_, `Box, _), (_, `Box, _))
  | Board((_, _, `Circle), (_, _, `Circle), (_, _, `Circle))
  | Board((_, _, `Box), (_, _, `Box), (_, _, `Box))
  | Board((`Circle, _, _), (_, `Circle, _), (_, _, `Circle))
  | Board((`Box, _, _), (_, `Box, _), (_, _, `Box))
  | Board((_, _, `Circle), (_, `Circle, _), (`Circle, _, _))
  | Board((_, _, `Box), (_, `Box, _), (`Box, _, _)) => true
  | Board((_, _, _), (_, _, _), (_, _, _)) => false
  };

let isBoardFull = (b: t) : bool =>
  switch (b) {
  | Board(
      (`Circle | `Box, `Circle | `Box, `Circle | `Box),
      (`Circle | `Box, `Circle | `Box, `Circle | `Box),
      (`Circle | `Box, `Circle | `Box, `Circle | `Box),
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
  | `Empty => ()
  | `Circle =>
    Circle.draw(
      ~color=Circle.Blue,
      ~xPos=x,
      ~yPos=y,
      ~size=Options.tokenSize,
      env,
    )
  | `Box =>
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

let isTokenAtColumn = (~column: Coord.c, ~row: row) : bool =>
  switch (column, row) {
  | (C1, (`Circle | `Box, _, _))
  | (C2, (_, `Circle | `Box, _))
  | (C3, (_, _, `Circle | `Box)) => true
  | (C1, (`Empty, _, _))
  | (C2, (_, `Empty, _))
  | (C3, (_, _, `Empty))
  | (None, _) => false
  };

let isTokenAtCoord = (~coord: Coord.t, ~board: t) : bool =>
  switch (coord, board) {
  | ((c, R1), Board(r, _, _))
  | ((c, R2), Board(_, r, _))
  | ((c, R3), Board(_, _, r)) => isTokenAtColumn(~column=c, ~row=r)
  | ((_, None), _) => false
  };

let draw = (b: t, env) =>
  switch (b) {
  | Board(x, y, z) =>
    drawRow(~yPos=0, x, env);
    drawRow(~yPos=1, y, env);
    drawRow(~yPos=2, z, env);
  };
