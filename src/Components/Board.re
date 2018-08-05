type row = (Token.t, Token.t, Token.t);

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

let getNthItem = (n: int, r: row) : Token.t =>
  switch (n, r) {
  | (0, (m, _, _)) => m
  | (1, (_, m, _)) => m
  | (2, (_, m, _)) => m
  | (_, (_, _, _)) => Empty
  };

let isBoardWon = (b: t) : bool =>
  switch (b) {
  | Board((Circle(_), Circle(_), Circle(_)), (_, _, _), (_, _, _))
  | Board((Box(_), Box(_), Box(_)), (_, _, _), (_, _, _))
  | Board((_, _, _), (Circle(_), Circle(_), Circle(_)), (_, _, _))
  | Board((_, _, _), (Box(_), Box(_), Box(_)), (_, _, _))
  | Board((_, _, _), (_, _, _), (Circle(_), Circle(_), Circle(_)))
  | Board((_, _, _), (_, _, _), (Box(_), Box(_), Box(_)))
  | Board((Circle(_), _, _), (Circle(_), _, _), (Circle(_), _, _))
  | Board((Box(_), _, _), (Box(_), _, _), (Box(_), _, _))
  | Board((_, Circle(_), _), (_, Circle(_), _), (_, Circle(_), _))
  | Board((_, Box(_), _), (_, Box(_), _), (_, Box(_), _))
  | Board((_, _, Circle(_)), (_, _, Circle(_)), (_, _, Circle(_)))
  | Board((_, _, Box(_)), (_, _, Box(_)), (_, _, Box(_)))
  | Board((Circle(_), _, _), (_, Circle(_), _), (_, _, Circle(_)))
  | Board((Box(_), _, _), (_, Box(_), _), (_, _, Box(_)))
  | Board((_, _, Circle(_)), (_, Circle(_), _), (Circle(_), _, _))
  | Board((_, _, Box(_)), (_, Box(_), _), (Box(_), _, _)) => true
  | Board((_, _, _), (_, _, _), (_, _, _)) => false
  };

let isBoardFull = (b: t) : bool =>
  switch (b) {
  | Board(
      (Circle(_) | Box(_), Circle(_) | Box(_), Circle(_) | Box(_)),
      (Circle(_) | Box(_), Circle(_) | Box(_), Circle(_) | Box(_)),
      (Circle(_) | Box(_), Circle(_) | Box(_), Circle(_) | Box(_)),
    ) =>
    true
  | Board((_, _, _), (_, _, _), (_, _, _)) => false
  };

let drawRow = (~yPos, r: row, env) =>
  switch (r) {
  | (x, y, z) =>
    Token.draw(~yPos, ~xPos=0, x, env);
    Token.draw(~yPos, ~xPos=1, y, env);
    Token.draw(~yPos, ~xPos=2, z, env);
  };

let insetTokenAtColumn = (~column: Coord.c, ~row: row, token: Token.t) : row =>
  switch (column, row) {
  | (C1, (_, x, y)) => (token, x, y)
  | (C2, (x, _, y)) => (x, token, y)
  | (C3, (x, y, _)) => (x, y, token)
  | (None, r) => r
  };

let insertTokenAtCoord = (~coord: Coord.t, ~board: t, token: Token.t) : t =>
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
  | (C1, (Circle(_) | Box(_), _, _))
  | (C2, (_, Circle(_) | Box(_), _))
  | (C3, (_, _, Circle(_) | Box(_))) => true
  | (C1, (Empty, _, _))
  | (C2, (_, Empty, _))
  | (C3, (_, _, Empty))
  | (None, _) => false
  };

let isTokenAtCoord = (~coord: Coord.t, ~board: t) : bool =>
  switch (coord, board) {
  | ((c, R1), Board(r, _, _))
  | ((c, R2), Board(_, r, _))
  | ((c, R3), Board(_, _, r)) => isTokenAtColumn(~column=c, ~row=r)
  | ((_, None), _) => false
  };

let getTokenAtColumn = (~column: Coord.c, row: row) : Token.t =>
  switch (column, row) {
  | (C1, (t, _, _))
  | (C2, (_, t, _))
  | (C3, (_, _, t)) => t
  | (None, _) => Empty
  };

let getTokenAtCoord = (~coord: Coord.t, ~board: t) : Token.t =>
  switch (coord, board) {
  | ((c, R1), Board(r, _, _))
  | ((c, R2), Board(_, r, _))
  | ((c, R3), Board(_, _, r)) => getTokenAtColumn(~column=c, r)
  | ((_, None), _) => Empty
  };

let decrementEach = (board: t) : t => {
  let nextBoard = ref(board);

  Grid.each((x, y) => {
    let coord = Coord.coord_of_xy(~x, ~y);

    if (isTokenAtCoord(~coord, ~board=nextBoard^)) {
      nextBoard :=
        getTokenAtCoord(~coord, ~board=nextBoard^)
        |> Token.decrement
        |> insertTokenAtCoord(~coord, ~board=nextBoard^);
    };
  });

  nextBoard^;
};

let map = (cb: (Token.t, Coord.t) => Token.t, board: t) : t => {
  let nextBoard = ref(board);
  Grid.each((x, y) => {
    let coord = Coord.coord_of_xy(~x, ~y);
    let token = getTokenAtCoord(~coord, ~board=nextBoard^);
    nextBoard :=
      cb(token, coord) |> insertTokenAtCoord(~coord, ~board=nextBoard^);
  });
  nextBoard^;
};

let draw = (b: t, env) =>
  switch (b) {
  | Board(x, y, z) =>
    drawRow(~yPos=0, x, env);
    drawRow(~yPos=1, y, env);
    drawRow(~yPos=2, z, env);
  };
