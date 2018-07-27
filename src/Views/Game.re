open Reprocessing;

let draw = (state: State.t, env) => {
  Draw.background(Theme.background, env);

  for (y in 0 to Options.gridSize - 1) {
    for (x in 0 to Options.gridSize - 1) {
      Box.draw(
        ~color=Box.Grey,
        ~size=Options.cellSize,
        ~xPos=x * (Options.cellSize + Options.spacing) + Options.spacing,
        ~yPos=y * (Options.cellSize + Options.spacing) + Options.spacing,
        env,
      );
    };
  };

  Board.draw(state.board, env);
  state;
};

let handleMouseUp = (state: State.t, env) : State.t => {
  let (mx, my) = Env.mouse(env);

  let nextBoard = ref(state.board);
  let nextPlayer = ref(state.player);
  let nextView = ref(state.view);

  for (y in 0 to Options.gridSize - 1) {
    for (x in 0 to Options.gridSize - 1) {
      let lowerX = x * (Options.cellSize + Options.spacing) + Options.spacing;
      let upperX = lowerX + Options.cellSize;

      let lowerY = y * (Options.cellSize + Options.spacing) + Options.spacing;
      let upperY = lowerY + Options.cellSize;

      if (Helpers.isCoordInBounds(~lower=lowerX, ~upper=upperX, mx)
          && Helpers.isCoordInBounds(~lower=lowerY, ~upper=upperY, my)) {
        let coord = Coord.coord_of_xy(~x, ~y);

        let token =
          switch (state.player) {
          | Player.X => Board.X
          | Player.O => Board.O
          };

        nextBoard :=
          Board.insertTokenAtCoord(~coord, ~board=state.board, token);

        nextPlayer :=
          (
            switch (state.player) {
            | Player.X => Player.O
            | Player.O => Player.X
            }
          );

        if (Board.isBoardWon(nextBoard^) || Board.isBoardFull(nextBoard^)) {
          nextView := Views.GameOver(state.player);
        };
      };
    };
  };

  {...state, board: nextBoard^, player: nextPlayer^, view: nextView^};
};