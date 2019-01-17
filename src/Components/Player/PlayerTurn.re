open Reprocessing;

let run = (state: State.t, env) : State.t => {
  let (mx, my) = Env.mouse(env);
  let hasDecRun = ref(false);

  let nextBoard = ref(state.board);
  let nextPlayer = ref(state.player);
  let nextView = ref(state.view);

  Grid.each((x, y) => {
    let lowerX = x * (Options.cellSize + Options.spacing) + Options.spacing;
    let upperX = lowerX + Options.cellSize;

    let lowerY = y * (Options.cellSize + Options.spacing) + Options.spacing;
    let upperY = lowerY + Options.cellSize;

    if (Helpers.isCoordInBounds(~lower=lowerX, ~upper=upperX, mx)
        && Helpers.isCoordInBounds(~lower=lowerY, ~upper=upperY, my)) {
      let coord = Coord.coord_of_xy(~x, ~y);

      if (! Board.isTokenAtCoord(~coord, ~board=nextBoard^)) {
        if (! hasDecRun^) {
          if (Options.decrementEnabled) {
            nextBoard := Board.decrementEach(nextBoard^);
          };
          hasDecRun := true;
        };

        let token = state.player |> Player.token_of_player;

        nextBoard :=
          Board.insertTokenAtCoord(~coord, ~board=nextBoard^, token);

        if (Board.isBoardWon(nextBoard^)) {
          nextView := Views.GameWon(state.player);
        } else if (Board.isBoardFull(nextBoard^)) {
          nextView := Views.GameOver;
        } else {
          nextPlayer :=
            (
              switch (state.player) {
              | Circle => Box
              | Box => Circle
              }
            );
        };
      };
    };
  });

  {...state, board: nextBoard^, player: nextPlayer^, view: nextView^};
};