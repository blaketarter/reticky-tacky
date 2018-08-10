open Reprocessing;

let draw = (state: State.t, env) => {
  Draw.background(Theme.background, env);

  Player.draw(state.player, env);

  Grid.each((x, y) =>
    Box.draw(
      ~color=Box.Grey,
      ~size=Options.cellSize,
      ~xPos=x * (Options.cellSize + Options.spacing) + Options.spacing,
      ~yPos=y * (Options.cellSize + Options.spacing) + Options.spacing,
      env,
    )
  );

  Board.draw(state.board, env);

  Grid.each((x, y) => {
    let coord = Coord.coord_of_xy(~x, ~y);
    let (x, y) = Token.getCoords(~xPos=x, ~yPos=y);
    let player_token =
      switch (state.player) {
      | Player.Circle => Token.Circle(Token.initialValue)
      | Player.Box => Token.Box(Token.initialValue)
      };
    let score = Ai.score_of_coord(~board=state.board, ~coord, player_token);
    Helpers.centerText(
      ~body=score |> string_of_int,
      ~pos=(x + Options.tokenInset, y + Options.tokenInset),
      ~font=None,
      env,
    );
  });
  state;
};

let handleMouseUp = (state: State.t, env) : State.t => {
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
          nextBoard := Board.decrementEach(nextBoard^);
          hasDecRun := true;
        };

        let token =
          switch (state.player) {
          | Player.Circle => Token.Circle(Token.initialValue)
          | Player.Box => Token.Box(Token.initialValue)
          };

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
              | Player.Circle => Player.Box
              | Player.Box => Player.Circle
              }
            );
        };
      };
    };
  });

  {...state, board: nextBoard^, player: nextPlayer^, view: nextView^};
};
