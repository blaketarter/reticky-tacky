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

let handleMouseUp = (state: State.t, _env) => state;
