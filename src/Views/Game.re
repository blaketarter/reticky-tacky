open Reprocessing;

let draw = (state: State.t, env) => {
  Draw.background(Theme.background, env);

  PlayerDraw.run(state.player, env);

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
  Turn.run(Draw, state, env);
};

let handleMouseUp = (state: State.t, env) : State.t =>
  Turn.run(Click, state, env);