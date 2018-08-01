open Reprocessing;

let initialState: State.t = {player: Circle, view: Title, board: Board.empty};

let setup = env => {
  Env.size(~width=Options.windowSize, ~height=Options.windowSize, env);
  Draw.rectMode(Options.rectMode, env);
  /* Draw.tint(Options.fontColor, env); */
  initialState;
};

let draw = (state: State.t, env) =>
  switch (state.view) {
  | Title => Title.draw(state, env)
  | Game => Game.draw(state, env)
  | GameOver(_) => GameOver.draw(state, env)
  };

let mouseUp = (state: State.t, env) =>
  switch (state.view) {
  | Title => Title.handleMouseUp(state, env)
  | Game => Game.handleMouseUp(state, env)
  | GameOver(_) => GameOver.handleMouseUp(state, env)
  };

run(~mouseUp, ~setup, ~draw, ());
