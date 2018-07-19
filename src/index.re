open Reprocessing;

let initialState: State.t = {
  view: Title,
  board: Board((O, X, O), (X, O, Empty), (O, X, Empty)),
};

let setup = env => {
  Env.size(~width=Options.windowSize, ~height=Options.windowSize, env);
  initialState;
};

let draw = (state: State.t, env) =>
  switch (state.view) {
  | Title => Title.draw(state, env)
  | Game => Game.draw(state, env)
  };

let mouseUp = (state: State.t, env) =>
  switch (state.view) {
  | Title => Title.handleMouseUp(state, env)
  | Game => Game.handleMouseUp(state, env)
  };

run(~mouseUp, ~setup, ~draw, ());
