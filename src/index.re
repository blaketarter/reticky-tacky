open Reprocessing;

let initialState: State.t = {
  view: Title,
  box: {
    color: Theme.blue,
    size: 300,
    pos: (150, 150),
  },
};

let setup = env => {
  Env.size(~width=Options.windowSize, ~height=Options.windowSize, env);
  initialState;
};

let drawView = (state: State.t, env) =>
  switch (state.view) {
  | Title => Title.draw(state, env)
  | Game => Game.draw(state, env)
  };

let draw = (state: State.t, env) => drawView(state, env);

let mouseUp = (state: State.t, env) =>
  switch (state.view) {
  | Title => Title.handleMouseUp(state, env)
  | Game => Game.handleMouseUp(state, env)
  };

run(~mouseUp, ~setup, ~draw, ());
