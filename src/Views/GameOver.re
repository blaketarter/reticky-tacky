open Reprocessing;

let draw = (state: State.t, env) => {
  Draw.background(Theme.white, env);
  Helpers.centerText(
    ~body="Game Over!",
    ~pos=(Options.windowSize / 2, Options.windowSize / 2 - 100),
    ~font=None,
    env,
  );
  Helpers.centerText(
    ~body="click to restart",
    ~pos=(Options.windowSize / 2, Options.windowSize / 2),
    ~font=None,
    env,
  );
  state;
};

let handleMouseUp = (state: State.t, _env) => {
  ...state,
  view: Game,
  board: Board.empty,
};