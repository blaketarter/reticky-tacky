open Reprocessing;

let draw = (state: State.t, env) => {
  Draw.background(Theme.background, env);
  Helpers.centerText(
    ~body="Ticky-Tacky",
    ~pos=(Options.windowSize / 2, Options.windowSize / 2 - 100),
    ~font=None,
    env,
  );
  Helpers.centerText(
    ~body="click to start",
    ~pos=(Options.windowSize / 2, Options.windowSize / 2),
    ~font=None,
    env,
  );
  state;
};

let handleMouseUp = (state: State.t, _env) => {...state, view: Game};
