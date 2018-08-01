open Reprocessing;

let draw = (state: State.t, env) => {
  let winner =
    switch (state.view) {
    | GameWon(player) => player
    | _ => state.player
    };

  Draw.background(Player.color_of_player(winner), env);
  Helpers.centerText(
    ~body=Player.text_of_player(winner) ++ " Won!",
    ~pos=(Options.windowSize / 2, Options.windowSize / 2 - 100),
    ~font=None,
    env,
  );
  Helpers.centerText(
    ~body="click to start a new game",
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
