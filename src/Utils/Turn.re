type t =
  | Click
  | Draw;

let run = (t: t, state: State.t, env) : State.t =>
  switch (t) {
  | Draw when state.player === Ai.player && Options.aiEnabled === true =>
    Ai.turn(state, env)
  | Click
      when
        Options.aiAutoPlay === true
        && state.player !== Ai.player
        || Options.aiAutoPlay === false =>
    PlayerTurn.run(state, env)
  | _ => state
  };