type t =
  | Click
  | Draw;

let run = (t: t, state: State.t, env) : State.t =>
  switch (t) {
  | Draw when state.player === Ai.player && Options.aiEnabled === true =>
    Ai.turn(state, env)
  | Click when state.player !== Ai.player => PlayerTurn.run(state, env)
  | _ => state
  };