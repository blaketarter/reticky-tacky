open Reprocessing;

type t =
  | Circle
  | Box;

let color_of_player = (player: t) =>
  switch (player) {
  | Box => Theme.orange
  | Circle => Theme.blue
  };

let playerDrawOffset = 0;
let playerStrokeWeight = 5;
let playerOffset = playerDrawOffset + playerStrokeWeight / 2;
let playerStart = playerOffset;
let playerEnd = Options.windowSize - playerOffset;

let draw = (p: t, env) => {
  Draw.stroke(color_of_player(p), env);
  Draw.strokeWeight(playerStrokeWeight, env);
  Draw.strokeCap(Reprocessing_Common.Project, env);
  Draw.line(
    ~p1=(playerStart, playerStart),
    ~p2=(playerStart, playerEnd),
    env,
  );
  Draw.line(
    ~p1=(playerStart, playerStart),
    ~p2=(playerEnd, playerStart),
    env,
  );
  Draw.line(~p1=(playerEnd, playerEnd), ~p2=(playerEnd, playerStart), env);
  Draw.line(~p1=(playerEnd, playerEnd), ~p2=(playerStart, playerEnd), env);
};
