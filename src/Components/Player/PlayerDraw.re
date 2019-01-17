open Reprocessing;

let run = (p: Player.t, env) => {
  Draw.stroke(Player.color_of_player(p), env);
  Draw.strokeWeight(Player.playerStrokeWeight, env);
  Draw.strokeCap(Reprocessing_Common.Project, env);
  Draw.line(
    ~p1=(Player.playerStart, Player.playerStart),
    ~p2=(Player.playerStart, Player.playerEnd),
    env,
  );
  Draw.line(
    ~p1=(Player.playerStart, Player.playerStart),
    ~p2=(Player.playerEnd, Player.playerStart),
    env,
  );
  Draw.line(
    ~p1=(Player.playerEnd, Player.playerEnd),
    ~p2=(Player.playerEnd, Player.playerStart),
    env,
  );
  Draw.line(
    ~p1=(Player.playerEnd, Player.playerEnd),
    ~p2=(Player.playerStart, Player.playerEnd),
    env,
  );
};