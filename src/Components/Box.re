open Reprocessing;

type t = {
  color: Reprocessing_Common.colorT,
  size: int,
  pos: (int, int),
};

let draw = (box: t, env) => {
  let (x, y) = box.pos;
  Draw.fill(box.color, env);
  Draw.rect(~pos=(x, y), ~width=box.size, ~height=box.size, env);
};
