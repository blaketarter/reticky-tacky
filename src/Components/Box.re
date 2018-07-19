open Reprocessing;

type boxColor =
  | Grey
  | Blue
  | Orange;

let color_of_boxColor = (b: boxColor) : Reprocessing.colorT =>
  switch (b) {
  | Grey => Theme.grey
  | Blue => Theme.blue
  | Orange => Theme.orange
  };

type t = {
  color: boxColor,
  size: int,
  pos: (int, int),
};

let draw = (~size: int, ~color: boxColor, ~xPos: int, ~yPos: int, env) => {
  Draw.fill(color |> color_of_boxColor, env);
  Draw.rect(~pos=(xPos, yPos), ~width=size, ~height=size, env);
};
