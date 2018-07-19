open Reprocessing;

type circleColor =
  | Grey
  | Blue
  | Orange;

let color_of_boxColor = (c: circleColor) : Reprocessing.colorT =>
  switch (c) {
  | Grey => Theme.grey
  | Blue => Theme.blue
  | Orange => Theme.orange
  };

type t = {
  color: circleColor,
  size: int,
  pos: (int, int),
};

let draw = (~size: int, ~color: circleColor, ~xPos: int, ~yPos: int, env) => {
  Draw.fill(color |> color_of_boxColor, env);
  Draw.ellipse(~center=(xPos, yPos), ~radx=size / 2, ~rady=size / 2, env);
};
