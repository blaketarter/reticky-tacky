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

let draw =
    (
      ~size: int,
      ~color: circleColor,
      ~xPos: int,
      ~yPos: int,
      ~text: option(string)=?,
      env,
    ) => {
  Draw.fill(color |> color_of_boxColor, env);
  Draw.strokeWeight(0, env);
  let center = (xPos + Options.tokenInset, yPos + Options.tokenInset);
  Draw.ellipse(~center, ~radx=size / 2, ~rady=size / 2, env);
  switch (text) {
  | Some(body) => Helpers.centerText(~body, ~pos=center, ~font=None, env)
  | None => ()
  };
};
