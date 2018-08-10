type t =
  | Empty
  | Circle(int)
  | Box(int);

let initialValue = 3;

let getCoords = (~yPos: int, ~xPos: int) => (
  xPos
  * (Options.cellSize + Options.spacing)
  + Options.spacing
  + Options.tokenInset,
  yPos
  * (Options.cellSize + Options.spacing)
  + Options.spacing
  + Options.tokenInset,
);

let decrement = (t: t) : t =>
  switch (t) {
  | Circle(n) when n > 1 => Circle(n - 1)
  | Circle(_) => Empty
  | Box(n) when n > 1 => Box(n - 1)
  | Box(_) => Empty
  | Empty => Empty
  };

let draw = (~yPos: int, ~xPos: int, t: t, env) => {
  let (x, y) = getCoords(~xPos, ~yPos);
  switch (t) {
  | Empty => ()
  | Circle(n) =>
    Circle.draw(
      ~color=Circle.Blue,
      ~xPos=x,
      ~yPos=y,
      ~size=Options.tokenSize,
      ~text=n |> string_of_int,
      env,
    )
  | Box(n) =>
    Box.draw(
      ~color=Box.Orange,
      ~xPos=x,
      ~yPos=y,
      ~size=Options.tokenSize,
      ~text=n |> string_of_int,
      env,
    )
  };
};