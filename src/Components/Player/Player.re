type t =
  | Circle
  | Box;

let color_of_player = (player: t) =>
  switch (player) {
  | Box => Theme.orange
  | Circle => Theme.blue
  };

let text_of_player = (player: t) : string =>
  switch (player) {
  | Box => "Box"
  | Circle => "Circle"
  };

let playerDrawOffset = 0;
let playerStrokeWeight = 5;
let playerOffset = playerDrawOffset + playerStrokeWeight / 2;
let playerStart = playerOffset;
let playerEnd = Options.windowSize - playerOffset;

let next_player = (p: t) : t =>
  switch (p) {
  | Circle => Box
  | Box => Circle
  };

let token_of_player = (p: t) : Token.t =>
  switch (p) {
  | Circle => Token.Circle(Token.initialValue)
  | Box => Token.Box(Token.initialValue)
  };