let isCoordInBounds = (~lower: int, ~upper: int, test: int) : bool =>
  test > lower && test < upper;

let isInBox =
    (~boxX: int, ~boxY: int, ~boxSize: int, ~testX: int, ~testY: int)
    : bool =>
  switch (
    isCoordInBounds(~lower=boxX, ~upper=boxX + boxSize, testX),
    isCoordInBounds(~lower=boxY, ~upper=boxY + boxSize, testY),
  ) {
  | (true, true) => true
  | (_, _) => false
  };

let centerText = (~body, ~pos as (x, y), ~font, env) => {
  let width = Reprocessing_Font.Font.calcStringWidth(env, font, body);
  switch (font) {
  | None =>
    Reprocessing.Draw.text(
      ~body,
      ~pos=(x - int_of_float(width) / 2, y - 13),
      env,
    )
  | Some(innerFont) =>
    Reprocessing.Draw.text(
      ~font=innerFont,
      ~body,
      ~pos=(x - int_of_float(width) / 2, y - 13),
      env,
    )
  };
};