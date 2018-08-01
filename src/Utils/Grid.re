let each = (cb: (int, int) => unit) : unit =>
  for (y in 0 to Options.gridSize - 1) {
    for (x in 0 to Options.gridSize - 1) {
      cb(x, y);
    };
  };
