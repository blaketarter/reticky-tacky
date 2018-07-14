open Reprocessing;

let draw = (state: State.t, env) => {
  Draw.background(Theme.background, env);

  let cellSize = (Options.windowSize - Options.spacing) / 3;
  let boxSize = cellSize - Options.spacing;

  for (y in 0 to Options.gridSize - 1) {
    for (x in 0 to Options.gridSize - 1) {
      Box.draw(
        {
          color: Theme.grey,
          size: boxSize,
          pos: (
            x * cellSize + Options.spacing,
            y * cellSize + Options.spacing,
          ),
        },
        env,
      );
    };
  };

  state;
};

let handleMouseUp = (state: State.t, env) => {
  let (x, y) = Env.mouse(env);

  if (Helpers.isInBox(
        ~boxX=fst(state.box.pos),
        ~boxY=fst(state.box.pos),
        ~boxSize=state.box.size,
        ~testX=x,
        ~testY=y,
      )) {
    {
      ...state,
      box: {
        ...state.box,
        color: state.box.color === Theme.blue ? Theme.orange : Theme.blue,
      },
    };
  } else {
    state;
  };
};
