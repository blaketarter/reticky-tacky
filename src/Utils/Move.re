let next = (state: State.t) : State.t =>
  if (Board.isBoardWon(state.board)) {
    {...state, view: Views.GameWon(state.player)};
  } else if (Board.isBoardFull(state.board)) {
    {...state, view: Views.GameOver};
  } else {
    {...state, player: state.player |> Player.next_player};
  };