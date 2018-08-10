type c =
  | C1
  | C2
  | C3
  | None;
type r =
  | R1
  | R2
  | R3
  | None;

type t = (c, r);

let empty: t = (None, None);

let c_of_x = x : c =>
  switch (x) {
  | 0 => C1
  | 1 => C2
  | 2 => C3
  | _ => None
  };

let r_of_y = y : r =>
  switch (y) {
  | 0 => R1
  | 1 => R2
  | 2 => R3
  | _ => None
  };

let coord_of_xy = (~x, ~y) : t => (x |> c_of_x, y |> r_of_y);