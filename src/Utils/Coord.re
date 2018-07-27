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

let c_of_x = x : c =>
  switch (x) {
  | 0 =>
    print_endline("C1");
    C1;
  | 1 =>
    print_endline("C2");
    C2;
  | 2 =>
    print_endline("C3");
    C3;
  | _ =>
    print_endline("CNone");
    None;
  };

let r_of_y = y : r =>
  switch (y) {
  | 0 =>
    print_endline("R3");
    R1;
  | 1 =>
    print_endline("R3");
    R2;
  | 2 =>
    print_endline("R3");
    R3;
  | _ =>
    print_endline("RNone");
    None;
  };

let coord_of_xy = (~x, ~y) : t => (x |> c_of_x, y |> r_of_y);