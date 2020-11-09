let subdivide_with_fill = (old_grid: Grid.t('a), fill): Grid.t('a) => {
  open Grid_compat;

  /*
     old_grid:
     1-2-3-
     ------
     4-5-6-
     ------
     7-8-9-
     ------

     diamonds:
     =-=-=-
     -1-2-3
     =-=-=-
     -4-5-6
     =-=-=-
     -7-8-9

     offset_squares:
     =1=2=3
     -=-=-=
     =4=5=6
     -=-=-=
     =7=8=9
     -=-=-=

     center_squares:
     ======
     1=2=3=
     ======
     4=5=6=
     ======
     7=8=9=
   */

  let diamonds =
    init(
      old_grid.side,
      (x, y) => {
        let nw = at_w(old_grid, x, y);
        let ne = at_w(old_grid, x + 1, y);
        let se = at_w(old_grid, x + 1, y + 1);
        let sw = at_w(old_grid, x, y + 1);
        fill(nw, ne, se, sw);
      },
    );
  let offset_squares =
    init(
      old_grid.side,
      (x, y) => {
        let n = at_w(diamonds, x, y - 1);
        let e = at_w(old_grid, x + 1, y);
        let s = at_w(diamonds, x, y);
        let w = at_w(old_grid, x, y);
        fill(n, e, s, w);
      },
    );
  let center_squares =
    init(
      old_grid.side,
      (x, y) => {
        let n = at_w(old_grid, x, y);
        let e = at_w(diamonds, x, y);
        let s = at_w(old_grid, x, y + 1);
        let w = at_w(diamonds, x - 1, y);
        fill(n, e, s, w);
      },
    );
  let combined =
    init(
      old_grid.side * 2,
      (x, y) => {
        let x' = x / 2;
        let y' = y / 2;
        switch (x mod 2 == 0, y mod 2 == 0) {
        | (true, true) => at(old_grid, x', y')
        | (false, true) => at(offset_squares, x', y')
        | (false, false) => at(diamonds, x', y')
        | (true, false) => at(center_squares, x', y')
        };
      },
    );
  combined;
};

let overwrite_subdivide_with_fill = (old_grid: Grid.t('a), fill) => {
  let grid = subdivide_with_fill(old_grid, fill);
  Grid_compat.map(grid, (x, y, here) =>
    if (x mod 2 == 0 && y mod 2 == 0) {
      let n = Grid_compat.at_w(grid, x, y - 1);
      let e = Grid_compat.at_w(grid, x + 1, y);
      let s = Grid_compat.at_w(grid, x, y + 1);
      let w = Grid_compat.at_w(grid, x - 1, y);
      fill(n, e, s, w);
    } else {
      here;
    }
  );
};

/** subdivide uses [random_fill] */
let subdivide = subdivide_with_fill(_, Fill.random);

let%expect_test "subdivide_with_fill" = {
  let rec print_list =
    fun
    | [] => print_newline()
    | [a] => print_endline(a)
    | [a, ...rest] => {
        print_string(a);
        print_string(", ");
        print_list(rest);
      };

  /*
    0  1  2  3
    4  5  6  7
    8  9 10 11
   12 13 14 15
   */
  let grid =
    Grid_compat.init(4, (x, y) => [string_of_int(x + y * 4)])
    |> subdivide_with_fill(_, (a, b, c, d) => a @ b @ c @ d);

  /* Old tile */
  print_list(Grid_compat.at(grid, 0, 0));
  %expect
  {| 0 |};

  /* Diamond */
  /*
    0   1
      X
    4   5
   */
  print_list(Grid_compat.at(grid, 1, 1));
  %expect
  {| 0, 1, 5, 4 |};

  /* Offset square */
  /*
      =
    0 X 1
      =
   */
  print_list(Grid_compat.at(grid, 1, 0));
  %expect
  {| 12, 13, 1, 0, 1, 0, 1, 5, 4, 0 |};

  /* Center square */
  /*
     0
   = X =
     4
   */
  print_list(Grid_compat.at(grid, 0, 1));
  %expect
  {| 0, 0, 1, 5, 4, 4, 3, 0, 4, 7 |};
};