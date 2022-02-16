let at_w = (g, x, y) => Mut.get_wrap(~x, ~z=y, g);

let subdivide_with_fill = (old_grid: Mut.t('a), fill): Mut.t('a) => {
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
    Mut.init_exact(
      ~side=old_grid.side,
      ~f=(~x, ~z) => {
        let nw = at_w(old_grid, x, z);
        let ne = at_w(old_grid, x + 1, z);
        let se = at_w(old_grid, x + 1, z + 1);
        let sw = at_w(old_grid, x, z + 1);
        fill(nw, ne, se, sw);
      },
    );
  let offset_squares =
    Mut.init_exact(
      ~side=old_grid.side,
      ~f=(~x, ~z) => {
        let n = at_w(diamonds, x, z - 1);
        let e = at_w(old_grid, x + 1, z);
        let s = at_w(diamonds, x, z);
        let w = at_w(old_grid, x, z);
        fill(n, e, s, w);
      },
    );
  let center_squares =
    Mut.init_exact(
      ~side=old_grid.side,
      ~f=(~x, ~z as y) => {
        let n = at_w(old_grid, x, y);
        let e = at_w(diamonds, x, y);
        let s = at_w(old_grid, x, y + 1);
        let w = at_w(diamonds, x - 1, y);
        fill(n, e, s, w);
      },
    );
  let combined =
    Mut.init_exact(
      ~side=old_grid.side * 2,
      ~f=(~x, ~z) => {
        let x' = x / 2;
        let z' = z / 2;
        switch (x mod 2 == 0, z mod 2 == 0) {
        | (true, true) => Mut.get(old_grid, ~x=x', ~z=z')
        | (false, true) => Mut.get(offset_squares, ~x=x', ~z=z')
        | (false, false) => Mut.get(diamonds, ~x=x', ~z=z')
        | (true, false) => Mut.get(center_squares, ~x=x', ~z=z')
        };
      },
    );
  combined;
};

let overwrite_subdivide_with_fill = (old_grid: Mut.t('a), fill) => {
  let grid = subdivide_with_fill(old_grid, fill);
  Mut.map(grid, ~f=(~x, ~z as y, here) =>
    if (x mod 2 == 0 && y mod 2 == 0) {
      let n = at_w(grid, x, y - 1);
      let e = at_w(grid, x + 1, y);
      let s = at_w(grid, x, y + 1);
      let w = at_w(grid, x - 1, y);
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
    Mut.init_exact(~side=4, ~f=(~x, ~z) => [string_of_int(x + z * 4)])
    |> subdivide_with_fill(_, (a, b, c, d) => a @ b @ c @ d);

  /* Old tile */
  print_list(Mut.get(grid, ~x=0, ~z=0));
  %expect
  {| 0 |};

  /* Diamond */
  /*
    0   1
      X
    4   5
   */
  print_list(Mut.get(grid, ~x=1, ~z=1));
  %expect
  {| 0, 1, 5, 4 |};

  /* Offset square */
  /*
      =
    0 X 1
      =
   */
  print_list(Mut.get(grid, ~x=1, ~z=0));
  %expect
  {| 12, 13, 1, 0, 1, 0, 1, 5, 4, 0 |};

  /* Center square */
  /*
     0
   = X =
     4
   */
  print_list(Mut.get(grid, ~x=0, ~z=1));
  %expect
  {| 0, 0, 1, 5, 4, 4, 3, 0, 4, 7 |};
};
