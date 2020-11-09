open Core_kernel;

include Grid;
include Grid.Poly;

let init = (side, f) => init(~side, ((x, y)) => f(x, y));

/*
  Index accessors
 */

let at = (grid, x, y) => get(x, y, grid);

let update = (grid, x, y, f) => update(x, y, ~f, grid);

let put = (grid, x, y, n) => set(x, y, n, grid);

/*
  Wrapped index accessors
 */

let at_w = (grid, x, y) => {
  let (x, y) = wrap_coord(grid, x, y);
  at(grid, x, y);
};

let update_w = (grid, x, y, f) => {
  let (x, y) = wrap_coord(grid, x, y);
  update(grid, x, y, f);
};

let put_w = (grid, x, y, n) => {
  let (x, y) = wrap_coord(grid, x, y);
  put(grid, x, y, n);
};

/*
  Folding and mapping
 */

let fold = (grid, acc, f) =>
  With_coords.fold(
    ~init=acc,
    ~f=(a, (x, y, n)) => f(a, x, y, n),
    With_coords.T(grid),
  );

let map = (grid, f) => {
  let init = make(~side=grid.side, f(0, 0, at(grid, 0, 0)));
  With_coords.fold(With_coords.T(grid), ~init, ~f=(accum, (x, y, n)) =>
    set(x, y, f(x, y, n), accum)
  );
};

let filter_map = (grid, f) =>
  With_coords.fold(With_coords.T(grid), ~init=[], ~f=(acc, (x, y, n)) =>
    switch (f(x, y, n)) {
    | Some(v) => [v, ...acc]
    | None => acc
    }
  );

let iter = (grid, f) =>
  With_coords.iter(With_coords.T(grid), ~f=((x, y, n)) => f(x, y, n));

let zip_map = (a, b, f) => zip_map(a, b, ~f);

let zip = (grid_a, grid_b) => zip_map(grid_a, grid_b, (a, b) => (a, b));

let multizip = grids => {
  let side = List.hd_exn(grids).side;
  if (List.exists(~f=g => g.side != side, grids)) {
    raise(Invalid_argument("grids don't all have same side"));
  };
  /* Probably a more efficient way to do this, oh well */
  Poly.init(~side, ((x, y)) => List.map(~f=g => at(g, x, y), grids));
};

/*
  Scanning
 */

let id_row_f = (acc, _y) => acc;
let noop_row_f = _y => ();

let rec scan_fold' = (grid, acc, row_f, f, x, y) =>
  switch (x, y) {
  | (x, y) when x >= grid.side =>
    let acc = row_f(acc, y);
    scan_fold'(grid, acc, row_f, f, 0, y + 1);
  | (_x, y) when y >= grid.side => acc
  | (x, y) =>
    let here = at(grid, x, y);
    let acc = f(acc, x, y, here);
    scan_fold'(grid, acc, row_f, f, x + 1, y);
  };
/**
  scan_fold folds over a list in row-major order, left to right then top to
  bottom. For example, it sees the coordinates (0,0), (1,0), (2,0), ... (x,
  0), (0, 1), ...

  It is much less efficient than fold and iter, which go in order based on
  the tree, however sometimes this iteration order is required.

  row_f is called at the end of each row.
 */
let scan_fold = (grid, acc, ~row_f=id_row_f, f) => {
  scan_fold'(grid, acc, row_f, f, 0, 0);
};

/**
  scan_iter iterates over a list in row-major order, left to right then top to
  bottom. For example, it sees the coordinates (0,0), (1,0), (2,0), ... (x,
  0), (0, 1), ...

  It is much less efficient than fold and iter, which go in order based on
  the tree, however sometimes this iteration order is required.
 */
let scan_iter = (grid, ~row_f=noop_row_f, f) =>
  scan_fold(
    grid,
    (),
    ~row_f=((), y) => row_f(y),
    ((), x, y, here) => {
      f(x, y, here);
      ();
    },
  );

/*
  Directions and neighbors
 */

/** four_directions is a list of the four cardinal direction offsets: N, E, S, W */
let four_directions = [(0, (-1)), (1, 0), (0, 1), ((-1), 0)];

/** eight_directions is a list of the eight direction offsets: NW, N, NE, ..., SW, W */
let eight_directions = [
  ((-1), (-1)),
  (0, (-1)),
  (1, (-1)),
  (1, 0),
  (1, 1),
  (0, 1),
  ((-1), 1),
  ((-1), 0),
];

/** neighbors returns the 8 neighbors starting with northwest and going clockwise */
let neighbors = (grid, x, y) => {
  List.map(~f=((dx, dy)) => at_w(grid, x + dx, y + dy), eight_directions);
};

/** neighbors_xy returns the 8 neighbors paired with their relative offset, starting with northwest and going clockwise */
let neighbors_xy = (grid, x, y) => {
  List.map(
    ~f=((dx, dy)) => (at_w(grid, x + dx, y + dy), dx, dy),
    eight_directions,
  );
};

/*
 Tests
 */

let test_print = grid => {
  scan_iter(
    grid,
    ~row_f=_ => print_endline(""),
    (_, _, here) => {
      print_string(here);
      print_string(",");
    },
  );
};

let%expect_test "init" = {
  let grid = Poly.init(~side=4, ((x, y)) => Printf.sprintf("%d:%d", x, y));
  test_print(grid);

  %expect
  {|
    0:0,1:0,2:0,3:0,
    0:1,1:1,2:1,3:1,
    0:2,1:2,2:2,3:2,
    0:3,1:3,2:3,3:3,
  |};
};

let%expect_test "map" = {
  let grid =
    Poly.init(~side=4, ident)
    |> map(_, (x, y, (hx, hy)) =>
         Printf.sprintf("%d:%d::%d:%d", x, y, hx, hy)
       );
  test_print(grid);

  %expect
  {|
    0:0::0:0,1:0::1:0,2:0::2:0,3:0::3:0,
    0:1::0:1,1:1::1:1,2:1::2:1,3:1::3:1,
    0:2::0:2,1:2::1:2,2:2::2:2,3:2::3:2,
    0:3::0:3,1:3::1:3,2:3::2:3,3:3::3:3,
  |};
};

let%expect_test "neighbors" = {
  let grid = Poly.init(~side=4, ((x, y)) => Printf.sprintf("%d:%d", x, y));
  neighbors(grid, 1, 2) |> List.iter(~f=print_endline, _);

  %expect
  {|
    0:1
    1:1
    2:1
    2:2
    2:3
    1:3
    0:3
    0:2
  |};
};

let%expect_test "neighbors_xy" = {
  let grid = Poly.init(~side=4, ((x, y)) => Printf.sprintf("%d:%d", x, y));
  neighbors_xy(grid, 1, 2)
  |> List.iter(
       ~f=((coord, dx, dy)) => Printf.printf("%s::%d:%d\n", coord, dx, dy),
       _,
     );

  %expect
  {|
    0:1::-1:-1
    1:1::0:-1
    2:1::1:-1
    2:2::1:0
    2:3::1:1
    1:3::0:1
    0:3::-1:1
    0:2::-1:0
  |};
};