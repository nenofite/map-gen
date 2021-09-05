open Core_kernel;

module Coord = {
  module T = {
    [@deriving (sexp, bin_io)]
    type t = (int, int);

    let compare = ((ax, ay), (bx, by)) => {
      /*
        Order first by Y, then by X. This way the memory layout (for caching)
        will hopefully by more similar to Grid's
       */
      switch (Int.compare(ay, by)) {
      | 0 => Int.compare(ax, bx)
      | n => n
      };
    };
  };
  include T;
  include Comparable.Make_binable(T);
};

/**
  t is a grid that's expected to be mostly empty. Instead of storing every
  coordinate, it stores non-empty coordinates in a tree.

  Sparse_grid tries to mirror the interface of Grid as much as is practical.
 */
[@deriving bin_io]
type t('a) = {
  side: int,
  map: Coord.Map.t('a),
};

let side = t => t.side;

let make = side => {side, map: Coord.Map.empty};

let is_within = (side, x, y) => 0 <= x && x < side && 0 <= y && y < side;
let is_within' = (t, x, y) => 0 <= x && x < t.side && 0 <= y && y < t.side;

let assert_within = (side, x, y) =>
  if (!is_within(side, x, y)) {
    raise(Invalid_argument("Coordinate out of bounds"));
  };

let wrap_coord' = (side, x, y) => {
  let x' =
    switch (x mod side) {
    | x when x < 0 => x + side
    | x => x
    };
  let y' =
    switch (y mod side) {
    | y when y < 0 => y + side
    | y => y
    };
  (x', y');
};
let wrap_coord = (grid, x, y) => wrap_coord'(grid.side, x, y);

let at = (grid, x, y) => {
  /* assert_within(grid.side, x, y); */
  Coord.Map.find(grid.map, (x, y));
};

let has = (grid, x, y) =>
  is_within(grid.side, x, y) && at(grid, x, y) |> Option.is_some;

let put = (grid, x, y, n) => {
  assert_within(grid.side, x, y);
  let map = Coord.Map.set(grid.map, ~key=(x, y), ~data=n);
  {...grid, map};
};

let put_opt = (grid, x, y, n) =>
  if (is_within(grid.side, x, y)) {
    put(grid, x, y, n);
  } else {
    grid;
  };

let at_w = (grid, x, y) => {
  let (x, y) = wrap_coord(grid, x, y);
  at(grid, x, y);
};

let put_w = (grid, x, y, n) => {
  let (x, y) = wrap_coord(grid, x, y);
  put(grid, x, y, n);
};

let map = (grid, f) => {
  let f = (~key, ~data) => f(key, data);
  let map = Coord.Map.mapi(grid.map, ~f);
  {...grid, map};
};

let fold = (grid, f, acc) => {
  let f = (~key, ~data, acc) => f(key, data, acc);
  Coord.Map.fold(grid.map, ~init=acc, ~f);
};

let for_all = (grid, f) => {
  let f = (~key, ~data) => f(key, data);
  Coord.Map.for_alli(grid.map, ~f);
};

let filter_map = (grid, f) => {
  let f = (~key, ~data) => f(key, data);
  let map = Coord.Map.filter_mapi(grid.map, ~f);
  {...grid, map};
};

let add_all = (~onto, overgrid) => {
  fold(overgrid, ((x, z), d, grid) => put(grid, x, z, d), onto);
};

let iter = (grid, f) => {
  let f = (~key, ~data) => f(key, data);
  Coord.Map.iteri(grid.map, ~f);
};

/** creates a grid with the same dimensions as the sparse grid. Where the sparse grid is undefined, default is used. */
let to_grid = (~default, t) => {
  fold(
    t,
    ((x, y), n) => Grid.Poly.set(x, y, n),
    Grid.make(~side=t.side, default),
  );
};
