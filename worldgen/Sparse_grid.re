module Coord = {
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

module Coord_map = Map.Make(Coord);

/**
  t is a grid that's expected to be mostly empty. Instead of storing every
  coordinate, it stores non-empty coordinates in a tree.

  Sparse_grid tries to mirror the interface of Grid as much as is practical.
 */
type t('a) = {
  side: int,
  map: Coord_map.t('a),
};

let make = side => {side, map: Coord_map.empty};

let is_within = (side, x, y) => 0 <= x && x < side && 0 <= y && y < side;

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
  assert_within(grid.side, x, y);
  Coord_map.find_opt((x, y), grid.map);
};

let put = (grid, x, y, n) => {
  assert_within(grid.side, x, y);
  let map = Coord_map.add((x, y), n, grid.map);
  {...grid, map};
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
  let map = Coord_map.mapi(f, grid.map);
  {...grid, map};
};

let fold = (grid, f, acc) => {
  Coord_map.fold(f, grid.map, acc);
};

let iter = (grid, f) => {
  Coord_map.iter(f, grid.map);
};