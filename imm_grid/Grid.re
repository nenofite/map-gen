/**
  - Tile is a leaf node, storing a single tile in the grid
  - Quad is a branch node with four children in clockwise order: NW, NE, SE, SW
 */
type node('a) =
  | Tile('a)
  | Quad(node('a), node('a), node('a), node('a));

/**
  t is an immutable grid. The grid is formed by a 4-tree where each branch
  has the same depth. The grid must be a square and the side length must be a
  power of 2.
 */
type t('a) = {
  side: int,
  root: node('a),
};

let rec is_log2 =
  fun
  | x when x <= 0 => false
  | 1 => true
  | x when x mod 2 != 0 => false
  | x => is_log2(x / 2);

let assert_side_is_log2 = side =>
  if (!is_log2(side)) {
    let msg = Printf.sprintf("grid side is not a power of 2: %d", side);
    raise(Invalid_argument(msg));
  };

let assert_within = (side, x, y) =>
  if (x < 0 || x >= side || y < 0 || y >= side) {
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

let rec make_tree = (side, base) =>
  switch (side) {
  | 1 => base
  | side =>
    let base = Quad(base, base, base, base);
    make_tree(side / 2, base);
  };

let make = (side, default) => {
  assert_side_is_log2(side);
  let root = make_tree(side, Tile(default));
  {side, root};
};

/*
  node_side is the side length of a Quad or Tile at this level. For example a grid
  with side=2 would have a Quad with node_side=2 over Tiles with node_side=1

  node_x,y are the coordinates of the top-left corner of this Quad/Tile
 */
let rec init' = (node_side, node_x, node_y, f) =>
  switch (node_side) {
  | 1 => Tile(f(node_x, node_y))
  | node_side =>
    let sub_side = node_side / 2;
    let nw = init'(sub_side, node_x, node_y, f);
    let ne = init'(sub_side, node_x + sub_side, node_y, f);
    let se = init'(sub_side, node_x + sub_side, node_y + sub_side, f);
    let sw = init'(sub_side, node_x, node_y + sub_side, f);
    Quad(nw, ne, se, sw);
  };
let init = (side, f) => {
  assert_side_is_log2(side);
  let root = init'(side, 0, 0, f);
  {side, root};
};

/*
  Index accessors
 */

let rec at' = (node, node_side, x, y) =>
  switch (node) {
  | Tile(n) =>
    assert(node_side == 1);
    assert(x == 0 && y == 0);
    n;
  | Quad(nw, ne, se, sw) =>
    let sub_side = node_side / 2;
    if (x < sub_side) {
      if (y < sub_side) {
        /* NW */
        at'(nw, sub_side, x, y);
      } else {
        /* SW */
        at'(sw, sub_side, x, y - sub_side);
      };
    } else if (y < sub_side) {
      /* NE */
      at'(ne, sub_side, x - sub_side, y);
    } else {
      /* SE */
      at'(se, sub_side, x - sub_side, y - sub_side);
    };
  };
let at = (grid, x, y) => {
  assert_within(grid.side, x, y);
  at'(grid.root, grid.side, x, y);
};

let rec update' = (node, node_side, x, y, f) =>
  switch (node) {
  | Tile(n) =>
    assert(node_side == 1);
    assert(x == 0 && y == 0);
    Tile(f(n));
  | Quad(nw, ne, se, sw) =>
    let sub_side = node_side / 2;
    if (x < sub_side) {
      if (y < sub_side) {
        /* NW */
        let nw = update'(nw, sub_side, x, y, f);
        Quad(nw, ne, se, sw);
      } else {
        /* SW */
        let sw = update'(sw, sub_side, x, y - sub_side, f);
        Quad(nw, ne, se, sw);
      };
    } else if (y < sub_side) {
      /* NE */
      let ne = update'(ne, sub_side, x - sub_side, y, f);
      Quad(nw, ne, se, sw);
    } else {
      /* SE */
      let se = update'(se, sub_side, x - sub_side, y - sub_side, f);
      Quad(nw, ne, se, sw);
    };
  };
let update = (grid, x, y, f) => {
  assert_within(grid.side, x, y);
  update'(grid.root, grid.side, x, y, f);
};

let put = (grid, x, y, n) => update(grid, x, y, _ => n);

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

let rec fold' = (node, node_side, node_x, node_y, acc, f) =>
  switch (node) {
  | Tile(n) =>
    assert(node_side == 1);
    f(acc, node_x, node_y, n);
  | Quad(nw, ne, se, sw) =>
    let sub_side = node_side / 2;
    let acc = fold'(nw, sub_side, node_x, node_y, acc, f);
    let acc = fold'(ne, sub_side, node_x + sub_side, node_y, acc, f);
    let acc =
      fold'(se, sub_side, node_x + sub_side, node_y + sub_side, acc, f);
    let acc = fold'(sw, sub_side, node_x, node_y + sub_side, acc, f);
    acc;
  };
let fold = (grid, acc, f) => {
  fold'(grid.root, grid.side, 0, 0, acc, f);
};

let rec map' = (node, node_side, node_x, node_y, f) =>
  switch (node) {
  | Tile(n) =>
    assert(node_side == 1);
    Tile(f(node_x, node_y, n));
  | Quad(nw, ne, se, sw) =>
    let sub_side = node_side / 2;
    let nw = map'(nw, sub_side, node_x, node_y, f);
    let ne = map'(ne, sub_side, node_x + sub_side, node_y, f);
    let se = map'(se, sub_side, node_x + sub_side, node_y + sub_side, f);
    let sw = map'(sw, sub_side, node_x, node_y + sub_side, f);
    Quad(nw, ne, se, sw);
  };
let map = (grid, f) => {
  map'(grid.root, grid.side, 0, 0, f);
};

let filter_map = (grid, f) =>
  fold(grid, [], (acc, x, y, n) =>
    switch (f(x, y, n)) {
    | Some(v) => [v, ...acc]
    | None => acc
    }
  );

let iter = (grid, f) =>
  fold(
    grid,
    (),
    ((), x, y, n) => {
      f(x, y, n);
      ();
    },
  );

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

let debug_print = grid =>
  scan_iter(
    grid,
    ~row_f=_y => print_newline(),
    (_x, _y, n) => print_int(n),
  );

/*
  Directions and neighbors
 */

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