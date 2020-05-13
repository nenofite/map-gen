type t('a) = {
  width: int,
  height: int,
  data: array('a),
};

let assert_within = (width, height, x, y) =>
  if (x < 0 || x >= width || y < 0 || y >= height) {
    raise(Invalid_argument("Coordinate out of bounds"));
  };

let i_of_xy = (width, height, x, y) => {
  assert_within(width, height, x, y);
  y * width + x;
};

let xy_of_i = (width, _height, i) => {
  let y = i / width;
  let x = i mod width;
  (x, y);
};

let wrap_coord = (width, height, x, y) => {
  let x' =
    switch (x mod width) {
    | x when x < 0 => x + width
    | x => x
    };
  let y' =
    switch (y mod height) {
    | y when y < 0 => y + height
    | y => y
    };
  (x', y');
};

let wrap = (grid, x, y) => wrap_coord(grid.width, grid.height, x, y);

let make = (width, height, default) => {
  width,
  height,
  data: Array.make(width * height, default),
};

let init = (width, height, f) => {
  width,
  height,
  data:
    Array.init(
      width * height,
      i => {
        let (x, y) = xy_of_i(width, height, i);
        f(x, y);
      },
    ),
};

let at = (grid, x, y) => {
  assert_within(grid.width, grid.height, x, y);
  grid.data[i_of_xy(grid.width, grid.height, x, y)];
};

/**
  at' wraps the coord and then calls at
 */
let at' = (grid, x, y) => {
  let (x, y) = wrap_coord(grid.width, grid.height, x, y);
  at(grid, x, y);
};

let put = (grid, x, y, v) => {
  assert_within(grid.width, grid.height, x, y);
  grid.data[i_of_xy(grid.width, grid.height, x, y)] = v;
};

/**
  put' wraps the coord and then calls put
 */
let put' = (grid, x, y, v) => {
  let (x, y) = wrap_coord(grid.width, grid.height, x, y);
  put(grid, x, y, v);
};

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
  [|
    at'(grid, x - 1, y - 1),
    at'(grid, x, y - 1),
    at'(grid, x + 1, y - 1),
    at'(grid, x - 1, y),
    at'(grid, x + 1, y),
    at'(grid, x - 1, y + 1),
    at'(grid, x, y + 1),
    at'(grid, x + 1, y + 1),
  |];
};

/** neighbors_xy returns the 8 neighbors paired with their relative offset, starting with northwest and going clockwise */
let neighbors_xy = (grid, x, y) => {
  [|
    (at'(grid, x - 1, y - 1), (-1), (-1)),
    (at'(grid, x, y - 1), 0, (-1)),
    (at'(grid, x + 1, y - 1), 1, (-1)),
    (at'(grid, x - 1, y), (-1), 0),
    (at'(grid, x + 1, y), 1, 0),
    (at'(grid, x - 1, y + 1), (-1), 1),
    (at'(grid, x, y + 1), 0, 1),
    (at'(grid, x + 1, y + 1), 1, 1),
  |];
};

let rec fold_xy' = (grid, acc, f, x, y) =>
  if (y >= grid.height) {
    acc;
  } else if (x >= grid.width) {
    fold_xy'(grid, acc, f, 0, y + 1);
  } else {
    let here = at(grid, x, y);
    let acc = f(acc, x, y, here);
    fold_xy'(grid, acc, f, x + 1, y);
  };
let fold_xy = (grid, acc, f) => fold_xy'(grid, acc, f, 0, 0);

let filter_map_xy = (grid, f) => {
  fold_xy(grid, [], (acc, x, y, here) => {
    switch (f(x, y, here)) {
    | Some(n) => [n, ...acc]
    | None => acc
    }
  });
};