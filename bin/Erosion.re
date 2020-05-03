type tile = int;

let colorize = Heightmap.colorize;

let compare_elevations = (a, b) => {
  let (ae, _, _) = a;
  let (be, _, _) = b;
  Int.compare(ae, be);
};

/** fall_to determines which neighbor the raindrop will move to */
let fall_to = (here, neighbors) => {
  Array.fast_sort(compare_elevations, neighbors);
  let (le, lx, ly) = neighbors[0];
  if (le < here) {
    Some((lx, ly));
  } else {
    None;
  };
};

let rec raindrop' = (grid: Grid.t(tile), x: int, y: int): unit => {
  let here = Grid.at(grid, x, y);
  let neighbors = Grid.neighbors_xy(grid, x, y);
  switch (fall_to(here, neighbors)) {
  | Some((dx, dy)) =>
    let (x', y') = Grid.wrap_coord(grid.width, grid.height, x + dx, y + dy);
    /* Fall to a neighbor */
    raindrop'(grid, x', y');
  | None =>
    /* Deposit here */
    Grid.put(grid, x, y, here + 1)
  };
};

/**
  raindrop starts at a given coordinate on the heightmap grid, takes
  sediment, then moves to lower and lower neighbors until it reaches a local
  minimum, where it deposits sediment (increases height by 1).
 */
let raindrop = (grid, x, y) => {
  let here = Grid.at(grid, x, y);
  Grid.put(grid, x, y, here - 1);
  raindrop'(grid, x, y);
};

let random_raindrops = (heightmap: Grid.t(tile)) => {
  let amount = heightmap.width * heightmap.height;
  for (_ in 1 to amount) {
    let start_x = Random.int(heightmap.width);
    let start_y = Random.int(heightmap.height);
    raindrop(heightmap, start_x, start_y);
  };
  heightmap;
};

let phase = Phase_chain.(repeat(10, random_raindrops(_)));