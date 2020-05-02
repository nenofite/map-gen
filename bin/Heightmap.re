type tile = int;

let colorize = (elevation: tile): int => (elevation + 100) * 0x010101;

let convert = (tectonic: Grid.t(Tectonic.tile)) => {
  Grid.init(
    tectonic.width,
    tectonic.height,
    (x, y) => {
      let here = Grid.at(tectonic, x, y);
      let (px, py) = Tectonic.xy_of_direction(here.direction);
      let toward = Grid.at'(tectonic, x + px, y + py);
      if (Tectonic.are_opposed(here.direction, toward.direction)) {
        /* Mountain */
        60 + Random.int(40);
      } else if (here.is_ocean) {
        /* Ocean */
        (-100) + Random.int(40);
      } else {
        /* Land */
        5 + Random.int(5);
      };
    },
  );
};

let fill = (a, b, c, d) => {
  let elevations = [|a, b, c, d|];
  Array.fast_sort(Int.compare, elevations);
  let index = Random.int(3);
  let between = Random.float(1.0);
  let new_elevation =
    elevations[index]
    + int_of_float(
        float_of_int(elevations[index + 1] - elevations[index]) *. between,
      );
  new_elevation;
};

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

/**
  raindrop starts at a given coordinate on the heightmap grid, then moves to lower
  and lower neighbors until it reaches a local minimum, where it deposits
  sediment (increases height by 1)
 */
let rec raindrop = (heightmap: Grid.t(tile), x: int, y: int): unit => {
  let here = Grid.at(heightmap, x, y);
  let neighbors = Grid.neighbors_xy(heightmap, x, y);
  switch (fall_to(here, neighbors)) {
  | Some((dx, dy)) =>
    let (x', y') =
      Grid.wrap_coord(heightmap.width, heightmap.height, x + dx, y + dy);
    raindrop(heightmap, x', y');
  | None => Grid.put(heightmap, x, y, here + 1)
  };
};

let random_raindrops = (heightmap: Grid.t(tile)): unit => {
  let amount = heightmap.width * heightmap.height * 20;
  for (_ in 1 to amount) {
    let start_x = Random.int(heightmap.width);
    let start_y = Random.int(heightmap.height);
    let start = Grid.at(heightmap, start_x, start_y);
    Grid.put(heightmap, start_x, start_y, start - 1);
    raindrop(heightmap, start_x, start_y);
  };
};

let run_phase = (tectonic: Grid.t(Tectonic.tile)): Grid.t(tile) => {
  let g =
    convert(tectonic)
    |> Util.times(Subdivide.subdivide_with_fill(_, fill), 2, _);
  random_raindrops(g);
  g;
};