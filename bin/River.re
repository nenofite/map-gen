type tile = {
  elevation: int,
  river: int,
  ocean: bool,
};

let colorize = (tile: tile): int => {
  let base = Color.{r: 0, g: 0, b: 0};
  let blue = Color.color_of_int(0x0000FF);
  Color.blend(base, blue, min(float_of_int(tile.river) /. 200., 1.))
  |> Color.int_of_color(_);
};

let convert = (old_grid: Grid.t(int)) => {
  Grid.init(
    old_grid.width,
    old_grid.height,
    (x, y) => {
      let elevation = Grid.at(old_grid, x, y);
      let ocean = elevation <= 0;
      {elevation, ocean, river: 0};
    },
  );
};

let compare_elevations = (a, b) => {
  let ({elevation: ae, river: ar}, _, _) = a;
  let ({elevation: be, river: br}, _, _) = b;
  Int.compare(ae - ar, be - br);
};

/** fall_to determines which neighbor the raindrop will move to */
let fall_to = (here, neighbors) => {
  Array.fast_sort(compare_elevations, neighbors);
  let (_, lx, ly) as l = neighbors[0];
  if (compare_elevations(l, (here, 0, 0)) < 0) {
    Some((lx, ly));
  } else {
    None;
  };
};

/**
  See [raindrop]
*/
let rec raindrop' =
        (~grid: Grid.t(tile), ~sediment: int, x: int, y: int): unit => {
  let here = Grid.at(grid, x, y);
  /* Rule 1: if ocean, stop */
  if (!here.ocean) {
    let neighbors = Grid.neighbors_xy(grid, x, y);
    switch (fall_to(here, neighbors)) {
    | None =>
      if (sediment > 0) {
        /* Rule 2: try to deposit sediment, then re-run */
        let here = {...here, elevation: here.elevation + 1};
        let sediment = sediment - 1;
        Grid.put(grid, x, y, here);
        if (sediment > 0) {
          raindrop'(~grid, ~sediment, x, y);
        };
      }
    | Some((dx, dy)) =>
      let (next_x, next_y) =
        Grid.wrap_coord(grid.width, grid.height, x + dx, y + dy);
      if (here.river == 0) {
        /* Rule 3: erode and create a river */
        let here = {...here, elevation: here.elevation - 1, river: 1};
        let sediment = sediment + 1;
        Grid.put(grid, x, y, here);
        raindrop'(~grid, ~sediment, next_x, next_y);
      } else {
        /* Rule 4: add to river */
        let here = {...here, river: here.river + 1};
        Grid.put(grid, x, y, here);
        raindrop'(~grid, ~sediment, next_x, next_y);
      };
    };
  };
};

/**
  raindrop follows four rules, depending on the current tile:

  1. if ocean, stop
  2. if no lower neighbor, try to deposit sediment; if still have sediment, run again in the same tile (now it's elevated by 1)
  3. if land with a lower neighbor, erodes it and add one to [river], then move to the lowest neighbor
  4. if river with a lower neighbor, do not erode but still add one to [river], then move to the lowest neighbor
 */
let raindrop = (grid, x, y) => raindrop'(~grid, ~sediment=0, x, y);

let random_raindrops = (heightmap: Grid.t(tile)) => {
  let amount = heightmap.width * heightmap.height;
  for (_ in 1 to amount) {
    let start_x = Random.int(heightmap.width);
    let start_y = Random.int(heightmap.height);
    raindrop(heightmap, start_x, start_y);
  };
  heightmap;
};

let run_phase = (old_grid: Grid.t(Heightmap.tile)): Grid.t(tile) => {
  convert(old_grid) |> random_raindrops(_);
};

let phase =
  Phase_chain.(convert(_) @> repeat(50, random_raindrops(_)) @@> finish);