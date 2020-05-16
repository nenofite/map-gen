type tile = {
  elevation: int,
  river: bool,
  ocean: bool,
};

let min_river_length = 30;

let colorize = (tile: tile): int => {
  let base = Heightmap.colorize(tile.elevation) |> Color.color_of_int(_);
  let blue = Color.color_of_int(0x0000FF);
  if (tile.ocean || tile.river) {
    Color.blend(base, blue, 0.5) |> Color.int_of_color(_);
  } else {
    base |> Color.int_of_color(_);
  };
};

let convert = (old_grid: Grid.t(int)) => {
  Grid.map(
    old_grid,
    (_x, _y, elevation) => {
      let ocean = elevation <= 0;
      {elevation, ocean, river: false};
    },
  );
};

let compare_elevations = (a, b) => {
  let ({elevation: ae, _}, _, _) = a;
  let ({elevation: be, _}, _, _) = b;
  Int.compare(ae, be);
};

/** fall_to determines which neighbor the river will flow to */
let fall_to = (here, neighbors) => {
  let neighbors = List.fast_sort(compare_elevations, neighbors);
  let (_, lx, ly) as l = List.hd(neighbors);
  let cmp = compare_elevations(l, (here, 0, 0));
  if (cmp < 0) {
    Some((lx, ly));
  } else {
    None;
  };
};

let rec array_find = (f, array, i) =>
  if (i < Array.length(array)) {
    if (f(array[i])) {
      Some(array[i]);
    } else {
      array_find(f, array, i + 1);
    };
  } else {
    None;
  };

let shuffle = list => {
  List.(
    map(x => (Random.bits(), x), list)
    |> fast_sort(((a, _), (b, _)) => Int.compare(a, b), _)
    |> map(((_, x)) => x, _)
  );
};

/**
  fall_to_with_flat_dir determines which neighbor the river will flow to:

  1. If there is lower land, move in that direction.
  2. Otherwise, if [flat_dir] is level with us, move in that direction.
  3. Otherwise, pick a random flat direction.
 */
let fall_to_with_flat_dir = (here, neighbors, flat_dir) => {
  let (flat_dir_x, flat_dir_y) = flat_dir;
  assert(flat_dir_x != 0 || flat_dir_y != 0);
  let flat_dir_n =
    List.find(((_, x, y)) => x == flat_dir_x && y == flat_dir_y, neighbors);

  let neighbors = List.fast_sort(compare_elevations, neighbors);
  let (_, lowest_x, lowest_y) as lowest = List.hd(neighbors);

  if (compare_elevations(lowest, (here, 0, 0)) < 0) {
    Some((lowest_x, lowest_y));
  } else if (compare_elevations(flat_dir_n, (here, 0, 0)) <= 0) {
    Some(flat_dir);
  } else {
    shuffle(neighbors)
    |> List.find_opt(x => compare_elevations(x, (here, 0, 0)) <= 0, _)
    |> Option.map(((_, x, y)) => (x, y), _);
  };
};

/** river_sources gets all potential river sources on the map, in random order */
let river_sources = (grid: Grid.t(tile)) => {
  let coords =
    Grid.fold(grid, [], (acc, x, y, here) =>
      if (!here.ocean && 24 <= here.elevation && here.elevation <= 30) {
        let neighbors = Grid.neighbors_xy(grid, x, y);
        if (Option.is_some(fall_to(here, neighbors))) {
          [(x, y, Random.bits()), ...acc];
        } else {
          acc;
        };
      } else {
        acc;
      }
    );
  let cmp = ((_, _, a), (_, _, b)) => Int.compare(a, b);
  List.fast_sort(cmp, coords) |> List.map(((x, y, _)) => (x, y), _);
};

/**
  place_river_tile modifies the given tile to have a river on it. If there's
  already a river there, it raises [Invalid_argument]
 */
let place_river_tile = (grid, x, y) => {
  Grid.update(grid, x, y, here =>
    switch (here) {
    | {river: true, _} => raise(Invalid_argument("Tile already has river"))
    | {river: false, _} as here => {...here, river: true}
    }
  );
};

/**
  deposit_sediment increases the elevation by 1 at the given coordinate.
 */
let deposit_sediment = (grid, x, y) => {
  Grid.update(grid, x, y, here => {...here, elevation: here.elevation + 1});
};

/**
  current_flow_direction gets the river's current direction given path and
  current coordinate. If the path is empty, this defaults to south.
 */
let current_flow_direction = (path, x, y) => {
  switch (path) {
  | [(px, py), ..._] => (x - px, y - py)
  | [] => (0, 1)
  };
};

/**
  flow_river moves downhill until the river reaches the ocean, another river,
  or a local minimum. If the river reaches a local minimum before reaching
  the ocean, it deposits sediment there and tries again from the previous
  tile. If the river reaches an ocean or another river, it succeeds and
  returns the path it took.
 */
let rec flow_river = (grid, path, x, y) => {
  let here = Grid.at(grid, x, y);
  if (!here.ocean && !here.river) {
    let next_path = [(x, y), ...path];
    let neighbors = Grid.neighbors_xy(grid, x, y);
    let flat_dir = current_flow_direction(path, x, y);
    switch (fall_to_with_flat_dir(here, neighbors, flat_dir)) {
    | Some((dx, dy)) =>
      let (next_x, next_y) = Grid.wrap_coord(grid, x + dx, y + dy);
      assert(next_x != x || next_y != y);
      if (!List.exists(((lx, ly)) => lx == next_x && ly == next_y, path)) {
        let grid = place_river_tile(grid, x, y);
        flow_river(grid, next_path, next_x, next_y);
      } else {
        /* We formed a loop. Deposit sediment and backtrack. */
        let grid = deposit_sediment(grid, x, y);
        switch (path) {
        | [(previous_x, previous_y), ...previous_path] =>
          flow_river(grid, previous_path, previous_x, previous_y)
        | [] => flow_river(grid, [], x, y)
        };
      };
    | None =>
      /* We're stuck in a ditch. Deposit sediment and backtrack. */
      let grid = deposit_sediment(grid, x, y);
      switch (path) {
      | [(previous_x, previous_y), ...previous_path] =>
        flow_river(grid, previous_path, previous_x, previous_y)
      | [] => flow_river(grid, [], x, y)
      };
    };
  } else if (List.length(path) > min_river_length) {
    /* We've made a river! Only accept if it's long enough */
    Some(grid);
  } else {
    None;
        /* Too short */
  };
};

let rec take = (amount, list) =>
  switch (list) {
  | [a, ...b] when amount > 0 => [a, ...take(amount - 1, b)]
  | [_, ..._]
  | [] => []
  };

/**
  river finds a non-ocean tile with an elevation between plains and
  mountains, then creates a river with the given id there. The river is only
  kept if it can reach the ocean.
 */
let river = (grid: Grid.t(tile), _id: int, source_x: int, source_y: int) =>
  flow_river(grid, [], source_x, source_y);

let add_rivers = (grid, amount): Grid.t(tile) => {
  let sources = river_sources(grid) |> take(amount, _);
  let amount = min(amount, List.length(sources));

  let (grid, succeeded) =
    List.mapi((id, coord) => (id, coord), sources)
    |> List.fold_left(
         ((grid, succeeded), (id, (x, y))) => {
           switch (river(grid, id, x, y)) {
           | Some(grid) => (grid, succeeded + 1)
           | None => (grid, succeeded)
           }
         },
         (grid, 0),
         _,
       );
  Printf.printf("Successfully placed %d of %d rivers\n", succeeded, amount);
  grid;
};

let phase =
  Phase_chain.(
    phase("Convert to river", convert(_))
    @> phase("Flow rivers", add_rivers(_, 100))
  );