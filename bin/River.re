type river = {id: int};

type tile = {
  elevation: int,
  river: option(river),
  ocean: bool,
};

let colorize = (tile: tile): int => {
  let base = Heightmap.colorize(tile.elevation) |> Color.color_of_int(_);
  let blue = Color.color_of_int(0x0000FF);
  if (tile.ocean || Option.is_some(tile.river)) {
    Color.blend(base, blue, 0.5) |> Color.int_of_color(_);
  } else {
    base |> Color.int_of_color(_);
  };
};

let convert = (old_grid: Grid.t(int)) => {
  Grid.init(
    old_grid.width,
    old_grid.height,
    (x, y) => {
      let elevation = Grid.at(old_grid, x, y);
      let ocean = elevation <= 0;
      {elevation, ocean, river: None};
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
  Array.fast_sort(compare_elevations, neighbors);
  let (_, lx, ly) as l = neighbors[0];
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

let shuffle_copy = array => {
  let ordered = Array.map(x => (Random.bits(), x), array);
  Array.fast_sort(((a, _), (b, _)) => Int.compare(a, b), ordered);
  Array.map(((_, x)) => x, ordered);
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
    array_find(
      ((_, x, y)) => x == flat_dir_x && y == flat_dir_y,
      neighbors,
      0,
    )
    |> Option.get(_);

  Array.fast_sort(compare_elevations, neighbors);
  let (_, lowest_x, lowest_y) as lowest = neighbors[0];

  if (compare_elevations(lowest, (here, 0, 0)) < 0) {
    Some((lowest_x, lowest_y));
  } else if (compare_elevations(flat_dir_n, (here, 0, 0)) <= 0) {
    Some(flat_dir);
  } else {
    shuffle_copy(neighbors)
    |> array_find(x => compare_elevations(x, (here, 0, 0)) <= 0, _, 0)
    |> Option.map(((_, x, y)) => (x, y), _);
  };
};

/** river_sources gets all potential river sources on the map, in random order */
let river_sources = (grid: Grid.t(tile)) => {
  let result = ref([]);
  for (y in 0 to pred(grid.height)) {
    for (x in 0 to pred(grid.width)) {
      let here = Grid.at(grid, x, y);
      if (!here.ocean && 24 <= here.elevation && here.elevation <= 30) {
        let neighbors = Grid.neighbors_xy(grid, x, y);
        if (Option.is_some(fall_to(here, neighbors))) {
          result := [(x, y, Random.bits()), ...result^];
        };
      };
    };
  };
  let result = Array.of_list(result^);
  let cmp = ((_, _, a), (_, _, b)) => Int.compare(a, b);
  Array.fast_sort(cmp, result);
  Array.map(((x, y, _)) => (x, y), result);
};

/**
  place_river_tile modifies the given tile to have a river on it. If there's
  already a river there, it raises [Invalid_argument]
 */
let place_river_tile = (grid, river, x, y) => {
  let here = Grid.at(grid, x, y);
  if (Option.is_some(here.river)) {
    raise(Invalid_argument("Tile already has river"));
  };
  Grid.put(grid, x, y, {...here, river: Some(river)});
};

let debug_deposited_sediment_ = ref(0);

/**
  deposit_sediment increases the elevation by 1 at the given coordinate.
 */
let deposit_sediment = (grid, x, y) => {
  let here = Grid.at(grid, x, y);
  Grid.put(grid, x, y, {...here, elevation: here.elevation + 1});
  debug_deposited_sediment_ := debug_deposited_sediment_^ + 1;
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
let rec flow_river = (grid, river, path, x, y) => {
  let here = Grid.at(grid, x, y);
  if (!here.ocean && Option.is_none(here.river)) {
    let next_path = [(x, y), ...path];
    let neighbors = Grid.neighbors_xy(grid, x, y);
    let flat_dir = current_flow_direction(path, x, y);
    switch (fall_to_with_flat_dir(here, neighbors, flat_dir)) {
    | Some((dx, dy)) =>
      let (next_x, next_y) =
        Grid.wrap_coord(grid.width, grid.height, x + dx, y + dy);
      assert(next_x != x || next_y != y);
      if (!List.exists(((lx, ly)) => lx == next_x && ly == next_y, path)) {
        flow_river(grid, river, next_path, next_x, next_y);
      } else {
        /* We formed a loop. Deposit sediment and backtrack. */
        deposit_sediment(grid, x, y);
        switch (path) {
        | [(previous_x, previous_y), ...previous_path] =>
          flow_river(grid, river, previous_path, previous_x, previous_y)
        | [] => flow_river(grid, river, [], x, y)
        };
      };
    | None =>
      /* We're stuck in a ditch. Deposit sediment and backtrack. */
      deposit_sediment(grid, x, y);
      switch (path) {
      | [(previous_x, previous_y), ...previous_path] =>
        flow_river(grid, river, previous_path, previous_x, previous_y)
      | [] => flow_river(grid, river, [], x, y)
      };
    };
  } else {
    Some(path);
  };
};

/**
  river finds a non-ocean tile with an elevation between plains and
  mountains, then creates a river with the given id there. The river is only
  kept if it can reach the ocean.
 */
let river = (grid: Grid.t(tile), id: int, source_x: int, source_y: int): bool => {
  let river = {id: id};
  switch (flow_river(grid, river, [], source_x, source_y)) {
  | Some(path) =>
    List.iter(((x, y)) => place_river_tile(grid, river, x, y), path);
    true;
  | None => false
  };
};

let add_rivers = (grid, amount): Grid.t(tile) => {
  let sources = river_sources(grid);
  let amount = min(amount, Array.length(sources));
  let succeeded = ref(0);
  debug_deposited_sediment_ := 0;
  for (id in 0 to pred(amount)) {
    let (source_x, source_y) = sources[id];
    if (river(grid, id, source_x, source_y)) {
      succeeded := succeeded^ + 1;
    };
  };
  Printf.printf("Successfully placed %d of %d rivers\n", succeeded^, amount);
  Printf.printf("Deposited sediment %d times\n", debug_deposited_sediment_^);
  grid;
};

let phase =
  Phase_chain.(
    phase("Convert to river", convert(_))
    @> phase("Flow rivers", add_rivers(_, 200))
  );