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
  let ({elevation: ae}, _, _) = a;
  let ({elevation: be}, _, _) = b;
  Int.compare(ae, be);
};

/** fall_to determines which neighbor the river will flow to */
let fall_to = (here, neighbors) => {
  Array.fast_sort(compare_elevations, neighbors);
  let (_, lx, ly) as l = neighbors[0];
  if (compare_elevations(l, (here, 0, 0)) < 0) {
    Some((lx, ly));
  } else {
    None;
  };
};

/** river_sources gets all potential river sources on the map, in random order */
let river_sources = (grid: Grid.t(tile)) => {
  let result = ref([]);
  for (y in 0 to pred(grid.height)) {
    for (x in 0 to pred(grid.width)) {
      let here = Grid.at(grid, x, y);
      if (!here.ocean && 10 < here.elevation && here.elevation < 30) {
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

/**
  flow_river moves downhill until the river reaches the ocean or a local
  minimum. If the river reaches a local minimum before reaching the ocean, it
  is abandoned. If the river reaches an ocean, it returns the path it took.
  No tiles are modified.
 */
let rec flow_river = (grid, river, path, x, y) => {
  let here = Grid.at(grid, x, y);
  if (!here.ocean) {
    let path = [(x, y), ...path];
    let neighbors = Grid.neighbors_xy(grid, x, y);
    switch (fall_to(here, neighbors)) {
    | Some((dx, dy)) =>
      let (next_x, next_y) =
        Grid.wrap_coord(grid.width, grid.height, x + dx, y + dy);
      flow_river(grid, river, path, next_x, next_y);
    | None => None
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
  for (id in 0 to pred(amount)) {
    let (source_x, source_y) = sources[id];
    if (river(grid, id, source_x, source_y)) {
      succeeded := succeeded^ + 1;
    };
  };
  Printf.printf("Successfully placed %d of %d rivers\n", succeeded^, amount);
  grid;
};

let phase = Phase_chain.(convert(_) @> add_rivers(_, 100) @> finish);