type river = {
  id: int,
  innate_direction: (int, int),
};

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
  flow_river adds the river to the given coordinate, then selects the next
  coordinate to flow into, carving it if necessary
 */
let rec flow_river = (grid, river, x, y, carve_elevation) => {
  let here = Grid.at(grid, x, y);
  if (!here.ocean && Option.is_none(here.river)) {
    let elevation = min(here.elevation, carve_elevation);
    let here = {...here, elevation, river: Some(river)};
    Grid.put(grid, x, y, here);

    let neighbors = Grid.neighbors_xy(grid, x, y);
    let (next_x, next_y) =
      switch (fall_to(here, neighbors)) {
      | Some((dx, dy)) =>
        Grid.wrap_coord(grid.width, grid.height, x + dx, y + dy)
      | None =>
        /* move in the innate direction and carve it to this elevation */
        let (dx, dy) = river.innate_direction;
        Grid.wrap_coord(grid.width, grid.height, x + dx, y + dy);
      };
    flow_river(grid, river, next_x, next_y, here.elevation);
  };
};

/**
  river finds a non-ocean tile with an elevation between plains and
  mountains, then creates a river with the given id there. It looks at the
  lowest surrounding neighbor to determine the river's innate direction, then
  flows the river.
 */
let river = (grid: Grid.t(tile), id: int, source_x: int, source_y: int): unit => {
  let here = Grid.at(grid, source_x, source_y);
  let neighbors = Grid.neighbors_xy(grid, source_x, source_y);
  let (fall_x, fall_y) = Option.get(fall_to(here, neighbors));
  let river = {id, innate_direction: (fall_x, fall_y)};
  flow_river(grid, river, source_x, source_y, here.elevation);
};

let add_rivers = (grid, amount): Grid.t(tile) => {
  let sources = river_sources(grid);
  let amount = min(amount, Array.length(sources));
  for (id in 0 to pred(amount)) {
    let (source_x, source_y) = sources[id];
    river(grid, id, source_x, source_y);
  };
  grid;
};

let phase = Phase_chain.(convert(_) @> add_rivers(_, 100) @> finish);