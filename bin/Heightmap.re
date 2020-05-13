type tile = int;

type intermediate = {
  tectonic: Tectonic.tile,
  distance_to_ocean: int,
  distance_to_mountain: int,
};

let colorize = (tile: tile): int => (tile + 50) * 255 / 150 * 0x010101;

let empty_distance = Int.max_int - 10;

let convert = (tectonic: Grid.t(Tectonic.tile)) => {
  Grid.init(
    tectonic.width,
    tectonic.height,
    (x, y) => {
      let here = Grid.at(tectonic, x, y);
      {
        tectonic: here,
        distance_to_ocean:
          switch (here) {
          | Ocean => 0
          | _ => empty_distance
          },
        distance_to_mountain:
          switch (here) {
          | Mountain => 0
          | _ => empty_distance
          },
      };
    },
  );
};

let convert_intermediate = (grid: Grid.t(intermediate)) => {
  Grid.init(
    grid.width,
    grid.height,
    (x, y) => {
      let {tectonic, distance_to_ocean, distance_to_mountain} =
        Grid.at(grid, x, y);
      switch (tectonic) {
      | Ocean => (-30) + Random.int(10)
      | Mountain => 30 + Random.int(70)
      | Plain =>
        assert(distance_to_ocean != empty_distance);
        if (distance_to_mountain != empty_distance) {
          let fraction =
            float_of_int(distance_to_ocean)
            /. float_of_int(distance_to_ocean + distance_to_mountain);
          1 + int_of_float(fraction *. 29.0);
        } else {
          min(distance_to_ocean, 30);
        };
      };
    },
  );
};

let until_changes_stop = (grid: Grid.t(intermediate), automata) => {
  let grid = ref(grid);
  let changed = ref(true);
  while (changed^) {
    changed := false;
    let old_grid = grid^;
    grid :=
      Grid.init(
        old_grid.width,
        old_grid.height,
        (x, y) => {
          let here = Grid.at(old_grid, x, y);
          let neighbors = Grid.neighbors(old_grid, x, y);
          switch (automata(here, neighbors)) {
          | Some(new_here) =>
            changed := true;
            new_here;
          | None => here
          };
        },
      );
  };
  grid^;
};

let spread_mountain_into = (grid, x, y, new_distance) => {
  let (x, y) = Grid.wrap(grid, x, y);
  switch (Grid.at(grid, x, y)) {
  | {distance_to_ocean: 0, _} =>
    /* Mountain distances don't spread across ocean */
    None
  | {distance_to_mountain, _} as here =>
    if (distance_to_mountain > new_distance) {
      /* This distance is shorter, so update */
      let here = {...here, distance_to_mountain: new_distance};
      Grid.put(grid, x, y, here);
      Some((new_distance, x, y));
    } else {
      None;
          /* This tile already has a shorter distance, so ignore */
    }
  };
};

let spread_mountain = (grid, x, y) => {
  let here = Grid.at(grid, x, y);
  let next_distance = here.distance_to_mountain + 1;
  /* Spread in all 8 directions */
  Grid.eight_directions
  |> List.filter_map(
       ((dx, dy)) =>
         spread_mountain_into(grid, x + dx, y + dy, next_distance),
       _,
     );
};

let spread_ocean_into = (grid, x, y, new_distance) => {
  let (x, y) = Grid.wrap(grid, x, y);
  switch (Grid.at(grid, x, y)) {
  | {distance_to_mountain: 0, _} =>
    /* Ocean distances don't spread across mountains */
    None
  | {distance_to_ocean, _} as here =>
    if (distance_to_ocean > new_distance) {
      /* This distance is shorter, so update */
      let here = {...here, distance_to_ocean: new_distance};
      Grid.put(grid, x, y, here);
      Some((new_distance, x, y));
    } else {
      None;
          /* This tile already has a shorter distance, so ignore */
    }
  };
};

let spread_ocean = (grid, x, y) => {
  let here = Grid.at(grid, x, y);
  let next_distance = here.distance_to_ocean + 1;
  /* Spread in all 8 directions */
  Grid.eight_directions
  |> List.filter_map(
       ((dx, dy)) => spread_ocean_into(grid, x + dx, y + dy, next_distance),
       _,
     );
};

let spread_distances = grid => {
  /* First spread mountain distances */
  /* Start the queue with all mountains (sources) */
  let mountains =
    Grid.filter_map_xy(grid, (x, y, here) =>
      switch (here) {
      | {distance_to_mountain: 0, _} => Some((x, y))
      | _ => None
      }
    );
  let needs_update = [(0, mountains)];
  Grid_flood.flood(grid, ~initial=needs_update, ~spread=spread_mountain);
  /* Then spread ocean distances */
  /* Start the queue with all oceans (sources) */
  let oceans =
    Grid.filter_map_xy(grid, (x, y, here) =>
      switch (here) {
      | {distance_to_ocean: 0, _} => Some((x, y))
      | _ => None
      }
    );
  let needs_update = [(0, oceans)];
  Grid_flood.flood(grid, ~initial=needs_update, ~spread=spread_ocean);

  grid;
};

let fill_weighted = (a, b, c, d) => {
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

let fill_avg = (a, b, c, d) => {
  let elevations = [|a, b, c, d|];
  Array.fast_sort(Int.compare, elevations);
  let between = Random.float(1.0);
  let new_elevation =
    elevations[0]
    + int_of_float(float_of_int(elevations[3] - elevations[0]) *. between);
  new_elevation + Random.int(2);
};

let phase =
  Phase_chain.(
    phase("Convert to heightmap", convert(_))
    @> phase("Spread distances", spread_distances(_))
    @> phase("Convert", convert_intermediate(_))
    /* @> Subdivide.subdivide_with_fill(_, fill_weighted) */
    @> phase_repeat(
         2,
         "Subdivide heightmap",
         Subdivide.subdivide_with_fill(_, fill_avg),
       )
  );