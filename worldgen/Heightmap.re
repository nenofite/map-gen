type tile = int;

type intermediate = {
  tectonic: Tectonic.tile,
  distance_to_ocean: int,
  distance_to_mountain: int,
};

let colorize = (tile: tile): int => {
  let frac = (float_of_int(tile) +. 30.) /. 130.;
  let frac = max(min(frac, 1.), 0.);
  let black = Color.color_of_int(0);
  let white = Color.color_of_int(0xFFFFFF);
  Color.blend(black, white, frac) |> Color.int_of_color;
};

let empty_distance = Int.max_int - 10;

let convert = (tectonic: Grid.t(Tectonic.tile)) => {
  Grid.map(tectonic, (_x, _y, here) => {
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
    }
  });
};

let convert_intermediate = (grid: Grid.t(intermediate)) => {
  Grid.map(
    grid,
    (_x, _y, here) => {
      let {tectonic, distance_to_ocean, distance_to_mountain} = here;
      switch (tectonic) {
      | Ocean => (-30) + Random.int(10)
      | Mountain => 90 + Random.int(10)
      | Plain =>
        let distance_to_ocean =
          if (distance_to_ocean != empty_distance) {
            distance_to_ocean;
          } else {
            1;
          };
        if (distance_to_mountain != empty_distance) {
          let fraction =
            float_of_int(distance_to_ocean)
            /. float_of_int(distance_to_ocean + distance_to_mountain);
          1 + int_of_float(fraction *. 79.0);
        } else {
          min(distance_to_ocean, 80);
        };
      };
    },
  );
};

let spread_mountain_into = (grid, updated_coords, x, y, new_distance) => {
  let (x, y) = Grid.wrap_coord(grid, x, y);
  switch (Grid.at(grid, x, y)) {
  | {distance_to_ocean: 0, _} =>
    /* Mountain distances don't spread across ocean */
    (grid, updated_coords)
  | {distance_to_mountain, _} as here =>
    if (distance_to_mountain > new_distance) {
      /* This distance is shorter, so update */
      let here = {...here, distance_to_mountain: new_distance};
      let grid = Grid.put(grid, x, y, here);
      let updated_coords = [
        Grid_flood.{level: new_distance, x, y},
        ...updated_coords,
      ];
      (grid, updated_coords);
    } else {
      (
        /* This tile already has a shorter distance, so ignore */
        grid,
        updated_coords,
      );
    }
  };
};

let spread_mountain = (grid, x, y) => {
  let here = Grid.at(grid, x, y);
  let next_distance = here.distance_to_mountain + 1;
  /* Spread in all 8 directions */
  Grid.eight_directions
  |> List.fold_left(
       ((grid, updated_coords), (dx, dy)) =>
         spread_mountain_into(
           grid,
           updated_coords,
           x + dx,
           y + dy,
           next_distance,
         ),
       (grid, []),
       _,
     );
};

let spread_ocean_into = (grid, updated_coords, x, y, new_distance) => {
  let (x, y) = Grid.wrap_coord(grid, x, y);
  switch (Grid.at(grid, x, y)) {
  | {distance_to_mountain: 0, _} =>
    /* Ocean distances don't spread across mountains */
    (grid, updated_coords)
  | {distance_to_ocean, _} as here =>
    if (distance_to_ocean > new_distance) {
      /* This distance is shorter, so update */
      let here = {...here, distance_to_ocean: new_distance};
      let grid = Grid.put(grid, x, y, here);
      let updated_coords = [
        Grid_flood.{level: new_distance, x, y},
        ...updated_coords,
      ];
      (grid, updated_coords);
    } else {
      (
        /* This tile already has a shorter distance, so ignore */
        grid,
        updated_coords,
      );
    }
  };
};

let spread_ocean = (grid, x, y) => {
  let here = Grid.at(grid, x, y);
  let next_distance = here.distance_to_ocean + 1;
  /* Spread in all 8 directions */
  Grid.eight_directions
  |> List.fold_left(
       ((grid, updated_coords), (dx, dy)) =>
         spread_ocean_into(
           grid,
           updated_coords,
           x + dx,
           y + dy,
           next_distance,
         ),
       (grid, []),
       _,
     );
};

let spread_distances = grid => {
  /* First spread mountain distances */
  /* Start the queue with all mountains (sources) */
  let mountains =
    Grid.filter_map(grid, (x, y, here) =>
      switch (here) {
      | {distance_to_mountain: 0, _} => Some((x, y))
      | _ => None
      }
    );
  let needs_update = [(0, mountains)];
  let grid =
    Grid_flood.flood(grid, ~initial=needs_update, ~spread=spread_mountain);
  /* Then spread ocean distances */
  /* Start the queue with all oceans (sources) */
  let oceans =
    Grid.filter_map(grid, (x, y, here) =>
      switch (here) {
      | {distance_to_ocean: 0, _} => Some((x, y))
      | _ => None
      }
    );
  let needs_update = [(0, oceans)];
  let grid =
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