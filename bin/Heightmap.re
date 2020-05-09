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

let spread_distances = grid =>
  until_changes_stop(
    grid,
    (here, neighbors) => {
      let ocean_distances = Array.map(x => x.distance_to_ocean, neighbors);
      Array.fast_sort(Int.compare, ocean_distances);
      let distance_to_ocean =
        min(here.distance_to_ocean, ocean_distances[0] + 1);

      let mountain_distances =
        Array.map(x => x.distance_to_mountain, neighbors);
      Array.fast_sort(Int.compare, mountain_distances);
      let distance_to_mountain =
        min(here.distance_to_mountain, mountain_distances[0] + 1);

      if (distance_to_ocean != here.distance_to_ocean
          || distance_to_mountain != here.distance_to_mountain) {
        Some({...here, distance_to_ocean, distance_to_mountain});
      } else {
        None;
      };
    },
  );

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