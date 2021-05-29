open Core_kernel;

type with_distances = {
  tectonic: Tectonic.tile,
  distance_to_ocean: int,
  distance_to_mountain: int,
};

let sea_level = 62;
let mountain_level = 120;
let max_slope_level = 100;

let precision_coef = 100;

let colorize = (tile: int): int => {
  let frac = float_of_int(tile / precision_coef) /. 200.;
  let frac = Float.(max(min(frac, 1.), 0.));
  let black = 0;
  let white = 0xFFFFFF;
  Color.blend(black, white, frac);
};

let empty_distance = Int.max_value - 10;

let empty_distances_of_tectonic = (tectonic: Grid.t(Tectonic.tile)) => {
  Grid.map_to_mut(tectonic, ~f=(~x as _, ~z as _, here) => {
    {
      tectonic: here,
      distance_to_ocean:
        switch (here) {
        | Ocean
        | Trench => 0
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

let elevation_of_distances = (~alloc_side, grid: Grid.Mut.t(with_distances)) => {
  Grid.Mut.init(
    0,
    ~alloc_side,
    ~side=Grid.Mut.side(grid),
    ~f=(~x, ~z) => {
      let e = precision_coef;
      let here = Grid.Mut.get(~x, ~z, grid);
      let {tectonic, distance_to_ocean, distance_to_mountain} = here;
      switch (tectonic) {
      | Trench => 1 * e + Random.int(10 * e)
      | Ocean => 30 * e + Random.int(10 * e)
      | Mountain => mountain_level * e + Random.int(10 * e)
      | Plain =>
        let distance_to_ocean =
          if (distance_to_ocean != empty_distance) {
            distance_to_ocean;
          } else {
            1;
          };
        if (distance_to_ocean <= 1) {
          /* shore lines level with the water */
          sea_level * e;
        } else {
          let variation = max_slope_level - sea_level;
          if (distance_to_mountain != empty_distance) {
            let fraction =
              float_of_int(distance_to_ocean)
              /. float_of_int(distance_to_ocean + distance_to_mountain);
            sea_level
            * e
            + int_of_float(fraction *. Float.of_int(variation * e));
          } else {
            sea_level * e + min(distance_to_ocean, variation) * e;
          };
        };
      };
    },
  );
};

let spread_mountain_into = (grid, updated_coords, x, z, new_distance) => {
  let side = Grid.Mut.side(grid);
  let x = x % side;
  let z = z % side;
  switch (Grid.Mut.get(grid, ~x, ~z)) {
  | {distance_to_ocean: 0, _} =>
    /* Mountain distances don't spread across ocean */
    (grid, updated_coords)
  | {distance_to_mountain, _} as here =>
    if (distance_to_mountain > new_distance) {
      /* This distance is shorter, so update */
      let here = {...here, distance_to_mountain: new_distance};
      Grid.Mut.set(~x, ~z, here, grid);
      let updated_coords = [(new_distance, (x, z)), ...updated_coords];
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

let spread_mountain = (grid, ~level as _, (x, z)) => {
  let here = Grid.Mut.get(grid, ~x, ~z);
  let next_distance = here.distance_to_mountain + 1;
  /* Spread in all 8 directions */
  List.fold(
    Grid_compat.eight_directions,
    ~init=(grid, []),
    ~f=((grid, updated_coords), (dx, dz)) =>
    spread_mountain_into(grid, updated_coords, x + dx, z + dz, next_distance)
  );
};

let spread_ocean_into = (grid, updated_coords, x, z, new_distance) => {
  let side = Grid.Mut.side(grid);
  let x = x % side;
  let z = z % side;
  switch (Grid.Mut.get(grid, ~x, ~z)) {
  | {distance_to_mountain: 0, _} =>
    /* Ocean distances don't spread across mountains */
    (grid, updated_coords)
  | {distance_to_ocean, _} as here =>
    if (distance_to_ocean > new_distance) {
      /* This distance is shorter, so update */
      let here = {...here, distance_to_ocean: new_distance};
      Grid.Mut.set(~x, ~z, here, grid);
      let updated_coords = [(new_distance, (x, z)), ...updated_coords];
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

let spread_ocean = (grid, ~level as _, (x, z)) => {
  let here = Grid.Mut.get(grid, ~x, ~z);
  let next_distance = here.distance_to_ocean + 1;
  /* Spread in all 8 directions */
  List.fold(
    Grid_compat.eight_directions,
    ~init=(grid, []),
    ~f=((grid, updated_coords), (dx, dz)) =>
    spread_ocean_into(grid, updated_coords, x + dx, z + dz, next_distance)
  );
};

let spread_distances = grid => {
  /* First spread mountain distances */
  /* Start the queue with all mountains (sources) */
  let mountains =
    Grid.Mut.fold(grid, ~init=[], ~f=(~x, ~z, acc, here) => {
      switch (here) {
      | {distance_to_mountain: 0, _} => [(x, z), ...acc]
      | _ => acc
      }
    });
  let needs_update = [(0, mountains)];
  ignore(
    Grid_flood.flood_gen(
      ~init=grid,
      ~live=needs_update,
      ~spread=spread_mountain,
    ): Grid.Mut.t(_),
  );
  /* Then spread ocean distances */
  /* Start the queue with all oceans (sources) */
  let oceans =
    Grid.Mut.fold(grid, ~init=[], ~f=(~x, ~z, acc, here) => {
      switch (here) {
      | {distance_to_ocean: 0, _} => [(x, z), ...acc]
      | _ => acc
      }
    });
  let needs_update = [(0, oceans)];
  ignore(
    Grid_flood.flood_gen(~init=grid, ~live=needs_update, ~spread=spread_ocean):
                                                                    Grid.Mut.t(
                                                                    _,
                                                                    ),
  );
};

let fill_weighted = (a, b, c, d) => {
  let elevations = [|a, b, c, d|];
  Array.sort(~compare=Int.compare, elevations);
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
  Array.sort(~compare=Int.compare, elevations);
  let between = Random.float(1.0);
  let new_elevation =
    elevations[0]
    + int_of_float(float_of_int(elevations[3] - elevations[0]) *. between);
  new_elevation + Random.int(2);
};

let fill_weighted_avg = (a, b, c, d) => {
  let elevations = [|a, b, c, d|];
  Array.sort(~compare=Int.compare, elevations);
  let lo_i = Random.int(3);
  let hi_i = lo_i + 1;
  let between = Random.float(1.0);
  let new_elevation =
    elevations[lo_i]
    + int_of_float(
        float_of_int(elevations[hi_i] - elevations[lo_i]) *. between,
      );
  new_elevation + Random.int(2);
};

let phase = tectonic =>
  Tale.block("Heightmap", ~f=() => {
    let distances_grid = empty_distances_of_tectonic(tectonic);
    spread_distances(distances_grid);
    let elevation_grid =
      elevation_of_distances(
        ~alloc_side=Grid.Mut.side(distances_grid) * Int.(2 ** 5),
        distances_grid,
      );
    let points =
      Point_cloud.of_grid_mut(elevation_grid, ~spacing=4)
      |> Point_cloud.subdivide_interpolate4(~spacing=2)
      |> Point_cloud.subdivide_interpolate4(~spacing=2)
      |> Point_cloud.subdivide(~spacing=4);
    Grid.Mut.raw_set_side(elevation_grid, ~side=Point_cloud.side(points));
    Grid.Mut.map(elevation_grid, ~f=(~x, ~z, _) =>
      Point_cloud.nearest_int(points, x, z)
    )
    |> ignore;
    Subdivide_mut.overwrite_subdivide_with_fill(
      ~fill=Fill.random_avg,
      elevation_grid,
    );
    for (_ in 1 to 2) {
      Subdivide_mut.overwrite_subdivide_with_fill(
        ~fill=Fill.avg,
        elevation_grid,
      );
    };
    elevation_grid;
  });
