type building = unit;

type town = {
  x: int,
  z: int,
  town: Town_prototype.output,
};

type t = list(town);

type obstacles = Grid.t(bool);

let tweak_dist = 150;
let tweak_tries = 100;
let num_towns = 8;
let min_dist_between_towns = 500;
let potential_sites_limit = 100;

let calc_obstacles = (base: Base_overlay.t, road: Road_overlay.t): obstacles => {
  Grid.map(base, (x, z, base) => {
    switch (base) {
    | {river: true, _}
    | {ocean: true, _} => true
    | _ when Sparse_grid.at(road.roads, x, z) |> Option.is_some => true
    | {river: false, ocean: false, _} => false
    }
  });
};

let within_region_boundaries = (x, z) =>
  Minecraft.Block_tree.(
    x
    mod block_per_region < block_per_region
    - Town_prototype.side
    && z
    mod block_per_region < block_per_region
    - Town_prototype.side
  );

let has_obstacle = (obstacles: obstacles, x, z) =>
  try(
    Range.exists(z, z + Town_prototype.side - 1, z =>
      Range.exists(x, x + Town_prototype.side - 1, x =>
        Grid.at(obstacles, x, z)
      )
    )
  ) {
  | Invalid_argument(_) => true
  };

let acceptable_elevations = (base: Base_overlay.t, x, z) => {
  let start_elev = Grid.at(base, x, z).elevation;
  let (emin, emax) =
    Range.fold(
      z,
      z + Town_prototype.side - 1,
      (start_elev, start_elev),
      ((emin, emax), z) =>
      Range.fold(
        x,
        x + Town_prototype.side - 1,
        (emin, emax),
        ((emin, emax), x) => {
          let here_elev = Grid.at(base, x, z).elevation;
          let emin = min(emin, here_elev);
          let emax = max(emax, here_elev);
          (emin, emax);
        },
      )
    );
  emax - emin <= Town_prototype.elevation_range;
};

let town_area_clear = (base: Base_overlay.t, obstacles: obstacles, x, z) =>
  if (!within_region_boundaries(x, z)) {
    print_endline("region boundaries");
    false;
  } else if (has_obstacle(obstacles, x, z)) {
    print_endline("has obstacle");
    false;
  } else if (!acceptable_elevations(base, x, z)) {
    print_endline("elevation");
    false;
  } else {
    true;
  };

let rec tweak_town_area = (base, obstacles, x, z, tries) =>
  if (tries > 0) {
    let try_x = x + Random.int(tweak_dist * 2) - tweak_dist;
    let try_z = z + Random.int(tweak_dist * 2) - tweak_dist;
    if (town_area_clear(base, obstacles, try_x, try_z)) {
      Some((try_x, try_z));
    } else {
      tweak_town_area(base, obstacles, x, z, tries - 1);
    };
  } else {
    None;
  };
let tweak_town_area = (base, obstacles, x, z) =>
  if (town_area_clear(base, obstacles, x, z)) {
    Some((x, z));
  } else {
    tweak_town_area(base, obstacles, x, z, tweak_tries);
  };

let rec first_suitable_towns =
        (
          base: Base_overlay.t,
          obstacles: obstacles,
          remaining,
          coords,
          selected,
        ) => {
  switch (coords) {
  | _ when remaining <= 0 => selected
  | [] => selected
  | [(x, z) as untweaked_coord, ...coords] =>
    let too_close =
      List.exists(
        other_coord =>
          Mg_util.distance_int(untweaked_coord, other_coord)
          < min_dist_between_towns,
        selected,
      );
    if (!too_close) {
      switch (tweak_town_area(base, obstacles, x, z)) {
      | Some(coord) =>
        print_endline("Selected town");
        let selected = [coord, ...selected];
        first_suitable_towns(
          base,
          obstacles,
          remaining - 1,
          coords,
          selected,
        );
      | None =>
        print_endline("Failed to tweak town");
        first_suitable_towns(base, obstacles, remaining, coords, selected);
      };
    } else {
      /* Too close to another town */
      print_endline("Town too close");
      first_suitable_towns(base, obstacles, remaining, coords, selected);
    };
  };
};

let prepare_town = (base: Base_overlay.t, x, z) => {
  /* Slice elevations from base overlay */
  let elevation =
    Grid.init(Town_prototype.side, (town_x, town_z) => {
      Grid.at(base, town_x + x, town_z + z).elevation
    });

  /* TODO do we actually need obstacles? */
  let obstacles = Sparse_grid.make(Town_prototype.side);

  let town = Town_prototype.run({elevation, obstacles});

  {x, z, town};
};

let prepare = (base: Base_overlay.t, roads: Road_overlay.t, ()): t => {
  /* Shuffle a list of all river tiles */
  print_endline("Finding river coords");
  let river_coords =
    Grid.filter_map(base, (x, z, base) => {
      switch (base) {
      | {river: true, _} =>
        Some((x - Town_prototype.side / 2, z - Town_prototype.side / 2))
      | {river: false, _} => None
      }
    });
  print_endline("Shuffling river coords");
  let river_coords =
    Mg_util.shuffle(river_coords) |> Mg_util.take(potential_sites_limit);
  /* Pick and tweak town sites from this list */
  print_endline("Calculating obstacles");
  let obstacles = calc_obstacles(base, roads);
  print_endline("Finding suitable towns");
  let towns =
    first_suitable_towns(base, obstacles, num_towns, river_coords, []);
  List.iter(((x, z)) => Printf.printf("town at %d, %d\n", x, z), towns);
  List.map(((x, z)) => prepare_town(base, x, z), towns);
};

let apply_region = (towns: t, args) => {
  let Minecraft_converter.{
        region,
        rx: _,
        rz: _,
        gx_offset,
        gy_offset,
        gsize: _,
      } = args;
  List.iter(
    ({x, z, town: {adjusted_elevation, farms, houses, plazas}}) => {
      let x = x - gx_offset;
      let z = z - gy_offset;
      if (0 <= x
          && x < Minecraft.Block_tree.block_per_region
          && 0 <= z
          && z < Minecraft.Block_tree.block_per_region) {
        /* Apply elevation changes */
        Grid.iter(adjusted_elevation, (town_x, town_z, target_elev) => {
          Building.raise_lower_elev(args, x + town_x, z + town_z, target_elev)
        });

        /* Place wool on town blocks */
        let place_wool = (wool, block) => {
          let Town_prototype.{min_x, max_x, min_z, max_z, _} = block;
          let min_x = min_x + x;
          let max_x = max_x + x;
          let min_z = min_z + z;
          let max_z = max_z + z;
          for (z in min_z to max_z) {
            for (x in min_x to max_x) {
              let y = 1 + Minecraft.Block_tree.height_at(region, ~x, ~z, ());
              Minecraft.Block_tree.set_block(region, x, y, z, wool);
            };
          };
        };

        List.iter(place_wool(Minecraft.Block.Wool), farms);
        List.iter(place_wool(Minecraft.Block.Stonebrick), plazas);
        List.iter(place_wool(Minecraft.Block.Brick_block), houses);
      };
    },
    towns,
  );
};

let overlay = (base, roads) =>
  Overlay.make("towns", prepare(base, roads), apply_region);