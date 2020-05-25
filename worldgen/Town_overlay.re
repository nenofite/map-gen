type town = (int, int);

type t = list(town);

type obstacles = Grid.t(bool);

let town_side = 100;
let max_elevation_range = 10;
let tweak_dist = 150;
let tweak_tries = 100;
let num_towns = 8;

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

let town_area_clear = (base: Base_overlay.t, obstacles: obstacles, x, z) => {
  let has_obstacle =
    Range.exists(z, z + town_side - 1, z =>
      Range.exists(x, x + town_side - 1, x => {Grid.at(obstacles, x, z)})
    );
  let start_elev = Grid.at(base, x, z).elevation;
  let (emin, emax) =
    Range.fold(
      z, z + town_side - 1, (start_elev, start_elev), ((emin, emax), z) =>
      Range.fold(
        x,
        x + town_side - 1,
        (emin, emax),
        ((emin, emax), x) => {
          let here_elev = Grid.at(base, x, z).elevation;
          let emin = min(emin, here_elev);
          let emax = max(emax, here_elev);
          (emin, emax);
        },
      )
    );
  let elev_acceptable = emax - emin <= max_elevation_range;
  let within_region =
    Minecraft.Block_tree.(
      x
      mod block_per_region < block_per_region
      - town_side
      && z
      mod block_per_region < block_per_region
      - town_side
    );
  if (has_obstacle) {
    print_endline("obstacle");
    false;
  } else if (!elev_acceptable) {
    print_endline("elevation");
    false;
  } else if (!within_region) {
    print_endline("region boundaries");
    false;
  } else {
    true;
  };
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
  | [(x, z), ...coords] =>
    switch (tweak_town_area(base, obstacles, x, z)) {
    | Some(coord) =>
      print_endline("Selected town");
      let selected = [coord, ...selected];
      first_suitable_towns(base, obstacles, remaining - 1, coords, selected);
    | None =>
      print_endline("Failed to tweak town");
      first_suitable_towns(base, obstacles, remaining, coords, selected);
    }
  };
};

let prepare = (base: Base_overlay.t, roads: Road_overlay.t, ()): t => {
  /* Shuffle a list of all river tiles */
  print_endline("Finding river coords");
  let river_coords =
    Grid.filter_map(base, (x, z, base) => {
      switch (base) {
      | {river: true, _} => Some((x - town_side / 2, z - town_side / 2))
      | {river: false, _} => None
      }
    });
  print_endline("Shuffling river coords");
  let river_coords = Util.shuffle(river_coords);
  /* Pick and tweak town sites from this list */
  print_endline("Calculating obstacles");
  let obstacles = calc_obstacles(base, roads);
  print_endline("Finding suitable towns");
  let towns =
    first_suitable_towns(base, obstacles, num_towns, river_coords, []);
  /* TODO make sure towns aren't too close to each other */
  List.iter(((x, z)) => Printf.printf("town at %d, %d\n", x, z), towns);
  towns;
};

let apply_region = (towns, args) => {
  let Minecraft_converter.{
        region: _,
        rx: _,
        rz: _,
        gx_offset,
        gy_offset,
        gsize,
      } = args;
  List.iter(
    ((x, z)) => {
      let x = x - gx_offset;
      let z = z - gy_offset;
      if (0 <= x && x < gsize && 0 <= z && z < gsize) {
        let rect =
          List.init(town_side, z => List.init(town_side, x => (x, z)))
          |> List.concat;
        Building.flatten_footprint(args, ~x, ~z, rect) |> ignore;
      };
    },
    towns,
  );
};

let overlay = (base, roads) =>
  Overlay.make("towns", prepare(base, roads), apply_region);