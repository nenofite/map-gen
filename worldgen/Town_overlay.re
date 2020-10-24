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
let wall_height = 4;

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
  Minecraft.Region.(
    x
    mod block_per_region_side < block_per_region_side
    - Town_prototype.side
    && z
    mod block_per_region_side < block_per_region_side
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
  let roads = Sparse_grid.make(Town_prototype.side);

  let Town_prototype.{bell, houses, farms} =
    Town_prototype.run({elevation, roads});

  /* Translate blocks into global coords */
  let translate_block = (b: Town_prototype.block) => {
    ...b,
    min_x: b.min_x + x,
    max_x: b.max_x + x,
    min_z: b.min_z + z,
    max_z: b.max_z + z,
  };
  let bell = translate_block(bell);
  let houses =
    List.map(
      h => Town_prototype.{...h, block: translate_block(h.block)},
      houses,
    );
  let farms = List.map(translate_block, farms);

  {
    x,
    z,
    town: {
      bell,
      houses,
      farms,
    },
  };
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

let create_bell =
    (bell: Town_prototype.block, args: Minecraft_converter.region_args) => {
  open Minecraft.Region;
  let base_material = Minecraft.Block.Stone;

  let Town_prototype.{min_x, max_x, min_z, max_z, elevation: _, _} = bell;

  /* Foundation */
  for (x in min_x to max_x) {
    for (z in min_z to max_z) {
      Building.raise_lower_elev_match(args, x, z, bell.elevation);
      set_block(~x, ~y=bell.elevation, ~z, base_material, args.region);
    };
  };

  set_block(
    Minecraft.Block.Bell,
    ~x=(min_x + max_x) / 2,
    ~y=bell.elevation + 1,
    ~z=(min_z + max_z) / 2,
    args.region,
  );
};

let worksite_material = (worksite: Town_prototype.worksite) => {
  Minecraft.Block.(
    switch (worksite) {
    | Butcher => Smoker
    | Fisherman => Barrel
    | Shepherd => Loom
    }
  );
};

let create_house =
    (house: Town_prototype.house, args: Minecraft_converter.region_args) => {
  open Minecraft.Region;
  open Minecraft.Block;
  let floor_material = Oak_planks;
  let lower_edge_material = Cobblestone;
  let wall_material = Oak_planks;
  let wall_post_material = Oak_log(Y);
  let wall_ns_beam_material = Oak_log(Z);
  let wall_ew_beam_material = Oak_log(X);
  let ceiling_material = Oak_planks;

  let Town_prototype.{
        block: {min_x, max_x, min_z, max_z, elevation, _},
        worksite,
      } = house;

  /* Foundation and floor */
  for (x in min_x to max_x) {
    for (z in min_z to max_z) {
      Building.raise_lower_elev_match(args, x, z, elevation);
      set_block(~x, ~y=elevation, ~z, floor_material, args.region);
    };
  };

  /* Lower edge of walls */
  for (y in elevation to elevation + 1) {
    for (x in min_x to max_x) {
      set_block(~x, ~y, ~z=min_z, lower_edge_material, args.region);
      set_block(~x, ~y, ~z=max_z, lower_edge_material, args.region);
    };
    for (z in min_z to max_z) {
      set_block(~x=min_x, ~y, ~z, lower_edge_material, args.region);
      set_block(~x=max_x, ~y, ~z, lower_edge_material, args.region);
    };
  };

  /* Walls */
  for (y in elevation + 2 to elevation + wall_height) {
    for (x in min_x to max_x) {
      set_block(~x, ~y, ~z=min_z, wall_material, args.region);
      set_block(~x, ~y, ~z=max_z, wall_material, args.region);
    };
    for (z in min_z to max_z) {
      set_block(~x=min_x, ~y, ~z, wall_material, args.region);
      set_block(~x=max_x, ~y, ~z, wall_material, args.region);
    };
  };

  /* Wall beams */
  /* east-west */
  for (x in min_x + 1 to max_x - 1) {
    set_block(
      ~x,
      ~y=elevation + wall_height,
      ~z=min_z,
      wall_ew_beam_material,
      args.region,
    );
    set_block(
      ~x,
      ~y=elevation + wall_height,
      ~z=max_z,
      wall_ew_beam_material,
      args.region,
    );
  };
  /* north-south */
  for (z in min_z + 1 to max_z - 1) {
    set_block(
      ~x=min_x,
      ~y=elevation + wall_height,
      ~z,
      wall_ns_beam_material,
      args.region,
    );
    set_block(
      ~x=max_x,
      ~y=elevation + wall_height,
      ~z,
      wall_ns_beam_material,
      args.region,
    );
  };

  /* Wall posts - clockwise order */
  for (y in elevation to elevation + wall_height) {
    set_block(~x=min_x, ~y, ~z=min_z, wall_post_material, args.region);
    set_block(~x=max_x, ~y, ~z=min_z, wall_post_material, args.region);
    set_block(~x=max_x, ~y, ~z=max_z, wall_post_material, args.region);
    set_block(~x=min_x, ~y, ~z=max_z, wall_post_material, args.region);
  };

  /* Ceiling */
  for (x in min_x + 1 to max_x - 1) {
    for (z in min_z + 1 to max_z - 1) {
      set_block(
        ~x,
        ~y=elevation + wall_height,
        ~z,
        ceiling_material,
        args.region,
      );
    };
  };

  /* Torch on the inside center of each wall */
  /* N wall */
  set_block(
    Minecraft.Block.(Wall_torch(S)),
    ~x=(min_x + max_x) / 2,
    ~y=elevation + 2,
    ~z=min_z + 1,
    args.region,
  );
  /* E wall */
  set_block(
    Minecraft.Block.(Wall_torch(W)),
    ~x=max_x - 1,
    ~y=elevation + 2,
    ~z=(min_z + max_z) / 2,
    args.region,
  );
  /* S wall */
  set_block(
    Minecraft.Block.(Wall_torch(N)),
    ~x=(min_x + max_x) / 2,
    ~y=elevation + 2,
    ~z=max_z - 1,
    args.region,
  );
  /* W wall */
  set_block(
    Minecraft.Block.(Wall_torch(E)),
    ~x=min_x + 1,
    ~y=elevation + 2,
    ~z=(min_z + max_z) / 2,
    args.region,
  );

  /* Door on N wall */
  let door_x = min_x + 1;
  let door_z = min_z;
  let door_y = elevation + 1;
  set_block(
    Minecraft.Block.(Oak_door(S, Lower)),
    ~x=door_x,
    ~y=door_y,
    ~z=door_z,
    args.region,
  );
  set_block(
    Minecraft.Block.(Oak_door(S, Upper)),
    ~x=door_x,
    ~y=door_y + 1,
    ~z=door_z,
    args.region,
  );
  Building.raise_lower_elev_match(args, door_x, door_z - 1, elevation);

  /* Torch on outside wall next to door */
  set_block(
    Minecraft.Block.(Wall_torch(N)),
    ~x=min_x,
    ~y=elevation + 2,
    ~z=min_z - 1,
    args.region,
  );

  /* Bed in NE corner */
  let bed_x = max_x - 1;
  let bed_z = min_z + 1;
  let bed_y = elevation + 1;
  set_block(
    Minecraft.Block.(Orange_bed(E, Head)),
    ~x=bed_x,
    ~y=bed_y,
    ~z=bed_z,
    args.region,
  );
  set_block(
    Minecraft.Block.(Orange_bed(E, Foot)),
    ~x=bed_x - 1,
    ~y=bed_y,
    ~z=bed_z,
    args.region,
  );

  /* Only add worksite and villager when one is specified */
  switch (worksite) {
  | None => ()
  | Some(worksite) =>
    /* Worksite in SE corner */
    set_block(
      worksite_material(worksite),
      ~x=max_x - 1,
      ~y=elevation + 1,
      ~z=max_z - 1,
      args.region,
    );
    /* Villager at foot of bed */
    add_entity(
      Mg_util.Floats.(
        Minecraft.Entity.{
          id: "villager",
          x: ~.(bed_x - 2),
          y: ~.bed_y,
          z: ~.bed_z,
        }
      ),
      args.region,
    );
  };
};

let create_farm =
    (farm: Town_prototype.block, args: Minecraft_converter.region_args) => {
  open Minecraft.Region;
  open Minecraft.Block;

  let Town_prototype.{min_x, max_x, min_z, max_z, elevation, _} = farm;
  let elevation = elevation + 1;

  /* Foundation */
  for (x in min_x to max_x) {
    for (z in min_z to max_z) {
      Building.raise_lower_elev_match(args, x, z, elevation);
    };
  };

  /* Composter on NE corner */
  set_block(Composter, ~x=max_x, ~y=elevation + 1, ~z=min_z, args.region);

  /* Villager next to the composter */
  add_entity(
    Mg_util.Floats.(
      Minecraft.Entity.{
        id: "villager",
        x: ~.(max_x - 1),
        y: ~.(elevation + 3),
        z: ~.min_z,
      }
    ),
    args.region,
  );

  /* Alternate water and crops */
  let crop = Wheat(Random.int(8));
  for (x in min_x + 1 to max_x - 1) {
    for (z in min_z + 1 to max_z - 1) {
      if (z mod 2 == 0) {
        set_block(Water, ~x, ~y=elevation, ~z, args.region);
      } else {
        set_block(Farmland, ~x, ~y=elevation, ~z, args.region);
        set_block(crop, ~x, ~y=elevation + 1, ~z, args.region);
      };
    };
  };
};

let apply_region = (towns: t, args: Minecraft_converter.region_args) => {
  List.iter(
    ({x, z, town: {bell, farms, houses}}) =>
      if (Minecraft.Region.is_within(~x, ~y=0, ~z, args.region)) {
        create_bell(bell, args);
        List.iter(house => create_house(house, args), houses);
        List.iter(farm => create_farm(farm, args), farms);
      },
    towns,
  );
};

let overlay = (base, roads) =>
  Overlay.make("towns", prepare(base, roads), apply_region);