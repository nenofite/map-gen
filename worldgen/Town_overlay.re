open Bin_prot.Std;

[@deriving bin_io]
type town = {
  x: int,
  z: int,
  town: Town_prototype.output,
};

[@deriving bin_io]
type t = (list(town), Canonical_overlay.delta);

type x = list(town);

let tweak_dist = 150;
let tweak_tries = 100;
let num_towns = 8;
let min_dist_between_towns = 500;
let potential_sites_limit = 100;
let wall_height = 4;

let draw_towns = (canon: Canonical_overlay.t, towns) => {
  let sg =
    Sparse_grid.(
      List.fold_left(
        (sg, (x, z)) => put(sg, x, z, ()),
        make(canon.side),
        towns,
      )
    );
  Draw.draw_sparse_grid(
    fun
    | None => 0
    | Some () => 0xFFFFFF,
    "towns.bmp",
    sg,
  );
};

let town_color = (255, 255, 0);

let draw_sparse_towns = (town_centers, draw_point) => {
  Core_kernel.(
    List.iter(town_centers, ~f=((x, z)) =>
      draw_point(~size=Town_prototype.side, x, z, town_color)
    )
  );
};

let apply_progress_view = ((towns, _)) => {
  open Core_kernel;
  let town_centers =
    List.map(towns, ~f=town =>
      (town.x + Town_prototype.side / 2, town.z + Town_prototype.side / 2)
    );
  let town_center = List.hd(town_centers);
  let l = Progress_view.push_layer();
  Progress_view.update(
    ~center=?town_center,
    ~title="towns",
    ~draw_sparse=draw_sparse_towns,
    ~state=town_centers,
    l,
  );
};

let within_region_boundaries = (~canon_side, min_x, min_z) => {
  Minecraft_converter.within_region_boundaries(
    ~canon_side,
    ~min_x,
    ~max_x=min_x + Town_prototype.side,
    ~min_z,
    ~max_z=min_z + Town_prototype.side,
  );
};

let obstacle_at = (~x, ~z, obstacles) =>
  !Canonical_overlay.can_build_on(Grid.get(x, z, obstacles));

let has_obstacle = (obstacles, x, z) =>
  Range.exists(z, z + Town_prototype.side - 1, z =>
    Range.exists(x, x + Town_prototype.side - 1, x =>
      Grid.is_within(x, z, obstacles) && obstacle_at(~x, ~z, obstacles)
    )
  );

let acceptable_elevations = (elevations, x, z) => {
  let start_elev = Grid_compat.at(elevations, x, z);
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
          let here_elev = Grid_compat.at(elevations, x, z);
          let emin = min(emin, here_elev);
          let emax = max(emax, here_elev);
          (emin, emax);
        },
      )
    );
  emax - emin <= Town_prototype.elevation_range;
};

let town_area_clear = (canon: Canonical_overlay.t, x, z) =>
  if (!within_region_boundaries(~canon_side=canon.side, x, z)) {
    Tale.log("region boundaries");
    false;
  } else if (has_obstacle(canon.obstacles, x, z)) {
    Tale.log("has obstacle");
    false;
  } else if (!acceptable_elevations(canon.elevation, x, z)) {
    Tale.log("elevation");
    false;
  } else {
    true;
  };

let rec tweak_town_area = (canon, x, z, tries) =>
  if (tries > 0) {
    let try_x = x + Random.int(tweak_dist * 2) - tweak_dist;
    let try_z = z + Random.int(tweak_dist * 2) - tweak_dist;
    if (town_area_clear(canon, try_x, try_z)) {
      Some((try_x, try_z));
    } else {
      tweak_town_area(canon, x, z, tries - 1);
    };
  } else {
    None;
  };
let tweak_town_area = (canon, x, z) =>
  if (town_area_clear(canon, x, z)) {
    Some((x, z));
  } else {
    tweak_town_area(canon, x, z, tweak_tries);
  };

let rec first_suitable_towns = (canon, remaining, coords, selected) => {
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
      switch (tweak_town_area(canon, x, z)) {
      | Some(coord) =>
        Tale.log("Selected town");
        let selected = [coord, ...selected];
        first_suitable_towns(canon, remaining - 1, coords, selected);
      | None =>
        Tale.log("Failed to tweak town");
        first_suitable_towns(canon, remaining, coords, selected);
      };
    } else {
      /* Too close to another town */
      Tale.log("Town too close");
      first_suitable_towns(canon, remaining, coords, selected);
    };
  };
};

let add_block_to_obstacles = (block, obstacles) => {
  let Town_prototype.{min_x, max_x, min_z, max_z, elevation: _} = block;
  Range.fold(min_z, max_z, obstacles, (obstacles, z) =>
    Range.fold(min_x, max_x, obstacles, (obstacles, x) =>
      Canonical_overlay.Obstacles.set(x, z, Impassable, obstacles)
    )
  );
};

let spawn_point_of_block = block => {
  let Town_prototype.{min_x, max_x, min_z, max_z, elevation: block_y} = block;
  let x = (min_x + max_x) / 2;
  let z = (min_z + max_z) / 2;
  let y = block_y + 1;
  (x, y, z);
};

let prepare_town =
    (
      canon: Canonical_overlay.t,
      canon_obstacles: Canonical_overlay.obstacles,
      town_min_x,
      town_min_z,
    ) => {
  let town_max_x = town_min_x + Town_prototype.side - 1;
  let town_max_z = town_min_z + Town_prototype.side - 1;

  /* Slice elevations from base overlay */
  let elevation =
    Grid_compat.init(Town_prototype.side, (town_x, town_z) => {
      Grid_compat.at(
        canon.elevation,
        town_x + town_min_x,
        town_z + town_min_z,
      )
    });

  /* TODO misnomer */
  let town_obstacles =
    Grid.With_coords.fold(
      Grid.With_coords.T(canon.obstacles),
      ~init=Sparse_grid.make(Town_prototype.side),
      ~f=(town_obstacles, (x, z, obs)) =>
      if (!Canonical_overlay.can_build_on(obs)
          && town_min_x <= x
          && x <= town_max_x
          && town_min_z <= z
          && z <= town_max_z) {
        Sparse_grid.put(town_obstacles, x - town_min_x, z - town_min_z, ());
      } else {
        town_obstacles;
      }
    );

  let Town_prototype.{bell, houses, farms} =
    Town_prototype.run({elevation, roads: town_obstacles});

  /* Translate blocks into global coords */
  let translate_block = (b: Town_prototype.block) => {
    ...b,
    min_x: b.min_x + town_min_x,
    max_x: b.max_x + town_min_x,
    min_z: b.min_z + town_min_z,
    max_z: b.max_z + town_min_z,
  };
  let bell = translate_block(bell);
  let houses =
    List.map(
      h => Town_prototype.{...h, block: translate_block(h.block)},
      houses,
    );
  let farms = List.map(translate_block, farms);

  let updated_obstacles =
    canon_obstacles
    |> add_block_to_obstacles(bell)
    |> List.fold_left(
         (o, b: Town_prototype.house) => add_block_to_obstacles(b.block, o),
         _,
         houses,
       )
    |> List.fold_left((o, b) => add_block_to_obstacles(b, o), _, farms);

  let town = {
    x: town_min_x,
    z: town_min_z,
    town: {
      bell,
      houses,
      farms,
    },
  };

  let spawn_point = spawn_point_of_block(bell);

  (town, updated_obstacles, spawn_point);
};

let prepare = (): t => {
  let canon = Canonical_overlay.require();
  let (base, _) = Base_overlay.require();
  /* Shuffle a list of all river tiles */
  Tale.log("Finding river coords");
  let river_coords =
    Grid_compat.filter_map(base, (x, z, base) => {
      switch (base) {
      | {river: true, _} =>
        Some((x - Town_prototype.side / 2, z - Town_prototype.side / 2))
      | {river: false, _} => None
      }
    });
  Tale.log("Shuffling river coords");
  let river_coords =
    Mg_util.shuffle(river_coords) |> Mg_util.take(potential_sites_limit);
  /* Pick and tweak town sites from this list */
  Tale.log("Finding suitable towns");
  let towns = first_suitable_towns(canon, num_towns, river_coords, []);
  List.iter(((x, z)) => Tale.logf("town at %d, %d", x, z), towns);
  draw_towns(canon, towns);
  let (towns, obs, spawn_points) =
    List.fold_left(
      ((towns, obs, spawn_points), (x, z)) => {
        let (town, obs, spawn_point) = prepare_town(canon, obs, x, z);
        ([town, ...towns], obs, [spawn_point, ...spawn_points]);
      },
      ([], Grid.make(~side=canon.side, Canonical_overlay.Clear), []),
      towns,
    );
  (
    towns,
    Canonical_overlay.make_delta(
      ~obstacles=`Add(obs),
      ~spawn_points=`Add(spawn_points),
      (),
    ),
  );
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
  /* 2x3 empty space in front of door */
  for (z in door_z - 2 to door_z - 1) {
    for (x in door_x - 1 to door_x + 1) {
      Building.raise_lower_elev_match(args, x, z, elevation);
    };
  };

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

/**
 adds torches to the town with min-corner x, z
 */
let illuminate_town = (~x, ~z, region): unit => {
  /** determines how far below the first non-Air block to continue illuminating */
  let bottom_extent = 10;
  let rec fold_down_column = (~x, ~y, ~z, ~init, ~f) => {
    switch (Minecraft.Region.get_block_opt(~x, ~y, ~z, region)) {
    | Some(Air) =>
      let next_acc = f(init, (x, y, z));
      fold_down_column(~x, ~y=y - 1, ~z, ~init=next_acc, ~f);
    | Some(_)
    | None =>
      /* Continue below */
      Mg_util.Range.fold(y - bottom_extent, y, init, (acc, y) => {
        f(acc, (x, y, z))
      })
    };
  };
  let over_town = (~init, ~f) => {
    Mg_util.Range.(
      fold(z, z + Town_prototype.side - 1, init, (acc, z) => {
        fold(x, x + Town_prototype.side - 1, acc, (acc, x) => {
          fold_down_column(
            ~x,
            ~y=Minecraft.Region.block_per_region_vertical - 1,
            ~z,
            ~init=acc,
            ~f,
          )
        })
      })
    );
  };
  Torching.illuminate(~volume=over_town, region);
};

let apply_region =
    ((towns, _canon): t, args: Minecraft_converter.region_args) => {
  List.iter(
    ({x, z, town: {bell, farms, houses}}) =>
      if (Minecraft.Region.is_within(~x, ~y=0, ~z, args.region)) {
        create_bell(bell, args);
        List.iter(house => create_house(house, args), houses);
        List.iter(farm => create_farm(farm, args), farms);
        illuminate_town(~x, ~z, args.region);
      },
    towns,
  );
};

let (require, prepare, apply) =
  Overlay.make(
    "towns",
    ~apply_progress_view,
    prepare,
    apply_region,
    bin_reader_t,
    bin_writer_t,
  );
