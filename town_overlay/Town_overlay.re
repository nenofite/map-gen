include Town_overlay_i;
open Mg_util;

let town_side = Town_layout.side;

let tweak_dist = 150;
let tweak_tries = 100;
let num_towns = 8;
let min_dist_between_towns = 500;
let potential_sites_limit = 100;
let wall_height = 4;
let torch_margin = 10;

let roads = t => t.roads;

let town_color = (255, 255, 0);

let apply_progress_view = ((towns, delta: Overlay.Canon.delta)) => {
  open Core_kernel;
  let side = Overlay.Canon.require().side;
  let obs =
    switch (delta.Overlay.Canon.obstacles) {
    | `Add(o) => o
    | _ => failwith("expected `Add obstacles")
    };

  let draw_dense = ((), x, z) =>
    if (Grid.is_within(x, z, obs)) {
      if (!Overlay.Canon.can_build_on(Grid.get(x, z, obs))) {
        Some(town_color);
      } else {
        None;
      };
    } else {
      None;
    };

  let town_centers =
    List.map(towns, ~f=town =>
      (town.x + Town_layout.side / 2, town.z + Town_layout.side / 2)
    );
  let town_center = List.hd(town_centers);
  let l = Progress_view.push_layer();
  Progress_view.update(
    ~center=?town_center,
    ~title="towns",
    ~draw_dense,
    ~state=(),
    l,
  );
  Progress_view.save(~side, "towns");
};

let within_region_boundaries = (~canon_side, min_x, min_z) => {
  Minecraft.Region.within_region_boundaries(
    ~canon_side,
    ~min_x,
    ~max_x=min_x + Town_layout.side,
    ~min_z,
    ~max_z=min_z + Town_layout.side,
  );
};

let obstacle_at = (~x, ~z, obstacles) =>
  !Overlay.Canon.can_build_on(Grid.get(x, z, obstacles));

let has_obstacle = (obstacles, x, z) =>
  Range.exists(z, z + Town_layout.side - 1, z =>
    Range.exists(x, x + Town_layout.side - 1, x =>
      Grid.is_within(x, z, obstacles) && obstacle_at(~x, ~z, obstacles)
    )
  );

let acceptable_elevations = (elevations, x, z) => {
  let start_elev = Grid.Compat.at(elevations, x, z);
  let (emin, emax) =
    Range.fold(
      z,
      z + Town_layout.side - 1,
      (start_elev, start_elev),
      ((emin, emax), z) =>
      Range.fold(
        x,
        x + Town_layout.side - 1,
        (emin, emax),
        ((emin, emax), x) => {
          let here_elev = Grid.Compat.at(elevations, x, z);
          let emin = min(emin, here_elev);
          let emax = max(emax, here_elev);
          (emin, emax);
        },
      )
    );
  emax - emin <= Town_layout.elevation_range;
};

let town_area_clear = (canon: Overlay.Canon.t, x, z) =>
  if (!within_region_boundaries(~canon_side=canon.side, x, z)) {
    false;
  } else if (has_obstacle(canon.obstacles, x, z)) {
    false;
  } else if (!acceptable_elevations(canon.elevation, x, z)) {
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
  let {min_x, max_x, min_z, max_z} = block;
  Range.fold(min_z, max_z, obstacles, (obstacles, z) =>
    Range.fold(min_x, max_x, obstacles, (obstacles, x) =>
      Overlay.Canon.Obstacles.set(x, z, Impassable, obstacles)
    )
  );
};

let spawn_point_of_block = block => {
  let {xz: {min_x, max_x, min_z, max_z}, elevation: block_y} = block;
  let x = (min_x + max_x) / 2;
  let z = (min_z + max_z) / 2;
  let y = block_y + 1;
  (x, y, z);
};

let extract_input = (canon: Overlay.Canon.t, town_min_x, town_min_z) => {
  let side = Town_layout.side;
  let elevation =
    Grid.Int.init(~side, ((tx, tz)) => {
      Grid.get(tx + town_min_x, tz + town_min_z, canon.elevation)
    });
  let obstacles =
    Mg_util.Range.(
      fold(0, side - 1, Sparse_grid.make(side), (town_obs, tz) => {
        fold(
          0,
          side - 1,
          town_obs,
          (town_obs, tx) => {
            let x = tx + town_min_x;
            let z = tz + town_min_z;
            let obs = Grid.get(x, z, canon.obstacles);
            if (!Overlay.Canon.can_build_on(obs)) {
              Sparse_grid.put(town_obs, tx, tz, ());
            } else {
              town_obs;
            };
          },
        )
      })
    );
  Town_layout.{elevation, obstacles};
};

let prepare_town =
    (
      canon: Overlay.Canon.t,
      canon_obstacles: Overlay.Canon.obstacles,
      town_min_x,
      town_min_z,
    ) => {
  let input = extract_input(canon, town_min_x, town_min_z);
  let {bell, houses, farms, roads} = Town_layout.run(input);

  let roads =
    Core_kernel.List.map(
      roads,
      ~f=Roads.Rules.Coord.translate(~dx=town_min_x, ~dz=town_min_z),
    );

  /* Translate blocks into global coords */
  let translate_block_no_elevation = (b: block_no_elevation) => {
    min_x: b.min_x + town_min_x,
    max_x: b.max_x + town_min_x,
    min_z: b.min_z + town_min_z,
    max_z: b.max_z + town_min_z,
  };
  let translate_block = (b: block) => {
    xz: translate_block_no_elevation(b.xz),
    elevation: b.elevation,
  };
  let translate_building = (b: fitted_building) => {
    ...b,
    block: translate_block_no_elevation(b.block),
  };
  let bell = translate_block(bell);
  let houses =
    List.map(h => {...h, building: translate_building(h.building)}, houses);
  let farms = List.map(translate_block, farms);

  let updated_obstacles =
    canon_obstacles
    |> add_block_to_obstacles(bell.xz)
    |> List.fold_left(
         (o, b: house) => add_block_to_obstacles(b.building.block, o),
         _,
         houses,
       )
    |> List.fold_left((o, b) => add_block_to_obstacles(b.xz, o), _, farms);

  let town = {
    x: town_min_x,
    z: town_min_z,
    town: {
      bell,
      houses,
      farms,
      roads,
    },
  };

  let spawn_point = spawn_point_of_block(bell);

  (town, updated_obstacles, spawn_point);
};

let prepare = (): t => {
  let canon = Overlay.Canon.require();
  let (base, _) = Base_overlay.require();
  let side = Base_overlay.side(base);
  /* Shuffle a list of all river tiles */
  Tale.log("Finding river coords");
  let river_coords =
    Mg_util.Range.fold(0, side - 1, [], (ls, z) => {
      Mg_util.Range.fold(0, side - 1, ls, (ls, x) =>
        if (Base_overlay.river_at(~x, ~z, base)
            || Base_overlay.oceanside_at(~x, ~z, base)) {
          [(x - Town_layout.side / 2, z - Town_layout.side / 2), ...ls];
        } else {
          ls;
        }
      )
    });
  Tale.log("Shuffling river coords");
  let river_coords =
    Mg_util.shuffle(river_coords) |> Mg_util.take(potential_sites_limit);
  /* Pick and tweak town sites from this list */
  Tale.log("Finding suitable towns");
  let towns = first_suitable_towns(canon, num_towns, river_coords, []);
  List.iter(((x, z)) => Tale.logf("town at %d, %d", x, z), towns);
  let (towns, obs, spawn_points) =
    List.fold_left(
      ((towns, obs, spawn_points), (x, z)) => {
        let (town, obs, spawn_point) = prepare_town(canon, obs, x, z);
        ([town, ...towns], obs, [spawn_point, ...spawn_points]);
      },
      ([], Grid.make(~side=canon.side, Overlay.Canon.Clear), []),
      towns,
    );
  (
    towns,
    Overlay.Canon.make_delta(
      ~obstacles=`Add(obs),
      ~spawn_points=`Add(spawn_points),
      (),
    ),
  );
};

let create_bell = (bell: block, region: Minecraft.Region.t) => {
  open Minecraft.Region;
  let plaza_material = Minecraft.Block.Stone_brick_slab;
  let base_material = Minecraft.Block.Stone;

  let {xz: {min_x, max_x, min_z, max_z}, elevation: _} = bell;

  /* Plaza */
  for (x in min_x to max_x) {
    for (z in min_z to max_z) {
      Building_old.raise_lower_elev_match(region, x, z, bell.elevation);
      set_block(~x, ~y=bell.elevation, ~z, plaza_material, region);
    };
  };

  /* Base of bell */
  let center_x = (max_x + min_x) / 2;
  let center_z = (max_z + min_z) / 2;
  for (x in center_x - 1 to center_x + 1) {
    for (z in center_z - 1 to center_z + 1) {
      Building_old.raise_lower_elev_match(region, x, z, bell.elevation);
      set_block(~x, ~y=bell.elevation, ~z, base_material, region);
    };
  };

  set_block(
    Minecraft.Block.Bell,
    ~x=center_x,
    ~y=bell.elevation + 1,
    ~z=center_z,
    region,
  );
};

let worksite_material = (worksite: worksite) => {
  Minecraft.Block.(
    switch (worksite) {
    | Butcher => Smoker
    | Fisherman => Barrel
    | Shepherd => Loom
    }
  );
};

let apply_worksite_to_house = (~house: building, worksite: option(worksite)) => {
  switch (worksite) {
  | None => house.template
  | Some(worksite) =>
    let (x, y, z) = house.worksite_offset;
    Minecraft_template.set_at(
      ~x,
      ~y,
      ~z,
      ~block=worksite_material(worksite),
      house.template,
    );
  };
};

let create_house = (house: house, region: Minecraft.Region.t) => {
  open Minecraft.Region;

  let {building, worksite} = house;
  let template =
    apply_worksite_to_house(worksite, ~house=building.building)
    |> Minecraft_template.normalize_on_origin;

  let x = building.block.min_x;
  let z = building.block.min_z;
  let y = Building_old.flatten_footprint(region, ~x, ~z, template.footprint);
  Minecraft_template.place_overwrite(template, ~x, ~y, ~z, region);

  switch (worksite) {
  | None => ()
  | Some(_) =>
    add_entity(
      Minecraft.Entity.{
        id: "villager",
        x: Float.of_int(x + 1),
        y: Float.of_int(y + 11),
        z: Float.of_int(z + 1),
      },
      region,
    )
  };
};

let create_farm = (farm: block, region: Minecraft.Region.t) => {
  open Minecraft.Region;
  open Minecraft.Block;

  let {xz: {min_x, max_x, min_z, max_z}, elevation} = farm;
  let elevation = elevation + 1;

  /* Foundation */
  for (x in min_x to max_x) {
    for (z in min_z to max_z) {
      Building_old.raise_lower_elev_match(region, x, z, elevation);
    };
  };

  /* Composter on NE corner */
  set_block(Composter, ~x=max_x, ~y=elevation + 1, ~z=min_z, region);

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
    region,
  );

  /* Alternate water and crops */
  let crop = Wheat(Random.int(8));
  for (x in min_x + 1 to max_x - 1) {
    for (z in min_z + 1 to max_z - 1) {
      if (z mod 2 == 0) {
        set_block(Water, ~x, ~y=elevation, ~z, region);
      } else {
        set_block(Farmland, ~x, ~y=elevation, ~z, region);
        set_block(crop, ~x, ~y=elevation + 1, ~z, region);
      };
    };
  };
};

/**
 adds torches to the town with min-corner x, z
 */
let illuminate_town = (~x, ~z, ~blocks, region): unit => {
  open Core_kernel;
  let dist_to_nearest_block = (~x, ~z) => {
    let n =
      List.map(blocks, ~f=b => {
        Town_layout.distance_to_block_edge(~x, ~z, b)
      })
      |> List.min_elt(~compare=Int.compare);
    Option.value(n, ~default=Town_layout.side);
  };
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
      fold(z, z + Town_layout.side - 1, init, (acc, z) => {
        fold(x, x + Town_layout.side - 1, acc, (acc, x) =>
          if (dist_to_nearest_block(~x, ~z) <= torch_margin) {
            fold_down_column(
              ~x,
              ~y=Minecraft.Region.block_per_region_vertical - 1,
              ~z,
              ~init=acc,
              ~f,
            );
          } else {
            acc;
          }
        )
      })
    );
  };
  Torching.illuminate(~volume=over_town, region);
};

let apply_region = ((towns, _canon): t, region: Minecraft.Region.t) => {
  List.iter(
    ({x, z, town: {bell, farms, houses, roads} as town}) =>
      if (Minecraft.Region.is_within(~x, ~y=0, ~z, region)) {
        // TODO
        ignore(roads);
        create_bell(bell, region);
        List.iter(house => create_house(house, region), houses);
        List.iter(farm => create_farm(farm, region), farms);
        illuminate_town(
          ~x,
          ~z,
          ~blocks=Town_layout.all_blocks(town),
          region,
        );
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
