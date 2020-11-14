open Core_kernel;

module Niceness = {
  module T = {
    [@deriving (ord, sexp, bin_io)]
    type t =
      | Dirt
      | Paved;
  };
  include T;
  include Comparable.Make(T);
};

[@deriving bin_io]
type road = {
  elevation: int,
  niceness: Niceness.t,
};

[@deriving bin_io]
type stepped_road = {
  road,
  step_down: bool,
};

[@deriving bin_io]
type x = {
  pois: list((int, int)),
  roads: Sparse_grid.t(stepped_road),
};

[@deriving bin_io]
type t = (x, Canonical_overlay.t);

let colorizer =
  fun
  | None => 0
  | Some(_road) => 0xFFFFFF;

let rec all_pairs = (list, result) =>
  switch (list) {
  | [] => result
  | [a, ...list] =>
    let result = List.map(list, ~f=b => (a, b)) @ result;
    all_pairs(list, result);
  };

let heuristic = (canon: Canonical_overlay.t, (ax, ay), (bx, by)) => {
  let a_elev = Grid_compat.at(canon.elevation, ax, ay);
  let b_elev = Grid_compat.at(canon.elevation, bx, by);
  A_star.distance_3d((ax, ay, a_elev), (bx, by, b_elev));
};

let edge_cost = (canon: Canonical_overlay.t, (ax, ay), (bx, by)) => {
  let a_elev = Grid_compat.at(canon.elevation, ax, ay);
  let b_elev = Grid_compat.at(canon.elevation, bx, by);
  let elev_diff = abs(a_elev - b_elev);
  let b_obs = Grid.get(bx, by, canon.obstacles);
  if (!b_obs && elev_diff <= 1) {
    Some(Mg_util.Floats.(~.elev_diff +. 1.));
  } else {
    None;
  };
};

let heighten_road = (roads, x, y, up_to_niceness, up_to_elevation) => {
  switch (Sparse_grid.at(roads, x, y)) {
  | Some({elevation, niceness})
      when elevation < up_to_elevation || Niceness.(niceness < up_to_niceness) =>
    let new_elevation = max(elevation, up_to_elevation);
    let new_niceness = Niceness.max(niceness, up_to_niceness);
    Sparse_grid.put(
      roads,
      x,
      y,
      {elevation: new_elevation, niceness: new_niceness},
    );
  | Some(_) => roads
  | None =>
    Sparse_grid.put(
      roads,
      x,
      y,
      {elevation: up_to_elevation, niceness: up_to_niceness},
    )
  };
};

/**
  widen_path makes Paved roads three blocks wide. Each block is the highest
  elevation and nicest niceness of any road it touches.
 */
let widen_road = (roads: Sparse_grid.t(road)) => {
  Sparse_grid.fold(
    roads,
    ((x, y), road, widened_roads) => {
      let {niceness, elevation} = road;
      switch (niceness) {
      | Dirt => widened_roads /* TODO */
      | Paved =>
        Grid_compat.eight_directions
        |> List.fold_left(~init=widened_roads, ~f=(roads, (dx, dy)) =>
             heighten_road(roads, x + dx, y + dy, niceness, elevation)
           )
      };
    },
    roads,
  );
};

/**
  add_steps takes a widened path and smooths it. If any block cardinally
  neighbors another block at the elevation below it, then it becomes a step.
 */
let add_steps = (roads: Sparse_grid.t(road)) => {
  Sparse_grid.map(
    roads,
    ((x, y), road) => {
      let {elevation, _} = road;
      let has_lower_neighbor =
        Grid_compat.four_directions
        |> List.exists(~f=((dx, dy)) =>
             switch (Sparse_grid.at(roads, x + dx, y + dy)) {
             | Some({elevation: neighbor_elevation, _})
                 when neighbor_elevation == elevation - 1 =>
               true
             | Some(_)
             | None => false
             }
           );
      {road, step_down: has_lower_neighbor};
    },
  );
};

let rec add_road_to_grid = (roads: Sparse_grid.t(road), path) =>
  switch (path) {
  | [] => roads
  | [(x, y, road_tile), ...path] =>
    let roads = Sparse_grid.put(roads, x, y, road_tile);
    add_road_to_grid(roads, path);
  };

let place_road =
    (canon: Canonical_overlay.t, roads: Sparse_grid.t(road), path) => {
  /* Add elevations */
  let path =
    List.map(
      path,
      ~f=((x, y)) => {
        let elevation = Grid_compat.at(canon.elevation, x, y);
        (x, y, {elevation, niceness: Paved /* TODO */});
      },
    );
  /* Add to grid */
  add_road_to_grid(roads, path);
};

let poi_of_town = (town: Town_overlay.town) => {
  Town_prototype.(town.x + side / 2, town.z + side / 2);
};

let starts_of_poi = ((x, z)) => {
  open Town_prototype;
  let min_x = x - side / 2;
  let min_z = z - side / 2;
  let max_x = min_x + side - 1;
  let max_z = min_z + side - 1;
  Range.map(min_z, max_z, z => Range.map(min_x, max_x, x => (x, z)))
  |> List.concat;
};

let goalf_of_poi = ((poi_x, poi_z)) => {
  open Town_prototype;
  let min_x = poi_x - side / 2;
  let min_z = poi_z - side / 2;
  let max_x = min_x + side - 1;
  let max_z = min_z + side - 1;
  ((x, z)) => min_x <= x && x <= max_x && min_z <= z && z <= max_z;
};

let prepare = (canon: Canonical_overlay.t, towns: Town_overlay.x, ()) => {
  /* Use a point cloud to get points of interest */
  Tale.log("Making points of interest");
  let pois = List.map(~f=poi_of_town, towns);
  /* Run A* to go from each point to each other point */
  let roads = Sparse_grid.make(canon.side);
  let poi_pairs = all_pairs(pois, []) |> Mg_util.take(100, _);
  Tale.log("Pathfinding roads");
  let roads =
    List.fold_left(poi_pairs, ~init=roads, ~f=(roads, (start, goal)) => {
      switch (
        A_star.run_multi(
          ~grid_side=canon.side,
          ~starts=starts_of_poi(start),
          ~goalf=goalf_of_poi(goal),
          ~edge_cost=edge_cost(canon),
          ~heuristic=heuristic(canon, goal),
        )
      ) {
      | Some(path) =>
        Tale.log("found a road");
        place_road(canon, roads, path); /* Add the path to the grid */
      | None =>
        Tale.log("couldn't find road");
        roads;
      }
    });
  Tale.log("Widening roads and adding steps");
  let roads = widen_road(roads);
  let roads = add_steps(roads);
  Tale.log("Drawing");
  Draw.draw_sparse_grid(colorizer, "roads.png", roads);
  let obstacles =
    Sparse_grid.(map(roads, (_, _) => true) |> to_grid(~default=false))
    |> Canonical_overlay.add_obstacles(~onto=canon.obstacles);
  /* TODO: add "stairs" to large increases in elev */
  ({pois, roads}, {...canon, obstacles});
};

/** fill_beneath_road places a column of cobblestone until it reaches solid ground */
let rec fill_beneath_road = (region, x, y, z) => {
  Minecraft.Region.(
    switch (get_block_opt(region, ~x, ~y, ~z)) {
    | Some(Air | Flowing_water(_) | Water) =>
      set_block(~x, ~y, ~z, Cobblestone, region);
      fill_beneath_road(region, x, y - 1, z);
    | None
    | Some(_) => ()
    }
  );
};

/** clear_above_road clears a short column above the road */
let clear_above_road = (region, x, y, z) => {
  for (dy in 0 to 10) {
    Minecraft.Region.set_block_opt(~x, ~y=y + dy, ~z, Air, region);
  };
};

let place_road_block = (region, x, y, z) => {
  clear_above_road(region, x, y + 1, z);
  Minecraft.Region.set_block_opt(~x, ~y, ~z, Stone_slab, region);
  fill_beneath_road(region, x, y - 1, z);
};

let place_step_block = (region, x, y, z) => {
  /* Step blocks are actually full blocks at y - 1 */
  let y = y - 1;
  clear_above_road(region, x, y + 1, z);
  Minecraft.Region.set_block(
    ~x,
    ~y,
    ~z,
    Minecraft.Block.(Smooth_stone_slab(Double)),
    region,
  );
  fill_beneath_road(region, x, y - 1, z);
};

let apply_region = ((state, _canon), args: Minecraft_converter.region_args) => {
  let region = args.region;
  Sparse_grid.iter(state.roads, ((x, z), road) =>
    if (Minecraft.Region.is_within(~x, ~y=0, ~z, region)) {
      let {road: {elevation: y, niceness}, step_down} = road;
      ignore(niceness); /* TODO dirt paths */
      if (step_down) {
        place_step_block(region, x, y, z);
      } else {
        place_road_block(region, x, y, z);
      };
    }
  );
};

let overlay = (canon, towns): Overlay.monad(t) =>
  Overlay.make(
    "roads",
    prepare(canon, towns),
    apply_region,
    bin_reader_t,
    bin_writer_t,
  );