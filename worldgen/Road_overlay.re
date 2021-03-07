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
type t = (x, Canonical_overlay.delta);

let town_goal_side = Town_prototype.side;

let colorizer =
  fun
  | None => 0
  | Some(_road) => 0xFFFFFF;

/** [a,b,c] => [(a,b),(b,a), (a,c),(c,a), (b,c),(c,b)] */
let rec all_pairs = (list, result) =>
  switch (list) {
  | [] => result
  | [a, ...list] =>
    let result = List.concat_map(list, ~f=b => [(a, b), (b, a)]) @ result;
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
  let b_obs =
    !Canonical_overlay.can_build_on(Grid.get(bx, by, canon.obstacles));
  if (!b_obs && elev_diff <= 1) {
    Some(Mg_util.Floats.(~.elev_diff +. 1.));
  } else {
    None;
  };
};

let heighten_road =
    (~get_obstacle, roads, x, y, up_to_niceness, up_to_elevation) => {
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
    switch (get_obstacle(~x, ~z=y)) {
    | Canonical_overlay.Impassable
    | Bridgeable => roads
    | Clear =>
      Sparse_grid.put(
        roads,
        x,
        y,
        {elevation: up_to_elevation, niceness: up_to_niceness},
      )
    }
  };
};

/**
  widen_path makes Paved roads three blocks wide. Each block is the highest
  elevation and nicest niceness of any road it touches.
 */
let widen_road = (~get_obstacle, roads: Sparse_grid.t(road)) => {
  Sparse_grid.fold(
    roads,
    ((x, y), road, widened_roads) => {
      let {niceness, elevation} = road;
      switch (niceness) {
      | Dirt => widened_roads /* TODO */
      | Paved =>
        Grid_compat.eight_directions
        |> List.fold_left(~init=widened_roads, ~f=(roads, (dx, dy)) =>
             heighten_road(
               ~get_obstacle,
               roads,
               x + dx,
               y + dy,
               niceness,
               elevation,
             )
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
    (_canon: Canonical_overlay.t, roads: Sparse_grid.t(road), path) => {
  /* Add elevations */
  let path =
    List.filter_map(path, ~f=(Bridge_pathing.{x, y, z, bridge}) => {
      switch (bridge) {
      | No_bridge => Some((x, z, {elevation: y, niceness: Paved /* TODO */}))
      | Ns_bridge
      | Ew_bridge =>
        Tale.logf("bridge at %d, %d", x, z);
        None;
      }
    });
  /* Add to grid */
  add_road_to_grid(roads, path);
};

let poi_of_town = (town: Town_overlay.town) => {
  Town_prototype.(town.x + side / 2, town.z + side / 2);
};

let starts_of_poi = ((poi_x, poi_z)) => {
  let min_x = poi_x - town_goal_side / 2;
  let min_z = poi_z - town_goal_side / 2;
  let max_x = min_x + town_goal_side - 1;
  let max_z = min_z + town_goal_side - 1;
  let g_score_at = (~x, ~z) =>
    Int.((x - poi_x) ** 2 + (z - poi_z) ** 2) * Bridge_pathing.flat_ground_cost;
  Range.map(min_z, max_z, z =>
    Range.map(min_x, max_x, x => (x, z, g_score_at(~x, ~z)))
  )
  |> List.concat;
};

let goalf_of_poi = ((poi_x, poi_z)) => {
  let min_x = poi_x - town_goal_side / 2;
  let min_z = poi_z - town_goal_side / 2;
  let max_x = min_x + town_goal_side - 1;
  let max_z = min_z + town_goal_side - 1;
  (~x, ~z) => min_x <= x && x <= max_x && min_z <= z && z <= max_z;
};

let prepare = () => {
  let canon = Canonical_overlay.require();
  let (towns, _) = Town_overlay.require();
  /* Use a point cloud to get points of interest */
  Tale.log("Making points of interest");
  let pois = List.map(~f=poi_of_town, towns);
  /* Run A* to go from each point to each other point and again in reverse */
  let poi_pairs = all_pairs(pois, []) |> Mg_util.take(1000, _);

  Tale.log("Pathfinding roads");
  let is_within = (~x, ~z) => Grid.is_within_side(~x, ~y=z, canon.side);
  let get_elevation = (~x, ~z) =>
    if (is_within(~x, ~z)) {
      Grid.get(x, z, canon.elevation);
    } else {
      0;
    };
  let get_obstacle = (~x, ~z) =>
    if (is_within(~x, ~z)) {
      Grid.get(x, z, canon.obstacles);
    } else {
      Impassable;
    };
  let get_obstacle_in_margin = (~margin, ~x, ~z) =>
    Range.fold(z - margin, z + margin, Canonical_overlay.Clear, (obs, z) =>
      Range.fold(
        x - margin,
        x + margin,
        obs,
        (obs, x) => {
          let here_obs = get_obstacle(~x, ~z);
          Canonical_overlay.Obstacle.max(obs, here_obs);
        },
      )
    );
  module Cs = Bridge_pathing.Coord.Set;
  let roads =
    List.fold_left(
      poi_pairs,
      ~init=Cs.empty,
      ~f=(roads, (start, goal)) => {
        let has_existing_road = coord => Cs.mem(roads, coord);
        switch (
          Bridge_pathing.pathfind_road(
            ~get_elevation,
            ~get_obstacle=get_obstacle_in_margin(~margin=3),
            ~has_existing_road,
            ~start_coords=starts_of_poi(start),
            ~goal_pred=goalf_of_poi(goal),
            ~goal_coord=goal,
          )
        ) {
        | Some(path) =>
          Tale.log("found a road");
          List.fold(path, ~init=roads, ~f=Cs.add);
        | None =>
          Tale.log("couldn't find road");
          roads;
        };
      },
    );
  let roads =
    place_road(canon, Sparse_grid.make(canon.side), Cs.to_list(roads));
  Tale.log("Widening roads and adding steps");
  let roads = widen_road(~get_obstacle, roads);
  let roads = add_steps(roads);
  Tale.log("Drawing");
  Draw.draw_sparse_grid(colorizer, "roads.png", roads);
  let obstacles =
    Sparse_grid.(
      map(roads, (_, _) => Canonical_overlay.Impassable)
      |> to_grid(~default=Canonical_overlay.Clear)
    );
  (
    {pois, roads},
    Canonical_overlay.make_delta(~obstacles=`Add(obstacles), ()),
  );
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

let (require, prepare, apply) =
  Overlay.make("roads", prepare, apply_region, bin_reader_t, bin_writer_t);
