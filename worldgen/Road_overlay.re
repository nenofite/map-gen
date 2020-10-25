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
type t = {
  pois: list((int, int)),
  roads: Sparse_grid.t(stepped_road),
};

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

let heuristic = (base: Grid.t(Base_overlay.tile), (ax, ay), (bx, by)) => {
  let a_elev = Grid.at(base, ax, ay).elevation;
  let b_elev = Grid.at(base, bx, by).elevation;
  A_star.distance_3d((ax, ay, a_elev), (bx, by, b_elev));
};

let edge_cost = (base: Grid.t(Base_overlay.tile), (ax, ay), (bx, by)) => {
  let a_elev = Grid.at(base, ax, ay).elevation;
  switch (Grid.at(base, bx, by)) {
  | {river: false, ocean: false, elevation: b_elev} =>
    let elev_diff = abs(a_elev - b_elev);
    switch (elev_diff) {
    | 0 => Some(1.)
    | 1 => Some(2.)
    | _ => None
    };
  | {river: true, _}
  | {ocean: true, _} => None
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
        Grid.eight_directions
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
        Grid.four_directions
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
    (base: Grid.t(Base_overlay.tile), roads: Sparse_grid.t(road), path) => {
  /* Add elevations */
  let path =
    List.map(
      path,
      ~f=((x, y)) => {
        let elevation = Grid.at(base, x, y).elevation;
        (x, y, {elevation, niceness: Paved /* TODO */});
      },
    );
  /* Add to grid */
  add_road_to_grid(roads, path);
};

let prepare = (base: Grid.t(Base_overlay.tile), ()) => {
  /* Use a point cloud to get points of interest */
  print_endline("Making points of interest");
  let pois =
    Point_cloud.init(
      ~width=base.side, ~height=base.side, ~spacing=256, (_, _) =>
      ()
    ).
      points
    |> List.filter_map(~f=(Point_cloud.{x, y, _}) => {
         let x = int_of_float(x);
         let y = int_of_float(y);
         switch (Grid.at(base, x, y)) {
         | {ocean: false, river: false, _} => Some((x, y))
         | _ => None
         };
       });
  /* Run A* to go from each point to each other point */
  let roads = Sparse_grid.make(base.side);
  let poi_pairs = all_pairs(pois, []) |> Mg_util.take(10, _);
  print_endline("Pathfinding roads");
  let roads =
    List.fold_left(poi_pairs, ~init=roads, ~f=(roads, (start, goal)) => {
      switch (
        A_star.run(
          ~grid_side=base.side,
          ~start,
          ~goal,
          ~edge_cost=edge_cost(base),
          ~heuristic=heuristic(base),
        )
      ) {
      | Some(path) =>
        print_endline("found a road");
        place_road(base, roads, path); /* Add the path to the grid */
      | None =>
        print_endline("couldn't find road");
        roads;
      }
    });
  print_endline("Widening roads and adding steps");
  let roads = widen_road(roads);
  let roads = add_steps(roads);
  print_endline("Drawing");
  Draw.draw_sparse_grid(colorizer, "roads.png", roads);
  /* TODO: add "stairs" to large increases in elev */
  {pois, roads};
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

let apply_region =
    (
      _base: Grid.t(Base_overlay.tile),
      state,
      args: Minecraft_converter.region_args,
    ) => {
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

let overlay = base =>
  Overlay.make(
    "roads",
    prepare(base),
    apply_region(base),
    bin_reader_t,
    bin_writer_t,
  );