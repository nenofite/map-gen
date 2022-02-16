open! Core_kernel;

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

module Rp = Roads.Rules.Coord;

[@deriving bin_io]
type road = Rp.t;

[@deriving bin_io]
type x = {
  pois: list((int, int)),
  roads: Sparse_grid.t(road),
  bridges: list(Roads.Rules.bridge),
};

[@deriving bin_io]
type t = (x, Overlay.Canon.delta);

let town_goal_side = Town_overlay.town_side;

let edge_cost = (canon: Overlay.Canon.t, (ax, ay), (bx, by)) => {
  let a_elev = Grid.Mut.get(canon.elevation, ~x=ax, ~z=ay);
  let b_elev = Grid.Mut.get(canon.elevation, ~x=bx, ~z=by);
  let elev_diff = abs(a_elev - b_elev);
  let b_obs = !Overlay.Canon.can_build_on(Grid.get(bx, by, canon.obstacles));
  if (!b_obs && elev_diff <= 1) {
    Some(Mg_util.Floats.(~.elev_diff +. 1.));
  } else {
    None;
  };
};

let place_road = (_canon: Overlay.Canon.t, roads: Sparse_grid.t(road), path) => {
  let rec go = (roads: Sparse_grid.t(road), path) =>
    switch (path) {
    | [] => roads
    | [road, ...path] =>
      let roads = Sparse_grid.put(roads, road.Rp.x, road.Rp.z, road);
      go(roads, path);
    };
  go(roads, path);
};

let poi_of_town = (town: Town_overlay.town) => {
  Town_overlay.(town.x + town_side / 2, town.z + town_side / 2);
};

let starts_of_poi = ((poi_x, poi_z)) => {
  let min_x = poi_x - town_goal_side / 2;
  let min_z = poi_z - town_goal_side / 2;
  let max_x = min_x + town_goal_side - 1;
  let max_z = min_z + town_goal_side - 1;
  Range.map(min_z, max_z, z => Range.map(min_x, max_x, x => (x, z)))
  |> List.concat;
};

let connect_all_towns = (towns: Town_overlay.t', ~pathing_state): unit => {
  let num_towns = List.length(towns);
  List.iteri(towns, ~f=(i, town) => {
    Tale.blockf(
      "Enroading town %d of %d",
      i + 1,
      num_towns,
      ~f=() => {
        let town_roads = Town_overlay.roads(town.town);
        Roads.enroad_roads(pathing_state, ~roads=town_roads);
      },
    )
  });
};

let prepare = () => {
  let canon = Overlay.Canon.require();
  let (towns, _) = Town_overlay.require();
  let pathing_state = Roads.init_state();
  connect_all_towns(towns, ~pathing_state);

  Tale.log("Widening roads and adding steps");
  let roads =
    Roads.get_paths_list(pathing_state)
    |> Roads.Rules.put_roads_onto_sparse_grid(
         ~grid=Sparse_grid.make(canon.side),
       );
  let bridges = Roads.get_bridges_list(pathing_state);
  let obstacles =
    Sparse_grid.(
      map(roads, (_, _) => Overlay.Canon.Impassable)
      |> to_grid(~default=Overlay.Canon.Clear)
    );
  (
    {pois: [], roads, bridges},
    Overlay.Canon.make_delta(~obstacles=`Add(obstacles), ()),
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
  Minecraft.Region.set_block_opt(~x, ~y, ~z, Stone_brick_slab, region);
  fill_beneath_road(region, x, y - 1, z);
};

let stair_dir_of_dir4: Rp.dir4 => Minecraft.Block.stair_dir =
  fun
  | N => N
  | E => E
  | S => S
  | W => W;

let place_stair_block = (region, x, y, z, ~dir) => {
  clear_above_road(region, x, y + 1, z);
  Minecraft.Region.set_block_opt(
    ~x,
    ~y,
    ~z,
    Stairs(Stone_brick_stairs, stair_dir_of_dir4(dir)),
    region,
  );
  fill_beneath_road(region, x, y - 1, z);
};

let template_of_bridge = bridge => {
  let Roads.Rules.{x, y, z, direction, length} = bridge;
  let rotation =
    switch (direction) {
    | N => 0
    | E => 1
    | S => 2
    | W => 3
    };
  Bridge.bridge(~length, ~rotation)
  |> Minecraft_template.translate(_, x, y, z);
};

let apply_region = ((state, _canon), region: Minecraft.Region.t) => {
  let region = region;
  Sparse_grid.iter(state.roads, ((x, z), road) =>
    if (Minecraft.Region.is_within(~x, ~y=0, ~z, region)) {
      let Rp.{x: _, y, z: _, structure} = road;
      switch (structure) {
      | Road => place_road_block(region, x, y, z)
      | Stair(dir) => place_stair_block(region, x, y, z, ~dir)
      | Bridge(_) => ()
      };
    }
  );
  List.iter(
    state.bridges,
    ~f=bridge => {
      let t = template_of_bridge(bridge);
      Minecraft_template.place_overwrite_opt(~x=0, ~y=0, ~z=0, t, region);
    },
  );
};

let apply_progress_view = state => {
  let ({roads, _}, _) = state;
  let draw_sparse = d => {
    let white = 0xFFFFFF;
    Sparse_grid.iter(roads, ((x, z), _) => {
      d(~size=1, ~color=white, x, z)
    });
  };
  let side = Overlay.Canon.require().side;
  let layer = Progress_view.push_layer();
  Progress_view.update(~fit=(0, side, 0, side), ~draw_sparse, layer);
  Progress_view.save(~side, "road");
};

let (require, prepare, apply) =
  Overlay.make(
    "roads",
    ~apply_progress_view,
    prepare,
    apply_region,
    bin_reader_t,
    bin_writer_t,
  );
