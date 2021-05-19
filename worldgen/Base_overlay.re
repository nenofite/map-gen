open Core_kernel;

[@deriving (eq, ord, bin_io)]
type water =
  | No_water
  | River(int)
  | Ocean;

module Water_grid =
  Grid.Make0({
    type t = water;
    let (==) = equal_water;
  });

[@deriving bin_io]
type x = {
  water: Grid.t(water),
  elevation: Grid.t(int),
};

[@deriving bin_io]
type t = (x, Overlay.Canon.t);

let overlay = Overlay.make_overlay("base", bin_reader_t, bin_writer_t);

let elevation_at = (~x, ~z, base) => {
  Grid.get(x, z, base.elevation);
};

let water_at = (~x, ~z, base) => {
  Grid.get(x, z, base.water);
};

let any_water_at = (~x, ~z, base) => {
  switch (water_at(~x, ~z, base)) {
  | No_water => false
  | River(_)
  | Ocean => true
  };
};

let river_at = (~x, ~z, base) => {
  switch (water_at(~x, ~z, base)) {
  | River(_) => true
  | _ => false
  };
};

let river_depth_at = (~x, ~z, base) => {
  switch (water_at(~x, ~z, base)) {
  | River(d) => d
  | _ => 0
  };
};

let ocean_at = (~x, ~z, base) => {
  switch (water_at(~x, ~z, base)) {
  | Ocean => true
  | _ => false
  };
};

let oceanside_at = (~x, ~z, base) => {
  !any_water_at(~x, ~z, base)
  && (
    ocean_at(~x=x - 1, ~z, base)
    || ocean_at(~x, ~z=z - 1, base)
    || ocean_at(~x=x + 1, ~z, base)
    || ocean_at(~x, ~z=z + 1, base)
  );
};

let side = base => Grid.side(base.elevation);

let gray_of_elevation = e => Float.(of_int(e) / 200.);

let gray_at = (~x, ~z, base) =>
  gray_of_elevation(elevation_at(~x, ~z, base));

let color_of_water =
  fun
  | No_water => 0xFFFFFF
  | River(_)
  | Ocean => 0x0000FF;

let color_at = (~x, ~z, base) => {
  let elevation = elevation_at(~x, ~z, base);
  let gray = gray_of_elevation(elevation);
  let color = water_at(~x, ~z, base) |> color_of_water;
  Color.split_rgb(Color.blend(0, color, gray));
};

let apply_progress_view = (state: t) => {
  let (world, _canon) = state;
  let side = side(world);
  let layer = Progress_view.push_layer();
  let draw_dense = ((), x, z) =>
    if (Grid.is_within_side(~x, ~y=z, side)) {
      Some(color_at(~x, ~z, world));
    } else {
      None;
    };
  Progress_view.update(
    ~fit=(0, side, 0, side),
    ~draw_dense,
    ~state=(),
    layer,
  );
  ();
};

let to_obstacles = base => {
  Overlay.Canon.Obstacles.map(
    base.water,
    ~f=
      fun
      | No_water => Clear
      | River(_) => Bridgeable
      | Ocean => Impassable,
  );
};

let extract_canonical = (base: x) =>
  Overlay.Canon.{
    side: side(base),
    elevation: base.elevation,
    obstacles: to_obstacles(base),
    spawn_points: [],
  };

let of_river_state = state => {
  let side = River.side(state);
  let elevation =
    Grid.Int.init(~side, ((x, z)) =>
      River.elevation_at(~x, ~z, state) / Heightmap.precision_coef
    );
  let water =
    Water_grid.init(~side, ((x, z)) =>
      if (River.ocean_at(~x, ~z, state)) {
        Ocean;
      } else if (River.river_at(~x, ~z, state)) {
        River(River.river_depth_at(~x, ~z, state));
      } else {
        No_water;
      }
    );
  {elevation, water};
};

let prepare = () => {
  module Pvh = Progress_view_helper.Make(Grid.Mut.Intf);
  let layer = Progress_view.push_layer();

  let side = Overlay.Canon.require().side;
  let grid = Tectonic.phase(side);
  let grid = Heightmap.phase(grid);
  Pvh.update_with_colorize(
    ~title="height",
    ~colorize=Heightmap.colorize,
    grid,
    layer,
  );
  Progress_view.remove_layer(layer);
  let grid = River.phase(grid);
  assert(River.side(grid) == side);
  let base = of_river_state(grid);
  let canon = extract_canonical(base);
  Overlay.Canon.restore(canon);
  (base, canon);
};

let river_mat = Minecraft.Block.Flowing_water(0);
let apply_region = (state: t, region: Minecraft.Region.t) => {
  let (world, _canon) = state;
  Minecraft_converter.iter_blocks(
    region,
    (~x, ~z) => {
      open Minecraft.Region;

      let elevation = elevation_at(~x, ~z, world);
      let water = water_at(~x, ~z, world);

      set_block(~x, ~y=0, ~z, Minecraft.Block.Bedrock, region);

      for (y in 1 to elevation) {
        set_block(~x, ~y, ~z, Stone, region);
      };
      switch (water) {
      | No_water => ()
      | River(depth) =>
        for (y in elevation - depth + 1 to elevation) {
          set_block(~x, ~y, ~z, river_mat, region);
        }
      | Ocean =>
        for (y in elevation + 1 to Heightmap.sea_level) {
          set_block(~x, ~y, ~z, Minecraft.Block.Water, region);
        }
      };
    },
  );
};

let after_prepare = ((_, canon) as state) => {
  apply_progress_view(state);
  Overlay.Canon.restore(canon);
};

let (require, prepare, apply) =
  Overlay.make_lifecycle(
    ~prepare,
    ~after_prepare,
    ~apply=apply_region,
    overlay,
  );
