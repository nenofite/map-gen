open Core_kernel;
include Constants;

[@deriving (eq, ord, bin_io)]
type water =
  | No_water
  | River(int)
  | Ocean;

[@deriving bin_io]
type x = {
  water: Grid.Mut.t(water),
  elevation: Grid.Mut.t(int),
};

[@deriving bin_io]
type t = (x, Overlay.Canon.t);

let overlay = Overlay.make_overlay("base", bin_reader_t, bin_writer_t);

let elevation_at = (~x, ~z, base) => {
  Grid.Mut.get(~x, ~z, base.elevation);
};

let water_at = (~x, ~z, base) => {
  Grid.Mut.get(~x, ~z, base.water);
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

let side = base => Grid.Mut.side(base.elevation);

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
  Mg_util.Color.blend(0, color, gray);
};

let apply_progress_view = (state: t) => {
  let (world, _canon) = state;
  let side = side(world);
  let layer = Progress_view.push_layer();
  let draw_dense = (x, z) =>
    if (Grid.Griddable.is_within_side(~x, ~z, side)) {
      Some(color_at(~x, ~z, world));
    } else {
      None;
    };
  Progress_view.update(~fit=(0, side, 0, side), ~draw_dense, layer);
  ();
};

let to_obstacles = base => {
  Grid.Mut.map(base.water, ~f=(~x as _, ~z as _, w) =>
    switch (w) {
    | No_water => Overlay.Canon.Clear
    | River(_) => Bridgeable
    | Ocean => Impassable
    }
  );
};

let extract_canonical = (base: x) =>
  Overlay.Canon.{
    side: side(base),
    elevation: Grid.copy(base.elevation),
    obstacles: to_obstacles(base),
    spawn_points: [],
  };

let of_river_state = state => {
  let side = River.side(state);
  let elevation =
    Grid.Mut.init(
      ~side,
      ~f=
        (~x, ~z) =>
          River.elevation_at(~x, ~z, state) / Constants.precision_coef,
      0,
    );
  let water =
    Grid.Mut.init(
      ~side,
      ~f=
        (~x, ~z) =>
          if (River.ocean_at(~x, ~z, state)) {
            Ocean;
          } else if (River.river_at(~x, ~z, state)) {
            River(River.river_depth_at(~x, ~z, state));
          } else {
            No_water;
          },
      Ocean,
    );
  {elevation, water};
};

let erode_flat_slopes = elevation => {
  let t = 2 * precision_coef;
  let elevation = Grid.copy(elevation);
  let side = Grid.side(elevation);

  let sea_level = sea_level * precision_coef;

  Tale.log_progress(1, 50, ~f=_ => {
    for (z in 1 to side - 2) {
      for (x in 1 to side - 2) {
        let height = Grid.get(~x, ~z, elevation);
        if (height > sea_level) {
          let dmax = ref(-1);
          let xmax = ref(0);
          let zmax = ref(0);
          List.iter(
            Grid.four_directions,
            ~f=((dx, dz)) => {
              let hx = x + dx;
              let hz = z + dz;
              let dh = height - Grid.get(~x=hx, ~z=hz, elevation);
              if (dh > dmax^) {
                dmax := dh;
                xmax := hx;
                zmax := hz;
              };
            },
          );
          if (dmax^ > 0 && dmax^ <= t) {
            let height2 = Grid.get(~x=xmax^, ~z=zmax^, elevation);
            let newh = (height + height2) / 2;
            Grid.set(~x, ~z, newh, elevation);
            Grid.set(~x=xmax^, ~z=zmax^, newh, elevation);
          };
        };
      };
    }
  });

  Grid.map_in_place(elevation, ~f=(~x as _, ~z as _, e) =>
    e / precision_coef
  );

  let water =
    Grid.init_exact(~side, ~f=(~x, ~z) =>
      if (Grid.get(~x, ~z, elevation) <= Constants.sea_level) {
        Ocean;
      } else {
        No_water;
      }
    );

  {elevation, water};
};

let prepare = () => {
  let layer = Progress_view.push_layer();
  let side = Overlay.Canon.require().side;
  let grid = Tectonic.phase(side);
  let grid = Heightmap.phase(grid);
  /* TODO restore rivers */
  /* let grid = River.phase(grid);
     assert(River.side(grid) == side);
     let base = of_river_state(grid); */
  Tale.log("About to erode");
  let base = erode_flat_slopes(grid);
  Tale.log("About to draw");
  Progress_view.update(
    ~title="height",
    ~draw_dense=
      (x, z) =>
        if (Grid.Mut.is_within(~x, ~z, base.elevation)) {
          Some(
            Grid.Mut.get(~x, ~z, base.elevation)
            |> Heightmap.colorize_without_coef,
          );
        } else {
          None;
        },
    layer,
  );
  Tale.log("About to save");
  Progress_view.save(~side=Grid.Mut.side(base.elevation), "height");
  Tale.log("Saved");
  let canon = extract_canonical(base);
  Overlay.Canon.restore(canon);
  Progress_view.remove_layer(layer);
  (base, canon);
};

let river_mat = Minecraft.Block.Flowing_water(0);
let apply_region = (state: t, region: Minecraft.Region.t) => {
  let (world, _canon) = state;
  Minecraft.Region.iter_region_xz(
    region,
    ~f=(~x, ~z) => {
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
        for (y in elevation + 1 to Constants.sea_level) {
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