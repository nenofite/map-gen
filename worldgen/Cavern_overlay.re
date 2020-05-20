type tile = {
  floor_elev: int,
  ceiling_elev: int,
};

let colorize = tile =>
  if (tile.floor_elev < tile.ceiling_elev) {
    let frac = float_of_int(tile.floor_elev) /. 50.;
    Color.(
      blend(color_of_int(0x101010), color_of_int(0xFFFFFF), frac)
      |> int_of_color
    );
  } else {
    0;
  };

/*
   floor: 35 - 45
   ceiling: 50 - 60
   pillars: 50
 */

let min_dist_to_surface = 5;
let pillar_meet_elev = 50;

let add_floor_pillars = (pillar_cloud: Point_cloud.t(bool), floor) => {
  List.fold_left(
    (floor, Point_cloud.{x, y, value}) =>
      if (value) {
        Grid.put(
          floor,
          int_of_float(x),
          int_of_float(y),
          pillar_meet_elev + 5,
        );
      } else {
        floor;
      },
    floor,
    pillar_cloud.points,
  );
};

let add_ceiling_pillars = (pillar_cloud: Point_cloud.t(bool), ceiling) => {
  List.fold_left(
    (ceiling, Point_cloud.{x, y, value}) =>
      if (value) {
        Grid.put(
          ceiling,
          int_of_float(x),
          int_of_float(y),
          pillar_meet_elev - 5,
        );
      } else {
        ceiling;
      },
    ceiling,
    pillar_cloud.points,
  );
};

let prepare = (world: Grid.t(Base_overlay.tile), ()) => {
  let r = 32;
  let start_side = world.side / r;
  let floor_cloud =
    Point_cloud.init(
      ~width=start_side, ~height=start_side, ~spacing=16, (x, y) =>
      switch (Grid.at_w(world, x * r, y * r)) {
      | {ocean: true, _} => 50.
      | {ocean: false, _} =>
        switch (Random.int(3)) {
        | 0 => 30. +. Random.float(5.)
        | 1 => 35. +. Random.float(5.)
        | _ => 60.
        }
      }
    );
  let ceiling_cloud =
    Point_cloud.init(
      ~width=start_side, ~height=start_side, ~spacing=4, (x, y) =>
      switch (Grid.at_w(world, x * r, y * r)) {
      | {ocean: true, _} => 35.
      | {ocean: false, _} => Random.float(10.) +. 50.
      }
    );
  let pillar_cloud =
    Point_cloud.init(
      ~width=start_side * 4, ~height=start_side * 4, ~spacing=8, (_, _) =>
      true
    );
  let floor =
    Phase_chain.(
      run_all(
        phase("Init floor", () =>
          Grid.init(start_side, (x, y) => {
            (
              Point_cloud.interpolate(
                floor_cloud,
                float_of_int(x),
                float_of_int(y),
              )
              |> int_of_float
            )
            - Random.int(2)
          })
        )
        @> phase_repeat(
             2,
             "Random avg subdivide",
             Subdivide.subdivide_with_fill(_, Fill.random_avg),
           )
        @> phase("Add pillars", add_floor_pillars(pillar_cloud))
        @> phase_repeat(
             2,
             "Random avg subdivide",
             Subdivide.subdivide_with_fill(_, Fill.random_avg),
           )
        @> phase_repeat(
             1,
             "Line subdivide",
             Subdivide.subdivide_with_fill(_, Fill.(line() **> avg)),
           ),
      )
    );
  let ceiling =
    Phase_chain.(
      run_all(
        phase("Init ceiling", () =>
          Grid.init(start_side, (x, y) => {
            (
              Point_cloud.interpolate(
                ceiling_cloud,
                float_of_int(x),
                float_of_int(y),
              )
              |> int_of_float
            )
            + Random.int(3)
            - 2
          })
        )
        @> phase_repeat(
             2,
             "Random avg subdivide",
             Subdivide.subdivide_with_fill(_, Fill.random_avg),
           )
        @> phase("Add pillars", add_ceiling_pillars(pillar_cloud))
        @> phase_repeat(
             2,
             "Random avg subdivide",
             Subdivide.subdivide_with_fill(_, Fill.random_avg),
           )
        @> phase_repeat(
             1,
             "Rough subdivide",
             Subdivide.subdivide_with_fill(_, Fill.random),
           ),
      )
    );
  Phase_chain.(
    run_all(
      phase("Combine", () =>
        Grid.zip_map(floor, ceiling, (_x, _y, floor_elev, ceiling_elev) =>
          {floor_elev, ceiling_elev}
        )
      )
      @> Draw.phase("cavern.ppm", colorize),
    )
  );
};

let apply_region = (world: Grid.t(Base_overlay.tile), cavern, args) => {
  let Minecraft_converter.{region, rx: _, rz: _, gx_offset, gy_offset, gsize} = args;
  Minecraft_converter.iter_blocks(
    ~gx_offset,
    ~gy_offset,
    ~gsize,
    (~gx, ~gy, ~x, ~z) => {
      open Minecraft.Block_tree;

      let surface_elev = Grid.at(world, gx, gy).elevation;
      let {floor_elev, ceiling_elev} = Grid.at(cavern, gx, gy);

      /* Don't go above the maximum */
      let ceiling_elev =
        min(ceiling_elev, surface_elev - min_dist_to_surface);
      for (y in floor_elev + 1 to pred(ceiling_elev)) {
        set_block(region, x, y, z, Minecraft.Block.Air);
      };
      /* TODO remove */
      if (floor_elev < ceiling_elev && x mod 8 == 0 && z mod 8 == 0) {
        set_block(region, x, floor_elev, z, Minecraft.Block.Glowstone);
        set_block(region, x, ceiling_elev, z, Minecraft.Block.Glowstone);
      };
    },
  );
};

let overlay = world =>
  Overlay.make("cavern", prepare(world), apply_region(world));