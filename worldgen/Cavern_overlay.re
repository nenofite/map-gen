type tile = {
  floor_elev: int,
  ceiling_elev: int,
};

/*
   floor: 1 - 15
   ceiling: 30 - 40
   pillars: 20
 */

let min_dist_to_surface = 5;
let pillar_meet_elev = 20;
let magma_sea_elev = 3;

let colorize = tile =>
  if (tile.floor_elev >= tile.ceiling_elev) {
    0;
  } else if (tile.floor_elev <= magma_sea_elev) {
    0xFF0000;
  } else {
    let frac = float_of_int(tile.floor_elev) /. 50.;
    Color.(
      blend(color_of_int(0x101010), color_of_int(0xFFFFFF), frac)
      |> int_of_color
    );
  };

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
      ~width=start_side, ~height=start_side, ~spacing=16, (_x, _y) =>
      switch (Random.int(3)) {
      | 0 => 1.
      | 1 => 3. +. Random.float(5.)
      | _ => 60.
      }
    );
  let ceiling_cloud =
    Point_cloud.init(
      ~width=start_side, ~height=start_side, ~spacing=4, (_x, _y) =>
      Random.float(10.) +. 30.
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
            switch (Grid.at(world, x * r, y * r)) {
            | {ocean: true, _} => 60
            | {ocean: false, _} =>
              (
                Point_cloud.interpolate(
                  floor_cloud,
                  float_of_int(x),
                  float_of_int(y),
                )
                |> int_of_float
              )
              + Random.int(2)
            }
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
            switch (Grid.at(world, x * r, y * r)) {
            | {ocean: true, _} => 50
            | {ocean: false, _} =>
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
            }
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
      /* Add magma sea */
      for (y in 1 to magma_sea_elev) {
        switch (get_block(region, x, y, z)) {
        | Air => set_block(region, x, y, z, Lava)
        | _ => ()
        };
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