type tile = {
  floor_elev: int,
  ceiling_elev: int,
};

let min_dist_to_surface = 10;

let prepare = (world: Grid.t(Base_overlay.tile), ()) => {
  let start_side = 128;
  let floor_cloud =
    Point_cloud.init(
      ~width=start_side, ~height=start_side, ~spacing=32, (_, _) => {
      Random.float(20.) +. 10.
    });
  let ceiling_cloud =
    Point_cloud.init(
      ~width=start_side, ~height=start_side, ~spacing=32, (_, _) => {
      Random.float(10.) +. 40.
    });
  let floor =
    Phase_chain.(
      run_all(
        phase("Init floor", () =>
          Grid.init(start_side, (x, y) => {
            Point_cloud.interpolate(
              floor_cloud,
              float_of_int(x),
              float_of_int(y),
            )
            |> int_of_float
          })
        )
        @> phase_repeat(
             2,
             "Avg subdivide",
             Subdivide.subdivide_with_fill(_, Fill.random_avg),
           )
        @> phase_repeat(
             1,
             "Line and pillar subdivide",
             Subdivide.subdivide_with_fill(
               _,
               Fill.(
                 sometimes(~probability=10, ~value=60, ())
                 **> (line() **> random_avg)
               ),
             ),
           )
        @> phase_repeat(
             2,
             "Line subdivide",
             Subdivide.subdivide_with_fill(_, Fill.(line() **> random_avg)),
           ),
      )
    );
  let ceiling =
    Phase_chain.(
      run_all(
        phase("Init ceiling", () =>
          Grid.init(start_side, (x, y) => {
            Point_cloud.interpolate(
              ceiling_cloud,
              float_of_int(x),
              float_of_int(y),
            )
            |> int_of_float
          })
        )
        @> phase_repeat(
             1,
             "Avg subdivide",
             Subdivide.subdivide_with_fill(_, Fill.random_avg),
           )
        @> phase_repeat(
             4,
             "Rough subdivide",
             Subdivide.subdivide_with_fill(_, Fill.random),
           ),
      )
    );
  (floor, ceiling);
};

let apply_region =
    (world: Grid.t(Base_overlay.tile), (floor, ceiling), args) => {
  let Minecraft_converter.{region, rx: _, rz: _, gx_offset, gy_offset, gsize} = args;
  Minecraft_converter.iter_blocks(
    ~gx_offset,
    ~gy_offset,
    ~gsize,
    (~gx, ~gy, ~x, ~z) => {
      open Minecraft.Block_tree;

      let surface_elev = Grid.at(world, gx, gy).elevation;
      let ceiling_elev = Grid.at(ceiling, gx, gy);
      let floor_elev = Grid.at(floor, gx, gy);

      /* Don't go above the maximum */
      let ceiling_elev =
        min(ceiling_elev, surface_elev - min_dist_to_surface);
      for (y in floor_elev + 1 to pred(ceiling_elev)) {
        set_block(region, x, y, z, Minecraft.Block.Air);
      };
      /* TODO remove */
      set_block(region, x, floor_elev, z, Minecraft.Block.Glowstone);
    },
  );
};

let overlay = world =>
  Overlay.make("cavern", prepare(world), apply_region(world));