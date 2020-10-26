type ore_layer = {
  min_elev: int,
  max_elev: int,
  ore: option(Minecraft.Block.material),
};

let colorize = tile =>
  switch (tile.ore) {
  | None => 0
  | Some(_) => 0xFFFFFF
  };

type t = Grid.t(list(ore_layer));

let random_ore = () => Minecraft.Block.Iron_ore; /* TODO other ores */

/**
  make_layer creates one layer of ores. For any given x and z, only one type of
  ore can occur. Therefore multiple ore layers should be stacked to create a
  more interesting world, and to make certain ores only occur at certain depths.
 */
let make_layer =
    (
      base: Grid.t(Base_overlay.tile),
      ~name,
      ~max_depth,
      ~max_thickness,
      ~ore_cloud,
    ) => {
  Tale.logf("Creating %s ore layer", name);
  /*
    Create random ore layers, with avg height of 2 blocks and avg depth of 10
    relative to surface
   */
  let r = 8;
  let max_elevs_small =
    Phase_chain.(
      run_all(
        phase("init max elevations", () =>
          Grid.init(
            base.side / r,
            (x, y) => {
              let surface_elev = Grid.at(base, x * r, y * r).elevation;
              let max_elev = surface_elev - Random.int(max_depth + 1);
              max_elev;
            },
          )
        ),
      )
    );
  let max_elevs =
    Phase_chain.(
      run_all(
        phase("start", () => max_elevs_small)
        @> phase_repeat(
             2,
             "random avg subdivide",
             Subdivide.subdivide_with_fill(_, Fill.random_avg),
           )
        @> phase_repeat(
             1,
             "line subdivide",
             Subdivide.subdivide_with_fill(_, Fill.(line() **> random)),
           ),
      )
    );
  let min_elevs =
    Phase_chain.(
      run_all(
        phase("init min elevations", () =>
          Grid.map(max_elevs_small, (_, _, max_elev) =>
            max_elev - Random.int(max_thickness + 1)
          )
        )
        @> phase_repeat(
             2,
             "random avg subdivide",
             Subdivide.subdivide_with_fill(_, Fill.random_avg),
           )
        @> phase_repeat(
             1,
             "line subdivide",
             Subdivide.subdivide_with_fill(_, Fill.(line() **> random)),
           ),
      )
    );

  /* Assign ores based on point cloud. Always a 2/3 chance of being nothing, even when next to an ore. */
  let r = 8;
  let ores =
    Phase_chain.(
      run_all(
        phase("init ores", () =>
          Grid.init(base.side / r, (x, y) =>
            Random.int(100) < 33
              ? Point_cloud.nearest(
                  ore_cloud,
                  float_of_int(x * r),
                  float_of_int(y * r),
                )
              : None
          )
        )
        @> phase_repeat(
             1,
             "random subdivide",
             Subdivide.subdivide_with_fill(_, Fill.random),
           )
        @> phase_repeat(
             2,
             "line subdivide",
             Subdivide.subdivide_with_fill(_, Fill.(line() **> random)),
           ),
      )
    );

  Phase_chain.(
    run_all(
      phase("zipping min and max elevations", () =>
        Grid.zip_map(
          ores,
          Grid.zip(max_elevs, min_elevs),
          (_x, _y, ore, (max_elev, min_elev)) => {
          {max_elev, min_elev, ore}
        })
      )
      @> Draw.phase(name ++ "-ores.png", colorize),
    )
  );
};

let prepare = (base: Grid.t(Base_overlay.tile), ()) => {
  /* Create a point cloud with ore types */
  let iron_cloud =
    Point_cloud.init(
      ~width=base.side, ~height=base.side, ~spacing=128, (_, _) =>
      Random.int(100) < 10 ? Some(random_ore()) : None
    );
  let iron_layer =
    make_layer(
      base,
      ~name="iron",
      ~max_depth=20,
      ~max_thickness=5,
      ~ore_cloud=iron_cloud,
    );
  let diamond_cloud =
    Point_cloud.init(
      ~width=base.side, ~height=base.side, ~spacing=128, (_, _) =>
      Random.int(100) < 10 ? Some(Minecraft.Block.Diamond_ore) : None
    );
  let diamond_layer =
    make_layer(
      base,
      ~name="diamond",
      ~max_depth=40,
      ~max_thickness=3,
      ~ore_cloud=diamond_cloud,
    );
  Grid.multizip([iron_layer, diamond_layer]);
};

let apply_region = (_base, state, args: Minecraft_converter.region_args) => {
  let region = args.region;
  Minecraft_converter.iter_blocks(
    region,
    (~x, ~z) => {
      let layers = Grid.at(state, x, z);
      List.iter(
        Minecraft.Region.(
          layer =>
            switch (layer) {
            | {ore: Some(ore), min_elev, max_elev} =>
              /* TODO catch this during layer generation */
              let min_elev = max(0, min_elev);
              let max_elev = max(0, max_elev);
              for (y in min_elev to max_elev) {
                switch (get_block(~x, ~y, ~z, region)) {
                | Dirt
                | Grass_block
                | Glass /* TODO */
                | Stone => set_block(~x, ~y, ~z, ore, region)
                | _ => ()
                };
              };
            | {ore: None, _} => ()
            }
        ),
        layers,
      );
    },
  );
};

let overlay = base =>
  Overlay.make("ores", prepare(base), apply_region(base));